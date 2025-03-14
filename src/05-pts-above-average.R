## Ryan Elmore
## Pts above/below average

library(dplyr)
library(ggplot2)
library(tidyr)
library(ggridges)
library(forcats)
library(ggrepel)

## Teams
df_tmp <- readRDS("data/nba-zone-combined-team-w-fts.rds") |> 
  tidyr::separate(team_season, into = c("team", "season"), sep = "_") 

df_nba <- readRDS("data/all-shots.rds") 
shots_data_players <- readRDS("data/all-shots-players-w-fts.rds") |> 
  na.omit()

alpha <- 5
dimension <- 20
K <- 7

string <- paste(dimension, "_", dimension, "_", alpha, sep = "")
string
## Read in the w, z, p, and q data
w <- readRDS("data/teams/w_pp_20_20_0.1_09122024.rds")
z <- readRDS("data/teams/z_pp_20_20_0.1_09122024.rds")
p <- readRDS("data/teams/p_20_20_0.1_09122024.rds")
q <- readRDS("data/teams/q_20_20_0.1_09122024.rds")

w_p <- readRDS("data/players/w_pp_20_20_0.1_08122024.rds")
z_p <- readRDS("data/players/z_pp_20_20_0.1_08122024.rds")
p_p <- readRDS("data/players/p_20_20_0.1_08122024.rds")
q_p <- readRDS("data/players/q_20_20_0.1_08122024.rds")

players <- readRDS("data/all-shots-players-w-fts.rds") |> 
  dplyr::rename(player = namePlayer,
                season = yearSeason)

#seasons <- 2010:2021
seasons <- 2021
for (i in seq_along(seasons)){
  
  season_sim <- seasons[i]
  
  cat(sprintf("\n\n Season %s at %s \n\n", season_sim, Sys.time()))
  
  df_teams <- df_tmp |> 
    dplyr::filter(season == season_sim) |> 
    dplyr::select(team, season, tidyr::ends_with("_n")) |> 
    dplyr::mutate(team_shots = rowSums(across(where(is.numeric)))) |> 
    dplyr::select(team, season, team_shots) |> 
    dplyr::mutate(season = as.numeric(season))
  
  players_sub <- players |> 
    dplyr::ungroup() |> 
    dplyr::filter(season == season_sim) |> 
    dplyr::select(player, season, n, ft_n) |> 
    dplyr::mutate(player_shots = n + ft_n)
  
  df_player_team <- df_nba |> 
    dplyr::select(yearSeason, namePlayer, nameTeam) |> 
    dplyr::rename(season = yearSeason, 
                  player = namePlayer,
                  team = nameTeam) |> 
    dplyr::filter(season == season_sim) |> 
    dplyr::distinct()
  
  df <- dplyr::left_join(players_sub, df_player_team) |> 
    dplyr::left_join(df_teams) |> 
    dplyr::mutate(prop_shots = player_shots/team_shots) |> 
    dplyr::group_by(player, season) |> 
    dplyr::summarize(prop = max(prop_shots)) |> 
    dplyr::mutate(player_shots = round(8000*prop), 
                  team_shots = round(8000*(1-prop))) |> 
    dplyr::ungroup()
  
  # saveRDS(df, paste("data/all-avg-pts-above-data-10-10-0.1-", season_sim, ".rds", sep = ""))
  
  df <- df |> #readRDS(paste("data/all-avg-pts-above-data-10-10-0.1-", season_sim, ".rds", sep = "")) |> 
    dplyr::filter(player != "Enes Kanter")
  
  teams_data <- df_tmp |> 
    dplyr::filter(season == season_sim) |> 
    dplyr::distinct(team) |> 
    dplyr::pull()
  
  #i <- 1
  
  B <- 10000
  set.seed(19892)
  rm(results)
  for(i in seq_along(df$player)){
    # i <- 10
    cat(sprintf(" Player %s at %s \n", df$player[i], Sys.time()))
    player <- df$player[i]
    N_team <- df$player_shots[i] #shots
    N_player <- df$player_shots[i] #shots
    
    total_team <- numeric(B)
    total_player <- numeric(B)
    
    #  team_index <- which(df_tmp$team == sample(teams_data, size = 1) & df_tmp$season == 2021)
    player_index <- which(shots_data_players$namePlayer == df$player[i] & 
                            shots_data_players$yearSeason == season_sim)
    for (b in 1:B) {
      #b <- 5000
      team_index <- which(df_tmp$team == sample(teams_data, size = 1) & 
                            df_tmp$season == season_sim)
      if(b %% 5000 == 0){
        cat(sprintf("  - Iteration %s at %s \n", b, Sys.time()))
      }
      Z_pp <- z[[b]][team_index, ]
      W_pp <- w[[b]][team_index, ]
      z_i <- which(rmultinom(1, 1, Z_pp) == 1)
      w_i <- which(rmultinom(1, 1, W_pp) == 1)
      
      Z_pp_p <- z_p[[b]][player_index, ]
      W_pp_p <- w_p[[b]][player_index, ]
      z_i_p <- which(rmultinom(1, 1, Z_pp_p) == 1)
      w_i_p <- which(rmultinom(1, 1, W_pp_p) == 1)
      
      ## Use w_i to sample from multinomial 
      n_i <- as.vector(rmultinom(1, N_team, unlist(q[[b]][w_i])))
      n_i_p <- as.vector(rmultinom(1, N_player, unlist(q_p[[b]][w_i_p])))
      
      m_i <- rep(NA, len = K)
      m_i_p <- rep(NA, len = K)
      for(k in 1:K){
        m_i[k] <- rbinom(1, n_i[k], p[[b]][z_i][[1]][k])
        m_i_p[k] <- rbinom(1, n_i_p[k], p_p[[b]][z_i_p][[1]][k])
      }
      total_team[b] <- sum(m_i * c(3, 2, 3, 2, 2, 3, 1))
      total_player[b] <- sum(m_i_p * c(3, 2, 3, 2, 2, 3, 1))
      
    }
    
    tmp_df <- tibble(team_points = total_team, 
                     player_points = total_player,
                     player = player)
    if(exists("results")){
      results <- rbind(results, tmp_df)
    } else results <- tmp_df
    
  }
  
  saveRDS(results, paste("data/all-pts-above-avg-20-20-0.1-", season_sim, ".rds", sep = ""))
  
}

df <- dplyr::bind_rows(readRDS("data/all-pts-above-avg-20-20-5-2010.rds") |>
                         dplyr::mutate(season = 2010),
                       readRDS("data/all-pts-above-avg-20-20-5-2011.rds") |>
                         dplyr::mutate(season = 2011),
                       readRDS("data/all-pts-above-avg-20-20-5-2012.rds") |>
                         dplyr::mutate(season = 2012),
                       readRDS("data/all-pts-above-avg-20-20-5-2013.rds") |>
                         dplyr::mutate(season = 2013),
                       readRDS("data/all-pts-above-avg-20-20-5-2014.rds") |>
                         dplyr::mutate(season = 2014),
                       readRDS("data/all-pts-above-avg-20-20-5-2015.rds") |>
                         dplyr::mutate(season = 2015),
                       readRDS("data/all-pts-above-avg-20-20-5-2016.rds") |>
                         dplyr::mutate(season = 2016),
                       readRDS("data/all-pts-above-avg-20-20-5-2017.rds") |>
                         dplyr::mutate(season = 2017),
                       readRDS("data/all-pts-above-avg-20-20-5-2018.rds") |>
                         dplyr::mutate(season = 2018),
                       readRDS("data/all-pts-above-avg-20-20-5-2019.rds") |>
                         dplyr::mutate(season = 2019),
                       readRDS("data/all-pts-above-avg-20-20-5-2020.rds") |>
                         dplyr::mutate(season = 2020),
                       readRDS("data/all-pts-above-avg-20-20-5-2021.rds") |>
                         dplyr::mutate(season = 2021))
saveRDS(df, "data/all-pts-above-avg-20-20-5-10-21.rds")

results <- readRDS(paste("data/all-pts-above-avg-20-20-5-", season_sim, ".rds", sep = ""))
df <- results |> 
  dplyr::mutate(points = player_points - team_points,
                sample = rep(1:10000, times = 100)) |> 
  dplyr::filter(sample >= 3001)

p <- ggplot(data = df %>% 
                   dplyr::mutate(., player = fct_reorder(player, points, 
                                                       .fun = 'mean')),
                 aes(x = points/72, y = player, fill = player))#, group = player, fill = player))
p + stat_density_ridges(scale = 1.1, quantiles = .5, quantile_lines = F) +
  scale_fill_viridis_d(option = "H", alpha = .75) +
  scale_y_discrete(expand = c(0.015, 0)) +
  guides(fill = "none") +
  labs(y = "", 
       x = "expected points per game") +
  theme_light()

players <- c("Giannis Antetokounmpo", "Stephen Curry","Luka Doncic", 
             "Nikola Jokic", "Kawhi Leonard","Joel Embiid","LeBron James",
             "Damian Lillard","Chris Paul", "Bradley Beal", "Jimmy Butler",
             "Paul George", "Rudy Gobert", "Kyrie Irving", "Anthony Davis", 
             "James Harden", "Russell Westbrook", "Devin Booker", "Jayson Tatum",
             "Julius Randle", "Donovan Mitchell", "Trae Young")
p <- ggplot(data = df |> 
              dplyr::filter(player %in% players) |> 
              dplyr::mutate(player = fct_reorder(player, points, 
                                                    .fun = 'mean')),
            aes(x = points/72, y = player))#, group = player, fill = player))
p + stat_density_ridges(scale = 1.1, quantiles = .5, quantile_lines = F) +
#  scale_fill_viridis_d(option = "H", alpha = .75) +
  scale_y_discrete(expand = c(0, 0)) +
  scale_x_continuous(breaks = seq(-8, 8, by = 2), limits = c(-8, 8)) +
  geom_vline(xintercept = 0, linetype = 3) +
  guides(fill = "none") +
  labs(y = "", 
       x = "expected points per game above/below average") +
  theme_light()

p <- ggplot(data = df |> 
              dplyr::filter(player %in% players) |> 
              dplyr::mutate(player = fct_reorder(player, -points, 
                                                 .fun = 'mean')),
            aes(x = points/72, y = player))#, group = player, fill = player))
p + geom_boxplot() +
  scale_x_continuous(breaks = seq(-8, 9, by = 2), limits = c(-8, 9)) +
  geom_vline(xintercept = 0, linetype = 3) +
  labs(y = "", 
       x = "expected points per game above average") +
  coord_flip() +
  theme_light() +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))

df_grouped <- df |> 
  dplyr::filter(player %in% players) |>
  dplyr::group_by(player) |> 
  dplyr::summarize(y0 = quantile(points, 0.05)/72,
                   y1 = quantile(points, 0.2)/72,
                   y2 = quantile(points, 0.5)/72,
                   y3 = quantile(points, 0.8)/72,
                   y4 = quantile(points, 0.95)/72) |> 
  dplyr::ungroup() |> 
  dplyr::arrange(desc(y2))
p <- ggplot(data = df_grouped |> 
              dplyr::mutate(player = fct_reorder(player, -y2)),
            aes(x = player, ymin = y0, lower = y1, middle = y2, upper = y3,
                ymax = y4))#, group = player, fill = player))
p + geom_boxplot(stat = "identity") +
  geom_hline(yintercept = 0, linetype = 3) +
  labs(x = "", 
       y = "expected points per game above average") +
  theme_light() +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))

results <- readRDS(paste("data/all-pts-above-avg-20-20-5-", season_sim, ".rds", sep = ""))
df <- results |> 
  dplyr::mutate(points = player_points - team_points,
                sample = rep(1:10000, times = 100)) |> 
  dplyr::filter(sample >= 3001)

df_props <- readRDS("data/all-avg-pts-above-data-20-20-5-2021.rds") 
df_agg <- df |> 
  dplyr::group_by(player) |> 
  dplyr::summarize(m = mean(points)/72,
                   `standard deviation:` = sd(points/72))

df_comb <- dplyr::left_join(df_props, df_agg) |> 
  dplyr::mutate(new_player = ifelse(m <= -1 | m > .9 | prop > .22, player, ""))

p <- ggplot(df_comb, aes(y = m, x = prop, label = new_player))
p + geom_point(aes(size = `standard deviation:`)) +
  scale_y_continuous(breaks = seq(-3, 4, by = 1), limits = c(-3, 4)) +
  scale_x_continuous(breaks = seq(.06, .32, by = .02)) +
  geom_text_repel() +
  geom_hline(yintercept = 0, linetype = 3) +
  labs(x = "proportion of team shots by player", 
       y = "mean points above/below average per game") +
  theme_light() +
  theme(legend.position = "bottom")

ggsave("fig/fg_pts_above_avg_by_prop_shots_revision_2.png", hei = 8, wid = 12)


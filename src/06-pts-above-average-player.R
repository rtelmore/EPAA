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
dimension <- 10
K <- 7

string <- paste(dimension, "_", dimension, "_", alpha, sep = "")

## Read in the w, z, p, and q data
w <- readRDS("data/teams/w_pp_10_10_5_22102023.rds")
z <- readRDS("data/teams/z_pp_10_10_5_22102023.rds")
p <- readRDS("data/teams/p_10_10_5_22102023.rds")
q <- readRDS("data/teams/q_10_10_5_22102023.rds")

w_p <- readRDS("data/players/w_pp_10_10_5_23102023.rds")
z_p <- readRDS("data/players/z_pp_10_10_5_23102023.rds")
p_p <- readRDS("data/players/p_10_10_5_23102023.rds")
q_p <- readRDS("data/players/q_10_10_5_23102023.rds")

season_sim = 2021

df_teams <- df_tmp |> 
  dplyr::filter(season == season_sim) |> 
  dplyr::select(team, season, tidyr::ends_with("_n")) |> 
  dplyr::mutate(team_shots = rowSums(across(where(is.numeric)))) |> 
  dplyr::select(team, season, team_shots) |> 
  dplyr::mutate(season = as.numeric(season))

players <- readRDS("data/all-shots-players-w-fts.rds") |> 
  dplyr::rename(player = namePlayer,
                season = yearSeason)

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

saveRDS(df, paste("data/all-avg-pts-above-data-", season_sim, ".rds", sep = ""))

df <- readRDS(paste("data/all-avg-pts-above-data-", season_sim, ".rds", sep = "")) |> 
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
  cat(sprintf("Player %s at %s \n", df$player[i], Sys.time()))
  player <- df$player[i]
  N_team <- df$player_shots[i] #shots
  N_player <- df$player_shots[i] #shots
  
  total_team <- numeric(B)
  total_player <- numeric(B)
  
  player_index <- which(shots_data_players$namePlayer == df$player[i] & 
                          shots_data_players$yearSeason == season_sim)
  for (b in 1:B) {
    team_index <- which(df_tmp$team == sample(teams_data, size = 1) & 
                          df_tmp$season == season_sim)
    if(b %% 5000 == 0){
      cat(sprintf(" - Iteration %s at %s \n", b, Sys.time()))
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

saveRDS(results, paste("data/all-pts-above-avg-", season_sim, ".rds", sep = ""))

season_sim <- 2021
results <- readRDS(paste("data/all-pts-above-avg-", season_sim, ".rds", sep = ""))
df <- results |> 
  dplyr::mutate(points = player_points - team_points,
                sample = rep(1:10000, times = 100)) |> 
  dplyr::filter(sample >= 3001)

results <- data.frame(player= unique(df$player), 
                      ESS = rep(NA, 100))
for(i in 1:100){
  tmp <- df |> 
    filter(player == results$player[i]) |> 
    pull(points)
#  print(length(tmp))
  results$ESS[i] <- coda::effectiveSize(tmp)
}

results$ESS[results$ESS > 7000] <- 7000
p <- ggplot(data = results,
            aes(x = ESS))
p + geom_histogram() +
  scale_x_continuous(breaks = seq(500, 8000, by = 500)) +
  scale_y_continuous(breaks = seq(0, 100, by = 10)) +
  labs(x = "Effective Sample Size") +
  theme_bw()

ggsave("fig/players/ess-top-100.png", hei = 6, width = 8)
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
            aes(x = points/72, y = player, fill = player))
p + stat_density_ridges(scale = 1.1, quantiles = .5, quantile_lines = F) +
  scale_fill_viridis_d(option = "H", alpha = .75) +
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
  scale_x_continuous(breaks = seq(-8, 8, by = 2), limits = c(-8, 8)) +
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

results <- readRDS("data/all-pts-above-avg-2021.rds")
df <- results |> 
  dplyr::mutate(points = player_points - team_points,
                sample = rep(1:10000, times = 100)) |> 
  dplyr::filter(sample >= 3001)

df_props <- readRDS("data/all-avg-pts-above-data-2021.rds") 
df_agg <- df |> 
  dplyr::group_by(player) |> 
  dplyr::summarize(m = mean(points)/72)

df_comb <- dplyr::left_join(df_props, df_agg) |> 
  dplyr::mutate(new_player = ifelse(m <= -1 | m > .9 | prop > .22, player, ""))

p <- ggplot(df_comb, aes(y = m, x = prop, label = new_player))
p + geom_point() +
  scale_y_continuous(breaks = seq(-4, 2, by = 1), limits = c(-4, 2.1)) +
  scale_x_continuous(breaks = seq(.06, .32, by = .02)) +
  geom_text_repel(min.segment.length = 0) +
  geom_hline(yintercept = 0, linetype = 3) +
  labs(x = "proportion of team shots by player", 
       y = "mean points above/below average per game") +
  theme_bw()

## Steph Curry, Russell Westbrook, Chris Paul, and Damian Lillard
df <- readRDS("data/all-pts-above-avg-10-21.rds") |> 
  dplyr::filter(player %in% c("Stephen Curry", "Russell Westbrook", "James Harden",
                              "Damian Lillard")) |> 
  dplyr::mutate(points = player_points - team_points,
              sample = rep(1:10000, times = 41)) |> 
  dplyr::filter(sample >= 3001, 
                season >= 2012)

p_box <- ggplot(data = df,
                aes(x = season, y = points/82, group = season, fill = player))
p_box + geom_hline(yintercept = 0, linetype = "dashed") +
  geom_boxplot() +
  facet_wrap(~ player, nc = 2) +
  labs(x = "season", y = "Expected Points Above (Below) Average per Game") +
  scale_fill_brewer("", palette = "Dark2") +
  scale_x_continuous(breaks = 2010:2021) +
  guides(fill = "none") +
  theme_bw()

## Ranked pts above avg
df <- readRDS("data/all-pts-above-avg-10-21.rds") |> 
  dplyr::mutate(points = player_points - team_points,
                sample = rep(1:10000, times = 1195)) |> 
  dplyr::filter(sample >= 3001)

df_ranks <- df |> 
  dplyr::group_by(player, season) |> 
  dplyr::summarize(avg = mean(points)/72, med = median(points)/72) |> 
  dplyr::ungroup() |> 
  dplyr::group_by(season) |> 
  dplyr::mutate(rank_mean = rank(-avg), rank_med = rank(-med))

players <- c("Stephen Curry", "Russell Westbrook", "James Harden",
             "Chris Paul", "Dirk Nowitzki", "Anthony Davis","Kawhi Leonard",
             "LeBron James", "Kevin Durant", "Dwyane Wade")

p_ranks <- ggplot(data = df_ranks |> 
                    dplyr::filter(player %in% players),
                aes(x = season, y = rank_med, color = player))
p_ranks + geom_point() +
  geom_line() +
  labs(x = "Season", y = "Rank of Median EPAA per Game") +
  scale_color_brewer("", palette = "Paired") +
  scale_x_continuous(breaks = 2010:2021, minor_breaks = NULL) +
  scale_y_reverse(breaks = seq(0, 100, by = 5)) +
  theme_bw()

ggsave("fig/epaa-player-ranks.png", hei = 8, wid = 11.5)

## Ryan Elmore
## Team rankings for 2021 (10 x 10)

library(dplyr)
library(ggplot2)
library(tidyr)
library(ggridges)
library(forcats)
library(coda)


source("src/99-prediction-functions.R")

df <- readRDS("data/nba-zone-combined-team-w-fts.rds") %>% 
  tidyr::separate(., team_season, into = c("team", "season"), sep = "_") 

## Some descriptive stats/plots
## Average team by year
df_avg_team_by_year <- df %>%
  dplyr::select(., team, season, tidyr::ends_with(c("_n", "_m"))) %>%
  dplyr::select(., -team) %>%
  dplyr::group_by(., season) %>%
  dplyr::summarize_all(., c(mean)) %>%
  dplyr::mutate(., atb_3_p = atb_3_m/atb_3_n, itp_p = itp_m/itp_n,
                lc_3_p = lc_3_m/lc_3_n, mid_p = mid_m/mid_n, ra_p = ra_m/ra_n,
                rc_3_p = rc_3_m/rc_3_n, ft_p = ft_m/ft_n)

shots <- df_avg_team_by_year %>%
  dplyr::filter(., season == 2021) %>%
  dplyr::select(., dplyr::ends_with("_n")) %>%
  round()
apply(df_avg_team_by_year[, -1] |> 
        dplyr::select(dplyr::ends_with("_n")) , 1, sum)

teams <- df |> 
  dplyr::filter(season == 2021) |> 
  dplyr::distinct(team) |> 
  dplyr::pull()
teams <- c(teams, "Average")

set.seed(19892)
alpha <- 5
dimension_1 <- 20
dimension_2 <- 20
string <- paste(dimension_1, "_", dimension_2, "_", alpha, sep = "")
#string
w <- readRDS(paste("data/teams/w_pp_", string, "_09122024.rds", sep = ""))
z <- readRDS(paste("data/teams/z_pp_", string, "_09122024.rds", sep = ""))
p <- readRDS(paste("data/teams/p_", string, "_09122024.rds", sep = ""))
q <- readRDS(paste("data/teams/q_", string, "_09122024.rds", sep = ""))

N <- 8000 #shots
K <- 7
rm(results)
for(i in seq_along(teams)){
  cat(sprintf("Team %s (%s) at %s \n", teams[i], i, Sys.time()))
  
  index <- which(df$team == teams[i] & df$season == 2021)
  
  B <- 10000
  total <- numeric(B)
  
  for (b in 1:B) {
    if (teams[i] == "Average"){
      index <- which(df$team == sample(teams[1:30], size = 1) & df$season == 2021)
    }
    
    if(b %% 5000 == 0){
      cat(sprintf("Iteration %s at %s \n", b, Sys.time()))
    }
    Z_pp <- z[[b]][index, ]
    W_pp <- w[[b]][index, ]
    z_i <- which(rmultinom(1, 1, Z_pp) == 1)
    w_i <- which(rmultinom(1, 1, W_pp) == 1)

    ## Use w_i to sample from multinomial 
    n_i <- as.vector(rmultinom(1, N, unlist(q[[b]][w_i])))
    
    m_i <- rep(NA, len = K)
    for(k in 1:K){
      m_i[k] <- rbinom(1, n_i[k], p[[b]][z_i][[1]][k])
    }
    total[b] <- sum(m_i * c(3, 2, 3, 2, 2, 3, 1))
    
  }
  tmp_df <- tibble(points = total, 
                   team = teams[i])
  if(exists("results")){
    results <- rbind(results, tmp_df)
  } else results <- tmp_df
  
}
  
# new_string <- paste(dimension, "_", dimension, "_", alpha, sep = "")
# saveRDS(results, paste("data/avg_team/avg_fg_pts_team_8K_", new_string, "_", 2021, 
#                        "_", format(Sys.Date(), "%d%m%Y"), ".rds", sep = ""))

results <- results |> 
  dplyr::mutate(iter = rep(1:10000, times = 31)) |> 
  dplyr::filter(iter > 3000, team != "Average")

results_10_30_0.1 <- results
results_10_30_5 <- results
results_30_10_0.1 <- results
results_30_10_5 <- results
results_20_20_0.1 <- results
results_20_20_5 <- results
results_30_30_5 <- results
results_30_30_0.1 <- results
results_10_10_5 <- results
results_10_10_0.1 <- results

df <- bind_rows(results_10_10_0.1 |> 
                  dplyr::mutate(alpha = 0.1, dim1 = 10, dim2 = 10),
                results_30_30_0.1 |> 
                  dplyr::mutate(alpha = 0.1, dim1 = 30, dim2 = 30),
                results_10_10_5 |> 
                  dplyr::mutate(alpha = 5, dim1 = 10, dim2 = 10),
                results_30_30_5 |> 
                  dplyr::mutate(alpha = 5, dim1 = 30, dim2 = 30),
                results_20_20_5 |> 
                  dplyr::mutate(alpha = 5, dim1 = 20, dim2 = 20),
                results_20_20_0.1 |> 
                  dplyr::mutate(alpha = 0.1, dim1 = 20, dim2 = 20),
                results_10_30_5 |> 
                  dplyr::mutate(alpha = 5, dim1 = 10, dim2 = 30),
                results_30_10_5 |> 
                  dplyr::mutate(alpha = 5, dim1 = 30, dim2 = 10),
                results_10_30_0.1 |> 
                  dplyr::mutate(alpha = 0.1, dim1 = 10, dim2 = 30),
                results_30_10_0.1 |> 
                  dplyr::mutate(alpha = 0.1, dim1 = 30, dim2 = 10))
saveRDS(df, "data/teams/epaa-all.rds")

df <- readRDS("data/teams/epaa-all.rds") |> 
  filter(dim1 == 20, dim2 == 20, alpha == 5, team != "Average") |> 
  mutate(iter = rep(1:10000, times = 21)) |> 
  filter(iter >= 3001)

p_comp <- ggplot(data = df |> 
                   dplyr::mutate(team = fct_reorder(team, points, 
                                                    .fun = 'mean')),
                 aes(x = points/72, y = team))#, group = player, fill = player))
#  scale_fill_viridis_d(option = "H", alpha = .75) +

p_comp + stat_density_ridges(scale = 1.1, quantiles = .5, quantile_lines = F) +
  scale_y_discrete(expand = c(0.036, 0)) +
  scale_x_continuous(breaks = seq(100, 125, by = 5), limits = c(98, 127)) +
  guides(fill = "none") +
  labs(y = "", 
       x = "expected points per game") +
  theme_light()
ggsave("fig/epaa/epaa-20-20-5.png", height = 11, width = 8.5)

## ESS Calculation
results <- data.frame(team = unique(df$team), 
                      ESS = rep(NA, 30))
for(i in 1:30){
  tmp <- df |> 
    filter(team == results$team[i]) |> 
    pull(points)
  results$ESS[i] <- coda::effectiveSize(tmp)
}
summary(results$ESS)
## supplement figure
df <- readRDS("data/teams/epaa-all.rds")
df <- df |> 
  mutate(dim = paste0(dim1, ", ", dim2))
p <- ggplot(data = df |> 
              filter(team != "Average", 
                     dim1 == 30) |> 
              mutate(team = fct_reorder(team, points, .fun = 'mean'),
                     cat = paste("(", paste(dim, alpha, sep = ", "), ")", sep = "")),
            aes(x = team, 
                y = points/72,
                fill = cat))

p + geom_boxplot(position = position_dodge(width = .7),
                 outlier.shape = NA) +
  coord_flip() +
  labs(x = "", 
       y = "expected points per game") +
  scale_y_continuous(breaks = seq(98, 128, by = 2)) +
  scale_fill_brewer(palette = "Set2") +
  guides(fill = guide_legend(title = "(J, L, alpha):")) +
  theme_bw() +
  theme(legend.position = "bottom")
ggsave("fig/epaa/epaa-all-30-accuracy.png", height = 11, width = 8.5)


p <- ggplot(data = df_group |> 
              mutate(team = fct_reorder(team, mean, .fun = 'mean'),
                     cat = paste("(", paste(dim, alpha, sep = ", "), ")", sep = "")),
            aes(x = team, 
                y = mean/72, 
                fill = cat))
p + geom_dotplot(binaxis = "y", alpha = .75) +
  coord_flip() +
  labs(x = "", 
       y = "mean expected points per game") +
  scale_y_continuous(breaks = seq(104, 120, by = 2)) +
  scale_fill_brewer(palette = "Dark2") +
  guides(fill = guide_legend(title = "(clusters, alpha):")) +
  theme_light() +
  theme(legend.position = "bottom")

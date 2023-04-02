## Ryan Elmore
## Player Clustering for all teams
## 31 October 31

library(dplyr)
library(ggplot2)
library(tidyr)

players <- readRDS("data/all-shots-players-w-fts.rds") |> 
  na.omit() |> 
  dplyr::rename(player = namePlayer,
                season = yearSeason)

z_pp_df <- readRDS("data/players/z_pp_10_10_5_02112022.rds")
w_pp_df <- readRDS("data/players/w_pp_10_10_5_02112022.rds")

# z_pp_df <- readRDS("data/teams_a5_5_03Nov21/z_pp_5_5_20211102.rds")
# w_pp_df <- readRDS("data/teams_a5_5_03Nov21/w_pp_5_5_20211102.rds")

## Look at select players
ind_players <- c("Chris Paul", "LeBron James", "James Harden", "Kevin Durant",
                 "Russell Westbrook", "Stephen Curry", "Nikola Jokic", 
                 "Giannis Antetokounmpo")
players_sub <- players |> 
  dplyr::filter(player %in% ind_players,
                season >= 2010)

set.seed(109823)
B <- 10000
seasons <- unique(players$season)
for(s in seq_along(seasons)){
  #s <- 13
  rm(results)
  ind <- which(players$season == seasons[s] & players$player %in% ind_players)
  cat(sprintf("Season %s at %s \n", seasons[s], Sys.time()))
  for(b in 1:B){
    if(b %% 500 == 0){
      cat(sprintf(" - Iteration %s at %s \n", b, Sys.time()))
    }
    for(i in seq_along(ind)){
      index <- ind[i]
      z_pp <- z_pp_df[[b]][index, ]
      w_pp <- w_pp_df[[b]][index, ]
      tmp <- tibble(post_sample = b,
                    team = players$player[index],
                    season = players$season[index],
                    z_cluster = which(as.vector(rmultinom(1, 1, prob = z_pp)) == 1),
                    w_cluster = which(as.vector(rmultinom(1, 1, prob = w_pp)) == 1))
      if(exists("results")){
        results <- dplyr::bind_rows(results, tmp)
      } else results <- tmp
    }
  }
  saveRDS(results, paste("data/players/posterior-clusters-10-10-5-", seasons[s], ".rds", sep = ""))
}

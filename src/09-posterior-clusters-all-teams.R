## Ryan Elmore
## Team Clustering for all teams

library(dplyr)
library(ggplot2)
library(tidyr)

teams <- readRDS("data/nba-zone-combined-team-w-fts.rds") %>% 
  tidyr::separate(., team_season, into = c("team", "season"), sep = "_")

z_pp_df <- readRDS("data/teams/z_pp_30_30_5_21102023.rds")
w_pp_df <- readRDS("data/teams/w_pp_30_30_5_21102023.rds")


set.seed(109823)
B <- 10000
seasons <- unique(teams$season)
for(s in seq_along(seasons)){
  rm(results)
  ind <- which(teams$season == seasons[s])
  cat(sprintf("Season %s at %s \n", seasons[s], Sys.time()))
  for(b in 1:B){
    if(b %% 500 == 0){
      cat(sprintf("Iteration %s at %s \n", b, Sys.time()))
    }
    for(i in seq_along(ind)){
      index <- ind[i]
      z_pp <- z_pp_df[[b]][index, ]
      w_pp <- w_pp_df[[b]][index, ]
      tmp <- tibble(post_sample = b,
                    team = teams$team[index],
                    season = teams$season[index],
                    z_cluster = which(as.vector(rmultinom(1, 1, prob = z_pp)) == 1),
                    w_cluster = which(as.vector(rmultinom(1, 1, prob = w_pp)) == 1))
      if(exists("results")){
        results <- dplyr::bind_rows(results, tmp)
      } else results <- tmp
    }
  }
  saveRDS(results, paste("data/teams/posterior-clusters-30-30-5-", seasons[s], ".rds", sep = ""))
}

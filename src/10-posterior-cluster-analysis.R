## Ryan Elmore
## Team Clustering Analysis

library(dplyr)
library(ggplot2)
library(tidyr)

dimension <- 30
alpha <- 1

## The following file path is given in 09-poster-clusters-all-teams.R

tt <- readRDS(paste("data/teams/posterior-clusters-",
                     dimension, "-", dimension, "-", alpha, 
                     "-2009.rds", sep = ""))

make_cluster_plots <- function(dimension = 30, alpha = 1){
  
  df <- dplyr::bind_rows(readRDS(paste("data/teams/posterior-clusters-",
                                       dimension, "-", dimension, "-", alpha, 
                                       "-2009.rds", sep = "")),
                         readRDS(paste("data/teams/posterior-clusters-",
                                       dimension, "-", dimension, "-", alpha, 
                                       "-2010.rds", sep = "")),
                         readRDS(paste("data/teams/posterior-clusters-",
                                       dimension, "-", dimension, "-", alpha, 
                                       "-2011.rds", sep = "")),
                         readRDS(paste("data/teams/posterior-clusters-",
                                       dimension, "-", dimension, "-", alpha, 
                                       "-2012.rds", sep = "")),
                         readRDS(paste("data/teams/posterior-clusters-",
                                       dimension, "-", dimension, "-", alpha, 
                                       "-2013.rds", sep = "")),
                         readRDS(paste("data/teams/posterior-clusters-",
                                       dimension, "-", dimension, "-", alpha, 
                                       "-2014.rds", sep = "")),
                         readRDS(paste("data/teams/posterior-clusters-",
                                       dimension, "-", dimension, "-", alpha, 
                                       "-2015.rds", sep = "")),
                         readRDS(paste("data/teams/posterior-clusters-",
                                       dimension, "-", dimension, "-", alpha, 
                                       "-2016.rds", sep = "")),
                         readRDS(paste("data/teams/posterior-clusters-",
                                       dimension, "-", dimension, "-", alpha, 
                                       "-2017.rds", sep = "")),
                         readRDS(paste("data/teams/posterior-clusters-",
                                       dimension, "-", dimension, "-", alpha, 
                                       "-2018.rds", sep = "")),
                         readRDS(paste("data/teams/posterior-clusters-",
                                       dimension, "-", dimension, "-", alpha, 
                                       "-2019.rds", sep = "")),
                         readRDS(paste("data/teams/posterior-clusters-",
                                       dimension, "-", dimension, "-", alpha, 
                                       "-2020.rds", sep = "")),
                         readRDS(paste("data/teams/posterior-clusters-",
                                       dimension, "-", dimension, "-", alpha, 
                                       "-2021.rds", sep = ""))) %>% 
    dplyr::filter(., post_sample >= 3001)
  
  df$team[df$team == "LA Clippers"] <- "Los Angeles Clippers"
  df$team[df$team == "Charlotte Bobcats"] <- "Charlotte Hornets"
  df$team[df$team == "New Jersey Nets"] <- "Brooklyn Nets"
  df$team[df$team == "New Orleans Hornets"] <- "New Orleans Pelicans"
  
  tmp <- df %>% 
    dplyr::group_by(., team, season, w_cluster) %>% 
    dplyr::summarize(., n = n())
  
  p <- ggplot(data = tmp %>% dplyr::filter(season >= 2010),
              aes(x = team, y = n, fill = as.factor(w_cluster)))
  p + geom_bar(position = "stack", stat = "identity") +
#    scale_fill_brewer("cluster", palette = "Paired") +
    scale_x_discrete("") +
    facet_wrap(. ~ season) +
    theme_bw() +
    coord_flip() +
    labs(y = "number of samples in each cluster")
  
  ggsave(paste("fig/teams/posterior-shot-selection-", dimension, "-", 
               dimension, "-", alpha, "-all-teams.png", sep = ""), 
         hei = 12, wid = 9)
  
  tmp <- df %>% 
    dplyr::group_by(., team, season, z_cluster) %>% 
    dplyr::summarize(., n = n())
  
  p <- ggplot(data = tmp %>% dplyr::filter(season >= 2010),
              aes(x = team, y = n, fill = as.factor(z_cluster)))
  p + geom_bar(position = "stack", stat = "identity") +
#    scale_fill_brewer("cluster", palette = "Paired") +
    scale_x_discrete("") +
    facet_wrap(. ~ season) +
    theme_bw() +
    coord_flip() +
    labs(y = "number of samples in each cluster")
  ggsave(paste("fig/teams/posterior-shot-making-", dimension, "-",
               dimension, "-", alpha, "-all-teams.png", sep = ""),
               hei = 12, wid = 9)
}

make_cluster_plots(dimension = 30, alpha = 1)
make_cluster_plots(dimension = 5, alpha = 5)
make_cluster_plots(dimension = 10, alpha = 1)
make_cluster_plots(dimension = 10, alpha = 5)

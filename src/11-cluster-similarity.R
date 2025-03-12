## Ryan Elmore
## 06 Mar 2025
## MDS-like stuff

library(dplyr)
library(ggplot2)
library(tidyr)
library(ggrepel)

## Read in the w, z, p, and q data
p <- readRDS("../EPAA/data/teams/p_20_20_5_10122024.rds")
q <- readRDS("../EPAA/data/teams/q_20_20_5_10122024.rds")

p_p <- readRDS("../EPAA/data/players/p_20_20_5_08122024.rds")
q_p <- readRDS("../EPAA/data/players/q_20_20_5_08122024.rds")

## Consider the mean of the last 1000 samples

get_probs_by_cluster <- function(dim, data){
  tmp <- data.frame(t(sapply(data, function(x) x[[dim]])))
  names(tmp) <- c("atb_3", "itp", "lc_3", "mid", "ra", "rc_3", "ft")
  return(tmp |> 
           tidyr::pivot_longer(1:7, names_to = "region") |> 
           dplyr::mutate(cluster = dim))
}

## Plot trace plots
## Zs and Ps are Shot making
## Ws and Qs are shot taking

df <- dplyr::bind_rows(get_probs_by_cluster(1, p),
                       get_probs_by_cluster(2, p),
                       get_probs_by_cluster(3, p),
                       get_probs_by_cluster(4, p),
                       get_probs_by_cluster(5, p),
                       get_probs_by_cluster(6, p),
                       get_probs_by_cluster(7, p),
                       get_probs_by_cluster(8, p),
                       get_probs_by_cluster(9, p),
                       get_probs_by_cluster(10, p),
                       get_probs_by_cluster(11, p),
                       get_probs_by_cluster(12, p),
                       get_probs_by_cluster(13, p),
                       get_probs_by_cluster(14, p),
                       get_probs_by_cluster(15, p),
                       get_probs_by_cluster(16, p),
                       get_probs_by_cluster(17, p),
                       get_probs_by_cluster(18, p),
                       get_probs_by_cluster(19, p),
                       get_probs_by_cluster(20, p)) |>  
  dplyr::mutate(index = rep(rep(1:10000, each = 7), 20)) 

fd <- df |> 
  filter(index >= 9001) |> 
  group_by(region, cluster) |> 
  summarize(m = mean(value)) |> 
  pivot_wider(names_from = "region", values_from = "m")

apply(fd[, -1], 1, sum)

saveRDS(fd, "data/team-shot-making-probs.rds")

df <- dplyr::bind_rows(get_probs_by_cluster(1, q),
                       get_probs_by_cluster(2, q),
                       get_probs_by_cluster(3, q),
                       get_probs_by_cluster(4, q),
                       get_probs_by_cluster(5, q),
                       get_probs_by_cluster(6, q),
                       get_probs_by_cluster(7, q),
                       get_probs_by_cluster(8, q),
                       get_probs_by_cluster(9, q),
                       get_probs_by_cluster(10, q),
                       get_probs_by_cluster(11, q),
                       get_probs_by_cluster(12, q),
                       get_probs_by_cluster(13, q),
                       get_probs_by_cluster(14, q),
                       get_probs_by_cluster(15, q),
                       get_probs_by_cluster(16, q),
                       get_probs_by_cluster(17, q),
                       get_probs_by_cluster(18, q),
                       get_probs_by_cluster(19, q),
                       get_probs_by_cluster(20, q)) |>  
  dplyr::mutate(index = rep(rep(1:10000, each = 7), 20)) 


fd <- df |> 
  filter(index >= 9001) |> 
  group_by(region, cluster) |> 
  summarize(m = mean(value)) |> 
  pivot_wider(names_from = "region", values_from = "m")

apply(fd[, -1], 1, sum)

saveRDS(fd, "data/team-shot-taking-probs.rds")

fd <- readRDS("data/team-shot-making-probs.rds")
d <- dist(fd[, -1])
fit <- cmdscale(d, eig=TRUE, k=1)
mds_fd <- data.frame(shot_making = rep(1:20, times = 20),
                     shot_taking = rep(1:20, each = 20))
mds_fd$shot_making_mds <- fit$points[,1]

fd <- readRDS("data/team-shot-taking-probs.rds")
d <- dist(fd[, -1])
fit <- cmdscale(d, eig=TRUE, k=1)
mds_fd$shot_taking_mds <- rep(fit$points[,1], each = 20)
mds_fd$type = "team"

fd <- readRDS("data/player-shot-making-probs.rds")
d <- dist(fd[, -1])
fit <- cmdscale(d, eig=TRUE, k=1)
mds_fd_2 <- data.frame(shot_making = rep(1:20, times = 20),
                       shot_taking = rep(1:20, each = 20))
mds_fd_2$shot_making_mds <- fit$points[,1]

fd <- readRDS("data/player-shot-taking-probs.rds")
d <- dist(fd[, -1])
fit <- cmdscale(d, eig=TRUE, k=1)
mds_fd_2$shot_taking_mds <- rep(fit$points[,1], each = 20)
mds_fd_2$type = "player"

fd <- rbind(mds_fd, mds_fd_2)

fd_team <- fd |> filter(type == "team")

p <- ggplot(data = fd,
            aes(x = shot_making_mds, y = shot_taking_mds))
p + geom_point() +
  facet_wrap(type ~ .) +
  theme_bw() +
  labs(x = "Shot Making",
       y = "Shot Taking")

## Add players
players <- readRDS("data/players/posterior-clusters-20-20-5-2021.rds") |> 
  filter(season == 2021) |> 
  group_by(team, z_cluster, w_cluster) |> 
  summarize(n = n()) |> 
  rename(shot_making = z_cluster, shot_taking = w_cluster) |> 
  ungroup() |> 
  filter(n > 2000)
## Z making; W taking

tmp <- fd |> 
  filter(type == "player") |> 
  left_join(players)
#options(scipen = 0)
p <- ggplot(data = tmp,
            aes(x = shot_making_mds, y = shot_taking_mds, label = team))
p + geom_point() +
  geom_text_repel(box.padding = 3.5, max.overlaps = 20) +
  geom_point(shape = ifelse(is.na(tmp$team), 19, 8)) +
  scale_x_continuous(limits = c(-.2, .4), breaks = seq(-.2, .4, by = .1)) +
  scale_y_continuous(limits = c(-.3, .3), breaks = seq(-.3, .3, by = .1),
                     labels = scales::number_format(accuracy = 0.1)) +
  theme_bw() +
  labs(x = "Shot Making",
       y = "Shot Taking")
ggsave("fig/mds-results.png", height = 6, wid = 8)

tmp <- fd |> 
  filter(type == "team")

p <- ggplot(data = fd,
            aes(x = shot_making_mds, y = shot_taking_mds))
p + geom_point() +
  facet_wrap(. ~ type) +
  theme_bw() +
  scale_x_continuous(limits = c(-.2, .4), breaks = seq(-.2, .4, by = .1)) +
  scale_y_continuous(limits = c(-.3, .3), breaks = seq(-.3, .3, by = .1),
                     labels = scales::number_format(accuracy = 0.1)) +
  labs(x = "Shot Making",
       y = "Shot Taking")
ggsave("fig/mds-results-both.png", height = 7, wid = 11)

#calculate distance matrix
fd <- readRDS("data/team-shot-making-probs.rds")
m1 <- as.matrix(fd[, -1])
tmp_1 <- m1[rep(1:nrow(m1), times = 10), ]
fd <- readRDS("data/team-shot-taking-probs.rds")
m2 <- as.matrix(fd[, -1])
tmp_2 <- m2[rep(1:nrow(m2), each = 10), ]
tmp <- cbind(tmp_1, tmp_2)
d <- dist(tmp)
#perform multidimensional scaling
fit <- cmdscale(d, eig=TRUE, k=2)

mds_fd <- data.frame(shot_making = rep(1:10, times = 10),
                     shot_taking = rep(1:10, each = 10),
                     d1 = fit$points[, 1],
                     d2 = fit$points[, 2])

p <- ggplot(data = mds_fd,
            aes(x = d1, y = d2))

p + geom_point()

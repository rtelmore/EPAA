## Ryan Elmore
## 06 Mar 2025
## MDS-like stuff

library(dplyr)
library(ggplot2)
library(tidyr)

## Read in the w, z, p, and q data
p <- readRDS("../EPAA/data/teams/p_10_10_5_22102023.rds")
q <- readRDS("../EPAA/data/teams/q_10_10_5_22102023.rds")

p_p <- readRDS("../EPAA/data/players/p_10_10_5_23102023.rds")
q_p <- readRDS("../EPAA/data/players/q_10_10_5_23102023.rds")

## Consider the mean of the last 1000 samples

get_probs_by_cluster <- function(dim, data){
  tmp <- data.frame(t(sapply(data, function(x) x[[dim]])))
  names(tmp) <- c("atb_3", "itp", "lc_3", "mid", "ra", "rc_3", "ft")
  return(tmp |> 
           tidyr::pivot_longer(1:7, names_to = "region") |> 
           dplyr::mutate(cluster = dim))
}

## Plot trace plots
## Shot taking

df <- dplyr::bind_rows(get_probs_by_cluster(1, p_p),
                       get_probs_by_cluster(2, p_p),
                       get_probs_by_cluster(3, p_p),
                       get_probs_by_cluster(4, p_p),
                       get_probs_by_cluster(5, p_p),
                       get_probs_by_cluster(6, p_p),
                       get_probs_by_cluster(7, p_p),
                       get_probs_by_cluster(8, p_p),
                       get_probs_by_cluster(9, p_p),
                       get_probs_by_cluster(10, p_p)) |>  
  dplyr::mutate(index = rep(rep(1:10000, each = 7), 10)) 

fd <- df |> 
  filter(index >= 9001) |> 
  group_by(region, cluster) |> 
  summarize(m = mean(value)) |> 
  pivot_wider(names_from = "region", values_from = "m")

apply(fd[, -1], 1, sum)

saveRDS(fd, "data/player-shot-making-probs.rds")

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

fd <- readRDS("data/team-shot-making-probs.rds")
d <- dist(fd[, -1])
fit <- cmdscale(d, eig=TRUE, k=1)
mds_fd <- data.frame(shot_making = rep(1:10, times = 10),
                     shot_taking = rep(1:10, each = 10))
mds_fd$shot_making_mds <- fit$points[,1]

fd <- readRDS("data/team-shot-taking-probs.rds")
d <- dist(fd[, -1])
fit <- cmdscale(d, eig=TRUE, k=1)
mds_fd$shot_taking_mds <- rep(fit$points[,1], each = 10)
mds_fd$type = "team"

fd <- readRDS("data/player-shot-making-probs.rds")
d <- dist(fd[, -1])
fit <- cmdscale(d, eig=TRUE, k=1)
mds_fd_2 <- data.frame(shot_making = rep(1:10, times = 10),
                       shot_taking = rep(1:10, each = 10))
mds_fd_2$shot_making_mds <- fit$points[,1]

fd <- readRDS("data/player-shot-taking-probs.rds")
d <- dist(fd[, -1])
fit <- cmdscale(d, eig=TRUE, k=1)
mds_fd_2$shot_taking_mds <- rep(fit$points[,1], each = 10)
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
ggsave("fig/mds-results.png", height = 6, wid = 8)

## Add players
players <- readRDS("../EPAA/data/players/posterior-clusters-10-10-5-2021.rds") |> 
  filter(season == 2021) |> 
  group_by(team, z_cluster, w_cluster) |> 
  summarize(n = n())
 
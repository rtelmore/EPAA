## Ryan Elmore
## Posterior Analysis

library(coda)
library(dplyr)
library(ggplot2)

alpha <- 5
dimension <- 20
string <- paste(dimension, "_", dimension, "_", alpha, sep = "")

w <- readRDS(paste("data/teams/w_pp_", string, "_10122024.rds", sep = ""))
z <- readRDS(paste("data/teams/z_pp_", string, "_10122024.rds", sep = ""))
p <- readRDS(paste("data/teams/p_", string, "_10122024.rds", sep = ""))
q <- readRDS(paste("data/teams/q_", string, "_10122024.rds", sep = ""))

## Check Effective Sample Size:
get_probs_by_cluster <- function(dim, data){
  tmp <- data.frame(t(sapply(data, function(x) x[[dim]])))
  names(tmp) <- c("atb_3", "itp", "lc_3", "mid", "ra", "rc_3", "ft")
  return(tmp %>%
           tidyr::pivot_longer(., 1:7, names_to = "region") %>%
           dplyr::mutate(., cluster = dim))
}

## Plot trace plots
## Shot taking

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
                       get_probs_by_cluster(20, p)) %>% 
  dplyr::mutate(index = rep(rep(1:10000, each = 7), 20)) 

p1 <- ggplot(df, aes(x = value, fill = region))
p1 + geom_density(alpha = .5) +
  facet_wrap(~ cluster, scales = "free_y") +
  theme_bw()

p2 <- ggplot(df, aes(x = index, y = value, col = region))
p2 + geom_line(alpha = .5) +
  facet_grid(cluster ~ .) +
  scale_color_brewer(palette = "Dark2") +
  theme_bw()


df_q <- dplyr::bind_rows(get_probs_by_cluster(1, q),
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
                         get_probs_by_cluster(20, q)) %>% 
  dplyr::mutate(index = rep(rep(1:10000, each = 7), 20)) 

p2 <- ggplot(df |> filter(cluster == 15), 
             aes(x = index, y = value, col = region))
p2 + geom_line(alpha = .5) +
  scale_color_brewer(palette = "Dark2") +
  theme_bw() +
  labs(x = "sample index",
       y = "shot making probabilities for a cluster") +
  scale_y_continuous(breaks = seq(0, .3, by = .05))
ggsave("fig/teams/shot-making-probablities-2.png", height = 11, width = 8.5)

p2 <- ggplot(df_q |> filter(cluster == 15), 
             aes(x = index, y = value, col = region))
p2 + geom_line(alpha = .5) +
  scale_color_brewer(palette = "Dark2") +
  theme_bw() +
  labs(x = "sample index",
       y = "shot taking probabilities for a cluster") +
  scale_y_continuous(breaks = seq(0, .3, by = .05))
ggsave("fig/teams/shot-taking-probablities-2.png", height = 11, width = 8.5)

results <- matrix(NA, nr = 10, nc = 7)
regions <- c("atb_3", "itp", "lc_3", "mid", "ra", "rc_3", "ft")
for(i in seq_along(regions)){
  for(j in 1:10){
    df <- get_probs_by_cluster(j, p) |> 
      filter(region == regions[i])
    results[j, i] <- coda::effectiveSize(df$value[1:10000])
  }
}

results

results <- matrix(NA, nr = 10, nc = 7)
regions <- c("atb_3", "itp", "lc_3", "mid", "ra", "rc_3", "ft")
for(i in seq_along(regions)){
  for(j in 1:10){
    df <- get_probs_by_cluster(j, q) |> 
      filter(region == regions[i])
    results[j, i] <- coda::effectiveSize(df$value[6500:10000])
  }
}
results

df <- get_probs_by_cluster(7, p) |> 
  filter(region == "atb_3")

coda::effectiveSize(df$value)

## Range from 2K - almost 6K
get_probs_by_cluster <- function(dim, data){
  tmp <- data.frame(t(sapply(data, function(x) x[[dim]])))
  names(tmp) <- c("atb_3", "itp", "lc_3", "mid", "ra", "rc_3", "ft")
  return(tmp %>%
           tidyr::pivot_longer(., 1:7, names_to = "region") %>%
           dplyr::mutate(., cluster = dim))
}

df <- get_probs_by_cluster(10, q) |> 
  filter(region == "rc_3")

coda::effectiveSize(df$value)


tt <- dplyr::bind_rows(get_probs_by_cluster(1, q),
                       get_probs_by_cluster(2, q),
                       get_probs_by_cluster(3, q),
                       get_probs_by_cluster(4, q),
                       get_probs_by_cluster(5, q)) %>%
  dplyr::group_by(., region, cluster) %>%
  dplyr::summarize(., m = mean(value))
p1 <- ggplot(tt, aes(x = cluster, y = m, fill = region))
p1 + geom_bar(stat = "identity") +
  scale_fill_brewer(palette = "Set1") +
  labs(y = "probability",
       title = "Shot Taking Distribution by Cluster") +
  theme_bw()

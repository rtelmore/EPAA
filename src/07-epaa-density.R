## Ryan Elmore
## 28 Feb 2023
## Look at distribution of EPAA and possibly EPAA vs playing time

library(ggplot2)
library(dplyr)
library(ggrepel)


df <- readRDS("app/NBA-apaa/all-pts-above-avg-18-21.rds")
df_plot <- df |> 
  dplyr::mutate(points = player_points - team_points,
                sample = rep(1:10000, times = 399)) |> 
  dplyr::filter(sample >= 3001,
                season == 2021)


p <- ggplot(data = df_plot |> 
              dplyr::group_by(season, player) |> 
              dplyr::summarize(epaa = mean(points)/72),
            aes(x = epaa))
p + geom_density() +
  geom_rug() + 
  theme_b

df_props <- readRDS("data/all-avg-pts-above-data-2021.rds") 

df_agg <- readRDS("app/NBA-apaa/all-pts-above-avg-18-21.rds") |> 
  dplyr::filter(season == 2021) |> 
  dplyr::mutate(points = player_points - team_points,
                sample = rep(1:10000, times = 100)) |> 
  dplyr::filter(sample >= 3001) |> 
  dplyr::group_by(player) |> 
  dplyr::summarize(m = mean(points)/72)

df_comb <- dplyr::left_join(df_props, df_agg) |> 
  dplyr::mutate(new_player = ifelse(m <= -1 | m > .9 | prop > .2, player, ""))

p <- ggplot(df_comb, aes(y = m, x = prop, label = new_player))
p + geom_point() +
  scale_y_continuous(breaks = seq(-4, 2, by = 1), limits = c(-4, 2.1)) +
  scale_x_continuous(breaks = seq(.07, .25, by = .02), limits = c(.08, .25)) +
  geom_text_repel() +
  geom_hline(yintercept = 0, linetype = 3) +
  labs(x = "proportion of team shots by player", 
       y = "mean points above/below average per game") +
  theme_bw()
ggsave("doc/fig/fg_pts_above_avg_by_prop_shots.png", hei = 8, wid = 12)


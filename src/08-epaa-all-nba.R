## Ryan Elmore
## Pts above/below average

library(dplyr)
library(ggplot2)
library(tidyr)
library(ggthemes)
library(stringi)
library(ggrepel)

season_sim <- 2021
results <- readRDS(paste("data/all-pts-above-avg-", season_sim, ".rds", sep = ""))
df <- results |> 
  dplyr::mutate(points = player_points - team_points,
                sample = rep(1:10000, times = 100)) |> 
  dplyr::filter(sample >= 3001)

players_2021 <- df |> 
  dplyr::group_by(player) |> 
  dplyr::summarize(m = mean(points)/72) |> 
  dplyr::arrange(desc(m)) |> 
  dplyr::top_n(20) |> 
  dplyr::mutate(all_nba = c(1, 3, 1, 3, 2, NA, NA, NA, NA, 2, NA, NA, NA, NA, 
                            NA, NA, NA, 2, 3, NA),
                all_star = c(T, T, T, T, T, F, F, F, F, T, F, F, T, F, F, 
                              F, F, T, T, T)) |> 
  dplyr::ungroup()

players <- players_2021 |> 
  dplyr::pull(player)
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

df_fig <- dplyr::left_join(df_grouped, players_2021)

p <- ggplot(data = df_fig |> 
              dplyr::mutate(player = fct_reorder(player, -y2)),
            aes(x = player, ymin = y0, lower = y1, middle = y2, upper = y3,
                ymax = y4, fill = all_star))#, group = player, fill = player))
p + geom_boxplot(stat = "identity") +
  geom_hline(yintercept = 0, linetype = 3) +
  labs(x = "", 
       y = "expected points per game above average",
       fill = "All Star") +
  theme_bw() +
  scale_fill_brewer(palette = "Set1") +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))

p <- ggplot(data = df_fig |> 
              dplyr::mutate(player = fct_reorder(player, -y2)),
            aes(x = player, ymin = y0, lower = y1, middle = y2, upper = y3,
                ymax = y4, fill = as.factor(all_nba)))#, group = player, fill = player))
p + geom_boxplot(stat = "identity") +
  geom_hline(yintercept = 0, linetype = 3) +
  labs(x = "", 
       y = "expected points per game above average",
       fill = "All NBA") +
  theme_bw() +
  scale_fill_brewer(palette = "Set1") +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))

## All NBA

all_nba <- data.frame(player = c("Giannis Antetokounmpo", "Stephen Curry", 
                                 "Luka Doncic", "Nikola Jokic", "Kawhi Leonard",
                                 "Joel Embiid", "LeBron James", "Damian Lillard",
                                 "Chris Paul", "Julius Randle", "Bradley Beal",
                                 "Jimmy Butler", "Paul George", "Rudy Gobert", 
                                 "Kyrie Irving", "Khris Middleton", "Devin Booker",
                                 "Michael Porter Jr.", "Zion Williamson", "Tobias Harris"),
                      all_nba = c(rep(1:3, each = 5), rep(NA, 5)))

df_grouped <- df |> 
  dplyr::filter(player %in% all_nba$player) |> 
  dplyr::group_by(player) |> 
  dplyr::summarize(y0 = quantile(points, 0.05)/72,
                   y1 = quantile(points, 0.2)/72,
                   y2 = quantile(points, 0.5)/72,
                   y3 = quantile(points, 0.8)/72,
                   y4 = quantile(points, 0.95)/72) |> 
  dplyr::ungroup() |> 
  dplyr::arrange(desc(y2))

df_fig <- dplyr::left_join(df_grouped, all_nba)

p <- ggplot(data = df_fig |> 
              dplyr::mutate(player = forcats::fct_reorder(player, -y2)),
            aes(x = player, ymin = y0, lower = y1, middle = y2, upper = y3,
                ymax = y4, fill = as.factor(all_nba)))#, group = player, fill = player))
p + geom_boxplot(stat = "identity") +
  labs(x = "", 
       y = "expected points per game above average",
       fill = "All NBA") +
  theme_bw() +
  scale_fill_brewer(palette = "Set2") +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1,
                                   face = c(rep('plain', 5), rep('bold', 2),
                                            'plain', 'bold', 'bold', 'plain', 'bold',
                                            rep('plain', 7)),
                                   colour = c(rep('black', 5), rep('maroon', 2),
                                              'black', 'maroon', 'maroon', 
                                              'black', 'maroon',
                                              rep('black', 7))))

## PER and BPM

per_bpm <- read.csv("data/per_bpm_20_21.csv") |> 
  dplyr::mutate(player = stringi::stri_trans_general(player, "Latin-ASCII")) |> 
  dplyr::select(player, per, bpm) |> 
  na.omit() |> 
  dplyr::mutate(rank_per = rank(-per),
                rank_bpm = rank(-bpm))

players_2021 <- df |> 
  dplyr::group_by(player) |> 
  dplyr::summarize(m = mean(points)/72) |> 
  dplyr::arrange(desc(m)) |> 
  dplyr::ungroup() |> 
  dplyr::mutate(rank_epaa = rank(-m))

df_per_bpm <- dplyr::left_join(players_2021, per_bpm)

p <- ggplot(data = df_per_bpm,
            aes(x = rank_epaa, y = rank_per))
p + geom_point() +
  theme_bw()

p <- ggplot(data = df_per_bpm |> 
              dplyr::mutate(new_player = ifelse(m < -1 | bpm > 5.5 | m > 1 | 
                                                  bpm < -.5, player, NA)),
            aes(x = m, y = bpm, label = new_player))
p + geom_point() +
  scale_x_continuous(breaks = seq(-4, 2, by = 1), limits = c(-4, 2.1)) +
  scale_y_continuous(breaks = seq(-2, 13, by = 2)) +
  geom_text_repel() +
  labs(y = "Box Plus/Minus", 
       x = "Mean EPAA per Game") +
  theme_bw()

p <- ggplot(data = df_per_bpm |> 
              dplyr::mutate(new_player = ifelse(m < -1 | per > 25 | m > 1 | 
                                                  per < 13.5, player, NA)),
            aes(x = m, y = per, label = new_player))
p + geom_point() +
  scale_x_continuous(breaks = seq(-4, 2, by = 1), limits = c(-4, 2.1)) +
  scale_y_continuous(breaks = seq(10, 35, by = 5)) +
  geom_text_repel() +
  labs(y = "Player Efficiency Rating", 
       x = "Mean EPAA per Game") +
  theme_bw()

p <- ggplot(data = df_per_bpm,
            aes(x = rank_epaa, y = rank_per))
p + geom_point() +
  labs(y = "Ranked Player Efficiency Rating", 
       x = "Ranks Mean Points Above/Below Average per Game") +
  theme_bw()

df_new <- df_per_bpm |> na.omit()
cor(df_new$m, df_new$per, method = "kendall")
cor(df_new$m, df_new$bpm, method = "kendall")

cor(df_new$m, df_new$per)
cor(df_new$m, df_new$bpm)
cor(df_new$per, df_new$bpm)

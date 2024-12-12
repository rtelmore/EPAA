## Ryan Elmore
## Pts above/below average

library(dplyr)
library(ggplot2)
library(tidyr)
library(ggthemes)
library(stringi)
library(ggrepel)

#season_sim <- 2021
#results <- readRDS("data/all-pts-above-avg-20-20-5-2021.rds")
results <- readRDS("data/all-pts-above-avg-20-20-5-2021.rds")
df <- results |> 
  dplyr::mutate(points = player_points - team_points,
                sample = rep(1:10000, times = 100)) |> 
  dplyr::filter(sample >= 3001)

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
saveRDS(df_fig, "data/all_nba/df_fig_20_20_0.1.rds")

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
                                   face = c(rep('plain', 3), rep('bold', 3),
                                            rep('plain', 7), 'bold', 
                                            rep('plain', 2), 'bold',
                                            rep('plain', 2)),
                                   colour = c(rep('black', 3), rep('maroon', 3),
                                              rep('black', 7), 'maroon', 
                                              rep('black', 2), 'maroon',
                                              rep('black', 2))))

ggsave("fig/fg_pts_above_avg_player_all_nba_snubs_20_20_5_2021_revision_2.png",
       hei = 5, wid = 7)

## Supplement Figure

df <- bind_rows(readRDS("data/all_nba/df_fig_20_20_5.rds") |>
                  dplyr::mutate(alpha = 5, dim1 = 20, dim2 = 20),
                readRDS("data/all_nba/df_fig_20_20_0.1.rds") |>
                  dplyr::mutate(alpha = 0.1, dim1 = 20, dim2 = 20),
                readRDS("data/all_nba/df_fig_30_30_0.1.rds") |>
                  dplyr::mutate(alpha = 0.1, dim1 = 30, dim2 = 30),
                readRDS("data/all_nba/df_fig_10_10_5.rds") |>
                  dplyr::mutate(alpha = 5, dim1 = 10, dim2 = 10),
                readRDS("data/all_nba/df_fig_30_30_5.rds") |>
                  dplyr::mutate(alpha = 5, dim1 = 30, dim2 = 30),
                readRDS("data/all_nba/df_fig_10_10_0.1.rds") |>
                  dplyr::mutate(alpha = 0.1, dim1 = 10, dim2 = 10),
                readRDS("data/all_nba/df_fig_10_30_5.rds") |>
                  dplyr::mutate(alpha = 5, dim1 = 10, dim2 = 30),
                readRDS("data/all_nba/df_fig_30_10_5.rds") |>
                  dplyr::mutate(alpha = 5, dim1 = 30, dim2 = 10),
                readRDS("data/all_nba/df_fig_10_30_0.1.rds") |>
                  dplyr::mutate(alpha = 0.1, dim1 = 10, dim2 = 30),
                readRDS("data/all_nba/df_fig_30_10_0.1.rds") |>
                  dplyr::mutate(alpha = 0.1, dim1 = 30, dim2 = 10))

p <- ggplot(data = df |>
              filter(dim1 == 10) |> 
              dplyr::mutate(player = forcats::fct_reorder(player, y2),
                            cat = paste("(", paste(dim1, dim2, alpha, sep = ", "), ")", sep = "")),
            aes(x = player, ymin = y0, lower = y1, middle = y2, upper = y3,
                ymax = y4, fill = cat))#, group = player, fill = player))
p + geom_boxplot(stat = "identity") +
  labs(x = "",
       y = "expected points per game above average") +
  guides(fill = guide_legend(title = "(J, L, alpha):")) +
  scale_fill_brewer(palette = "Set2") +
  theme_bw() +
  theme(axis.text.y = element_text(face = c(rep('plain', 7), 'bold',
                                            rep('plain', 2), rep('bold', 4),
                                            rep('plain', 5)),
                                   colour = c(rep('black', 7), 'maroon',
                                              rep('black', 2), rep('maroon', 4),
                                              rep('black', 5)))) +
  coord_flip() +
  theme(legend.position = "bottom")
ggsave("fig/players/epaa-all-nba-10-accuracy.png", height = 11, width = 8.5)

# 
# 
# ggsave("fig/fg_pts_above_avg_player_all_nba_snubs_2021_all_combos_revision.png",
#        hei = 11, wid = 8.5)


# p <- ggplot(data = df |> 
#               dplyr::mutate(player = forcats::fct_reorder(player, -y2),
#                             cat = paste("(", paste(dim, alpha, sep = ", "), ")", 
#                                         sep = "")),
#             aes(x = player, ymin = y0, lower = y1, middle = y2, upper = y3,
#                 ymax = y4, fill = as.factor(all_nba)))#, group = player, fill = player))
# p + geom_boxplot(stat = "identity") +
#   labs(x = "", 
#        y = "expected points per game above average",
#        fill = "All NBA") +
#   scale_fill_brewer(palette = "Set2") +
#   scale_y_continuous(breaks = seq(-4, 6, by = 2), limits = c(-4, 6)) +
#   facet_grid(cat ~ .) +
#   theme_light() +
#   theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1,
#                                    face = c(rep('plain', 5), 'bold', 'bold',
#                                             'plain', rep('bold', 2), 
#                                             rep('plain', 3), 'bold', 
#                                             rep('plain', 5)),
#                                    colour = c(rep('black', 5), 'maroon',
#                                               'maroon', 'black',
#                                               rep('maroon', 2), rep('black', 3), 
#                                               'maroon', rep('black', 5)))) +
#   theme(legend.position = "bottom")

# ggsave("fig/fg_pts_above_avg_player_all_nba_snubs_2021_all_combos_2_revision.png",
#        hei = 11, wid = 8.5)

# players_2021 <- df |> 
#   dplyr::group_by(player) |> 
#   dplyr::summarize(m = mean(points)/72) |> 
#   dplyr::arrange(desc(m)) |> 
#   dplyr::top_n(20) |> 
#   dplyr::mutate(all_nba = c(3, 1, 1, 2, 3, NA, NA, NA, NA, 2, NA, NA, NA, 
#                             NA, NA, NA, NA, NA, 3, 2),
#                 all_star = c(T, T, T, T, T, F, F, F, F, T, F, T, 
#                              T, F, F, F, F, T, T, T)) |> 
#   dplyr::ungroup()

# players <- players_2021 |> 
#   dplyr::pull(player)
# df_grouped <- df |> 
#   dplyr::filter(player %in% players) |>
#   dplyr::group_by(player) |> 
#   dplyr::summarize(y0 = quantile(points, 0.05)/72,
#                    y1 = quantile(points, 0.2)/72,
#                    y2 = quantile(points, 0.5)/72,
#                    y3 = quantile(points, 0.8)/72,
#                    y4 = quantile(points, 0.95)/72) |> 
#   dplyr::ungroup() |> 
#   dplyr::arrange(desc(y2))
# 
# df_fig <- dplyr::left_join(df_grouped, players_2021)

# p <- ggplot(data = df_fig |> 
#               dplyr::mutate(player = fct_reorder(player, -y2)),
#             aes(x = player, ymin = y0, lower = y1, middle = y2, upper = y3,
#                 ymax = y4, fill = all_star))#, group = player, fill = player))
# p + geom_boxplot(stat = "identity") +
#   geom_hline(yintercept = 0, linetype = 3) +
#   labs(x = "", 
#        y = "expected points per game above average",
#        fill = "All Star") +
#   theme_bw() +
#   scale_fill_brewer(palette = "Set1") +
#   theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))
# 
# p <- ggplot(data = df_fig |> 
#               dplyr::mutate(player = fct_reorder(player, -y2)),
#             aes(x = player, ymin = y0, lower = y1, middle = y2, upper = y3,
#                 ymax = y4, fill = as.factor(all_nba)))#, group = player, fill = player))
# p + geom_boxplot(stat = "identity") +
#   geom_hline(yintercept = 0, linetype = 3) +
#   labs(x = "", 
#        y = "expected points per game above average",
#        fill = "All NBA") +
#   theme_bw() +
#   scale_fill_brewer(palette = "Set1") +
#   theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))
# 
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
                                            'plain', rep('bold', 3), 
                                            rep('plain', 8)),
                                   colour = c(rep('black', 5), rep('maroon', 2),
                                              'black', rep('maroon', 3), 
                                              rep('black', 8))))

ggsave("fig/fg_pts_above_avg_player_all_nba_snubs_2021_revision.png",
       hei = 5, wid = 7)

ggsave("fig/all-nba-revision.png", height = 8.5, width = 11)
## PER and BPM

results <- readRDS("data/all-pts-above-avg-20-20-5-2021.rds")
df <- results |> 
  dplyr::mutate(points = player_points - team_points,
                sample = rep(1:10000, times = 100)) |> 
  dplyr::filter(sample >= 3001)

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
  scale_x_continuous(breaks = seq(-3, 4, by = 1), limits = c(-3, 4)) +
  scale_y_continuous(breaks = seq(-4, 15, by = 2)) +
  geom_text_repel() +
  labs(y = "Box Plus/Minus", 
       x = "Mean EPAA per Game") +
  theme_bw()
ggsave("fig/fg_pts_above_avg_vs_bpm_revision_2.png",
       hei = 5, wid = 6.5)

p <- ggplot(data = df_per_bpm |> 
              dplyr::mutate(new_player = ifelse(m < -1 | per > 25 | m > 1 | 
                                                  per < 13.5, player, NA)),
            aes(x = m, y = per, label = new_player))
p + geom_point() +
  scale_x_continuous(breaks = seq(-3, 4, by = 1), limits = c(-3, 4)) +
  scale_y_continuous(breaks = seq(10, 35, by = 5)) +
  geom_text_repel() +
  labs(y = "Player Efficiency Rating", 
       x = "Mean EPAA per Game") +
  theme_bw()

ggsave("fig/fg_pts_above_avg_vs_per_revision_2.png",
       hei = 5, wid = 6.5)

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


## Ryan Elmore
## Summary stats for paper
## 13 Feb 2023

library(dplyr)
library(tidyr)
library(ggplot2)

df <- readRDS("data/nba-zone-combined-team-w-fts.rds") |> 
  tidyr::separate(team_season, into = c("team", "season"), sep = "_") |> 
  dplyr::select(season, tidyr::ends_with("_n"), tidyr::ends_with("m"), -team) |>
  dplyr::group_by(season) |> 
  dplyr::summarize_all(sum)

df_p <- dplyr::select(df, tidyr::ends_with("_m")) / dplyr::select(df, tidyr::ends_with("_n")) 
names(df_p) <- gsub("_m", "_p", names(df_p))

df <- df |> 
  dplyr::bind_cols(df_p)

df_summary <- dplyr::bind_rows(
  df |> 
    dplyr::select(season, tidyr::ends_with("_n")) |> 
    tidyr::pivot_longer(-1) |> 
    dplyr::mutate(name = gsub("_n", "", name),
                  type = "Shots"),
  df |> 
    dplyr::select(season, tidyr::ends_with("_p")) |> 
    tidyr::pivot_longer(-1) |> 
    dplyr::mutate(name = gsub("_p", "", name),
                  type = "Percentage"),
  df |> 
    dplyr::select(season, tidyr::ends_with("_m")) |> 
    tidyr::pivot_longer(-1) |> 
    dplyr::mutate(name = gsub("_m", "", name),
                  type = "Makes")
) |> 
  dplyr::mutate(name = case_when(
    name == 'itp' ~ 'ITP',
    name == 'ft' ~ 'FT',
    name == 'lc_3' ~ 'LC3',
    name == 'rc_3' ~ 'RC3',
    name == 'mid' ~ 'MID',
    name == 'ra' ~ 'RA',
    name == 'atb_3' ~ 'ATB',
  ))

cols <- c("ATB" = "#e41a1c", "RA" = "#ff7f00", "MID" = "#984ea3", 
          "RC3" = "#ffff33", "LC3" = "#4daf4a", "FT" = "#a65628",
          "ITP" = "#377eb8")
p1 <- ggplot(data = df_summary |> dplyr::filter(type %in% c("Shots")),
            aes(x = season, y = value, col = name, group = name)) + 
  geom_point() +
  geom_line() +
  theme_bw() +
  scale_y_continuous(breaks = seq(0, 80000, by = 10000)) +
  labs(x = "Season",
       y = "Number of Shots Taken",
       col = "Region:") +
  scale_color_manual(values = cols)

p2 <- ggplot(data = df_summary |> dplyr::filter(type %in% c("Percentage")),
            aes(x = season, y = value, col = name, group = name)) + 
  geom_point() +
  geom_line() +
  theme_bw() +
  scale_y_continuous(labels = scales::percent_format(accuracy=1), limits = c(.3, .8)) +
  labs(x = "Season",
       y = "Percentage of Shots Made",
       col = "region") +
  scale_color_manual(values = cols)

prow <- cowplot::plot_grid(
  p1 + theme(legend.position="none"),
  p2 + theme(legend.position="none"),
  align = 'vh',
  nrow = 2
)

legend <- cowplot::get_legend(
  p1 +
    guides(color = guide_legend(nrow = 1)) +
    theme(legend.position = "bottom", 
          legend.box.margin = margin(-45, 0, 0, 0))
)

p_final <- cowplot::plot_grid(prow, legend, nrow = 2, rel_heights = c(1, .1))
p_final
cowplot::save_plot("doc/fig/eda-cowplot.png", p_final, 
                   base_height = 10, base_width = 8)

## App figure
df <- readRDS("data/nba-zone-combined-team-w-fts.rds") |> 
  tidyr::separate(team_season, into = c("team", "season"), sep = "_") |> 
  dplyr::select(season, team, tidyr::ends_with("_n"), tidyr::ends_with("m")) |>
  dplyr::group_by(season, team) |> 
  dplyr::summarize_all(sum) |> 
  dplyr::ungroup()

df_p <- dplyr::select(df, tidyr::ends_with("_m"), - team) / dplyr::select(df, tidyr::ends_with("_n"), -season) 
names(df_p) <- gsub("_m", "_p", names(df_p))

df <- df |> 
  dplyr::bind_cols(df_p)

tmp <- df |> 
  dplyr::select(season, team, tidyr::ends_with("_n")) |> 
  tidyr::pivot_longer(-c(1, 2)) |> 
  dplyr::mutate(name = gsub("_n", "", name),
                type = "Shots")

df_summary <- dplyr::bind_rows(
  df |> 
    dplyr::select(season, team, tidyr::ends_with("_n")) |> 
    tidyr::pivot_longer(-c(1, 2)) |> 
    dplyr::mutate(name = gsub("_n", "", name),
                  type = "Shots"),
  df |> 
    dplyr::select(season, team, tidyr::ends_with("_p")) |> 
    tidyr::pivot_longer(-c(1, 2)) |> 
    dplyr::mutate(name = gsub("_p", "", name),
                  type = "Percentage"),
  df |> 
    dplyr::select(season, team, tidyr::ends_with("_m")) |> 
    tidyr::pivot_longer(-c(1, 2)) |> 
    dplyr::mutate(name = gsub("_m", "", name),
                  type = "Makes")
)

df_summary$team[df_summary$team == "New Orleans Hornets"] <- "New Orleans Pelicans"
df_summary$team[df_summary$team == "LA Clippers"] <- "Los Angeles Clippers"
df_summary$team[df_summary$team == "Charlotte Bobcats"] <- "Charlotte Hornets"
df_summary$team[df_summary$team == "New Jersey Nets"] <- "Brooklyn Nets"
length(unique(df_summary$team))

tmp <- df_summary |> 
  dplyr::filter(type %in% c("Shots"),
                team == "Denver Nuggets")

p1 <- ggplot(data = df_summary |> 
               dplyr::filter(type %in% c("Shots"),
                             team == "Denver Nuggets"),
             aes(x = season, y = value, col = name, group = name)) + 
  geom_point() +
  geom_line() +
  theme_bw() +
  scale_y_continuous(breaks = seq(0, 4000, by = 500)) +
  labs(x = "Season",
       y = "Number of Shots Taken",
       col = "Region:") +
  scale_color_brewer(palette = "Paired")

p2 <- ggplot(data = df_summary |> dplyr::filter(type %in% c("Percentage"),
                                                team == "Denver Nuggets"),
             aes(x = season, y = value, col = name, group = name)) + 
  geom_point() +
  geom_line() +
  theme_bw() +
  scale_y_continuous(labels = scales::percent_format(accuracy=1), 
                     breaks = seq(.3, .8, by = .1),
                     limits = c(.25, .8)) +
  labs(x = "Season",
       y = "Percentage of Shots Made",
       col = "region") +
  scale_color_brewer(palette = "Paired") 

prow <- cowplot::plot_grid(
  p1 + theme(legend.position="none"),
  p2 + theme(legend.position="none"),
  align = 'vh',
  nrow = 2
)

legend <- cowplot::get_legend(
  p1 +
    guides(color = guide_legend(nrow = 1)) +
    theme(legend.position = "bottom", 
          legend.box.margin = margin(-45, 0, 0, 0))
)

cowplot::plot_grid(prow, legend, nrow = 2, rel_heights = c(1, .1))

p <- ggplot(data = df_summary |> 
               dplyr::filter(type %in% c("Shots"),
                             team == "Denver Nuggets"),
             aes(x = season, y = value, col = name, group = name)) 
p + geom_point() +
  geom_line() +
  theme_bw() +
  labs(x = "Season",
       y = "Number of Shots Taken",
       col = "Region:") +
  scale_color_brewer(palette = "Paired")

saveRDS(df_summary, "app/NBA-apaa/summary-shots-for-app.rds")

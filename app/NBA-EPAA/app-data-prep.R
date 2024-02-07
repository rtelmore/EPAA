## Ryan Elmore
## Data for shiny app

library(dplyr)

df <- dplyr::bind_rows(readRDS("data/all-pts-above-avg-2013.rds") |> 
                         dplyr::mutate(season = 2013),
                       readRDS("data/all-pts-above-avg-2014.rds") |> 
                         dplyr::mutate(season = 2014),
                       readRDS("data/all-pts-above-avg-2015.rds") |> 
                         dplyr::mutate(season = 2015),
                       readRDS("data/all-pts-above-avg-2016.rds") |> 
                         dplyr::mutate(season = 2016),
                       readRDS("data/all-pts-above-avg-2017.rds") |> 
                         dplyr::mutate(season = 2017),
                       readRDS("data/all-pts-above-avg-2018.rds") |> 
                         dplyr::mutate(season = 2018),
                       readRDS("data/all-pts-above-avg-2019.rds") |> 
                         dplyr::mutate(season = 2019),
                       readRDS("data/all-pts-above-avg-2020.rds") |> 
                         dplyr::mutate(season = 2020),
                       readRDS("data/all-pts-above-avg-2021.rds") |> 
                         dplyr::mutate(season = 2021))
saveRDS(df, "app/NBA-EPAA/all-pts-above-avg-13-21.rds")

tmp <- df |> 
  dplyr::filter(season == 2021,
                player %in% c("LeBron James", "James Harden")) |> 
  dplyr::mutate(points = player_points - team_points,
                sample = rep(1:10000, times = 2)) |> 
  dplyr::filter(sample >= 3000)
p <- ggplot(data = tmp,
            aes(x = points, color = player))
p + geom_density()

df_2 <- df |> 
  distinct(player, season) |> 
  group_by(player) |> 
  summarize(n = n())

df_ranks <- df |> 
  dplyr::mutate(points = player_points - team_points,
                sample = rep(1:10000, times = 895)) |> 
  dplyr::filter(sample >= 3001) |> 
  dplyr::group_by(player, season) |> 
  dplyr::summarize(avg = mean(points)/72, med = median(points)/72) |> 
  dplyr::ungroup() |> 
  dplyr::group_by(season) |> 
  dplyr::mutate(rank_mean = rank(-avg), rank_med = rank(-med))

players <- c("Stephen Curry", "Russell Westbrook", "James Harden",
             "Chris Paul", "Dirk Nowitzki", "Anthony Davis","Kawhi Leonard",
             "LeBron James", "Kevin Durant", "Dwyane Wade")
# players <- c("Giannis Antetokounmpo", "Stephen Curry","Luka Doncic", 
#              
#              "Damian Lillard","Chris Paul", "Bradley Beal", "Jimmy Butler",
#              "Paul George", "Rudy Gobert", "Kyrie Irving", "Anthony Davis", 
#              "James Harden", "Russell Westbrook", "Devin Booker", "Jayson Tatum",
#              "Julius Randle", "Donovan Mitchell", "Trae Young")


p_ranks <- ggplot(data = df_ranks |> 
                    dplyr::filter(player %in% players),
                  aes(x = season, y = rank_med, color = player))
p_ranks + geom_point() +
  geom_line() +
  labs(x = "Season", y = "Rank of Median EPAA per Game") +
  scale_color_brewer("", palette = "Paired") +
  scale_x_continuous(breaks = 2010:2021, minor_breaks = NULL) +
  scale_y_reverse(breaks = seq(0, 100, by = 5)) +
  #  geom_label() +
  theme_bw()

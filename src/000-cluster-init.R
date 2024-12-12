## Ryan Elmore
## Number of clusters


library(factoextra)
shots_data <- readRDS("data/nba-zone-combined-team-w-fts.rds")

location_prob <- shots_data %>% 
  dplyr::select(c(team_season, dplyr::ends_with("_n"))) %>% 
  dplyr::mutate(., n = rowSums(select_if(., is.numeric))) %>% 
  dplyr::mutate_at(., vars(-team_season), ~ ./n) %>% 
  dplyr::select(-c(n, team_season))

df_p <- dplyr::select(shots_data, ends_with("_p"))

set.seed(1983)
# function to compute total within-cluster sum of squares
fviz_nbclust(location_prob, kmeans, method = "wss", k.max = 30) + 
  theme_minimal()

fviz_nbclust(df_p, kmeans, method = "wss", k.max = 30) + 
  theme_minimal()

fviz_nbclust(scale(shots_data[,-1]), kmeans, method = "silhouette", k.max = 30) + 
  theme_minimal()

shots_data_players <- readRDS("data/all-shots-players-w-fts.rds") |> 
  na.omit()

location_prob <- shots_data_players %>% 
  dplyr::ungroup() %>%
  dplyr::select(dplyr::ends_with("_n"), namePlayer)  %>% 
  dplyr::mutate(., n = rowSums(select_if(., is.numeric))) %>% 
  dplyr::mutate_at(., vars(-namePlayer), ~ ./n) %>% 
  dplyr::select(-n, -namePlayer)


df_p <- shots_data_players |> 
  dplyr::ungroup() |> 
  dplyr::select(ends_with("_p"))

set.seed(1983)
# function to compute total within-cluster sum of squares
fviz_nbclust(location_prob, kmeans, method = "wss", k.max = 30) + 
  theme_minimal() + 
  labs(title = "Player clusters (shot location probability)")

fviz_nbclust(df_p, kmeans, method = "wss", k.max = 30) + 
  theme_minimal() + 
  labs(title = "Player clusters (shot making probabilites)")


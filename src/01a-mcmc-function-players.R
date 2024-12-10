## Ryan Elmore
## Bayesian Hierarchical MCMC algorithm 

## Read in Data
shots_data_players <- readRDS("data/all-shots-players-w-fts.rds") |> 
  na.omit()

## Function to fit the model
bayes_hier_mcmc <- function(df_shots, B = 10000, K = 7, n_z = 30, n_w = 30,
                            alpha = 0.1, seed = 1994){

  require(dplyr)
  require(tidyr)
  require(MCMCpack)

  cat(sprintf("Starting the simuation at %s \n", Sys.time()))
  
  ## Initialization
  set.seed(seed)
  alpha_init <- alpha
  p <- q <- pi_z <- pi_w <- Z_pp <- W_pp <- rep(list(NA), B)
  
  ## 1. q (Dirichlet)
  ## 2. p (beta)
  ## 3. pi^z (Dirichlet)
  ## 4. pi^w (Dirichlet)
  ## 5. z (product of binomials)
  ## 6. w (multinomial)
  
  location_prob <- df_shots %>% 
    dplyr::ungroup() %>% 
    dplyr::select(c(yearSeason, slugSeason, namePlayer, dplyr::ends_with("_n"))) %>% 
    dplyr::mutate(., n = rowSums(select_if(., is.numeric))) %>% 
    dplyr::mutate_at(., vars(-yearSeason, -slugSeason, -namePlayer), ~ ./n) %>% 
    dplyr::select(-c(n, yearSeason, slugSeason, namePlayer))
  
  df_N <- df_shots %>% 
    dplyr::ungroup() %>% 
    dplyr::select(., dplyr::ends_with("_n"))
  df_M <- df_shots %>% 
    dplyr::ungroup() %>% 
    dplyr::select(., dplyr::ends_with("_m"))
  
  ## Init. clusterings
  ## 1. Cluster Z's on probabilities
  df_p <- df_shots %>%
    dplyr::ungroup() %>%
    dplyr::select(ends_with("_p"))
  Z <- kmeans(df_p, n_z)$cluster
  ## 2. Cluster W's on shot location probabilities
  W <- kmeans(location_prob, n_w)$cluster
  
  P <- dim(df_shots)[1] # Observations
  Z_mat <- W_mat <- matrix(NA, nr = B, nc = P)
  Z_pp_tmp <- matrix(NA, nr = P, nc = n_z)
  W_pp_tmp <- matrix(NA, nr = P, nc = n_w)
  
  Z_probs <- numeric(n_z)
  W_probs <- numeric(n_w)
  
  for (b in 1:B) {
    #b <- 2
    if(b %% 500 == 0){
      cat(sprintf("Iteration %s at %s \n", b, Sys.time()))
    }
    Z_mat[b, ] <- Z
    W_mat[b, ] <- W
    ## Update q
    for(j in 1:n_w){
      q[[b]][j] <- list(rdirichlet(1, alpha = alpha_init + 
                                     apply(df_N[which(W == j), ], 2, sum)))
    }
    ## Update p
    for(s in 1:n_z){
      tmp <- 1:K
      for(k in 1:K){
        M_total <- sum(df_M[which(Z == s), k])
        N_total <- sum(df_N[which(Z == s), k])
        tmp[k] <- rbeta(1, 1 + M_total, 1 + N_total - M_total)
      }
      p[[b]][s] <- list(tmp)
    }
    ## Update pi_z  
    pi_z[[b]] <- as.vector(rdirichlet(1, alpha = alpha_init + 
                                        sapply(1:n_z, function(x) sum(Z == x))))
    ## Update pi_w  
    pi_w[[b]] <- as.vector(rdirichlet(1, alpha = alpha_init + 
                                        sapply(1:n_w, function(x) sum(W == x))))
    
    for(i in 1:P) {
      #i <- 1
      for(x in 1:n_z){
        #x <- 1
        Z_probs[x] <- prod(dbinom(unlist(df_M[i, ]), 
                                  unlist(df_N[i, ]), 
                                  prob = unlist(p[[b]][x])))*pi_z[[b]][x]
      }
      Z_pp_tmp[i, ] <- Z_probs/sum(Z_probs)
      for(y in 1:n_w){
        W_probs[y] <- dmultinom(unlist(df_N[i, ]), 
                                sum(df_N[i, ]),
                                unlist(q[[b]][y]))*pi_w[[b]][y]
      }
      W_pp_tmp[i, ] <- W_probs/sum(W_probs)
      Z[i] <- which(rmultinom(1, 1, Z_pp_tmp[i, ]) == 1)
      W[i] <- which(rmultinom(1, 1, W_pp_tmp[i, ]) == 1)
    }
    Z_pp[[b]] <- Z_pp_tmp
    W_pp[[b]] <- W_pp_tmp
  }

  parms <- paste(n_z, n_w, alpha, sep = "_")
  saveRDS(Z_pp, paste("data/players/z_pp_", parms, "_", format(Sys.Date(), "%d%m%Y"), ".rds", 
                      sep = ""))
  saveRDS(W_pp, paste("data/players/w_pp_", parms, "_", format(Sys.Date(), "%d%m%Y"), ".rds", 
                      sep = ""))
  saveRDS(pi_z, paste("data/players/pi_z_", parms, "_", format(Sys.Date(), "%d%m%Y"), ".rds", 
                      sep = ""))
  saveRDS(pi_w, paste("data/players/pi_w_", parms, "_", format(Sys.Date(), "%d%m%Y"), ".rds", 
                      sep = ""))
  saveRDS(p, paste("data/players/p_", parms, "_", format(Sys.Date(), "%d%m%Y"), ".rds", 
                   sep = "")) 
  saveRDS(q, paste("data/players/q_", parms, "_", format(Sys.Date(), "%d%m%Y"), ".rds", 
                   sep = ""))

}

bayes_hier_mcmc(shots_data_players, B = 10000, K = 7, n_z = 30, n_w = 30, alpha = 0.1)
bayes_hier_mcmc(shots_data_players, B = 10000, K = 7, n_z = 30, n_w = 30, alpha = 5)
bayes_hier_mcmc(shots_data_players, B = 10000, K = 7, n_z = 10, n_w = 10, alpha = 0.1)
bayes_hier_mcmc(shots_data_players, B = 10000, K = 7, n_z = 10, n_w = 10, alpha = 5)

bayes_hier_mcmc(shots_data_players, B = 10000, K = 7, n_z = 30, n_w = 10, alpha = 0.1)
bayes_hier_mcmc(shots_data_players, B = 10000, K = 7, n_z = 30, n_w = 10, alpha = 5)
bayes_hier_mcmc(shots_data_players, B = 10000, K = 7, n_z = 10, n_w = 30, alpha = 0.1)
bayes_hier_mcmc(shots_data_players, B = 10000, K = 7, n_z = 10, n_w = 30, alpha = 5)
bayes_hier_mcmc(shots_data_players, B = 10000, K = 7, n_z = 20, n_w = 20, alpha = 0.1)
bayes_hier_mcmc(shots_data_players, B = 10000, K = 7, n_z = 20, n_w = 20, alpha = 5)

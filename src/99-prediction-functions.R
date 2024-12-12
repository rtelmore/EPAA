## Ryan Elmore
## A function for predicting a team's points from field goals

predict_fg_points <- function(shots, makes, p, q, pi_w, pi_z, B, K, n_z, n_w){
  require(dplyr)
  require(ggplot2)
  require(tidyr)
  
  N <- sum(shots)
  
  Z_probs <- numeric(n_z)
  W_probs <- numeric(n_w)
  total <- numeric(B)
  
  for (b in 1:B) {
    if(b %% 5000 == 0){
      cat(sprintf("Iteration %s at %s \n", b, Sys.time()))
    }
    for(x in 1:n_z){
      Z_probs[x] <- prod(dbinom(unlist(makes[1, ]), 
                                unlist(shots[1, ]), 
                                prob = unlist(p[[b]][x])))*pi_z[[b]][x]
    }
    Z_pp <- Z_probs/sum(Z_probs)
    
    for(y in 1:n_w){
      W_probs[y] <- dmultinom(unlist(shots[1, ]), 
                              sum(shots[1, ]),
                              unlist(q[[b]][y]))*pi_w[[b]][y]
    }
    W_pp <- W_probs/sum(W_probs)
    
    z_i <- which(rmultinom(1, 1, Z_pp) == 1)
    w_i <- which(rmultinom(1, 1, W_pp) == 1)
    
    ## Use w_i to sample from multinomial 
    n_i <- as.vector(rmultinom(1, N, unlist(q[[b]][w_i])))
    
    m_i <- rep(NA, len = K)
    for(k in 1:K){
      m_i[k] <- rbinom(1, n_i[k], p[[b]][z_i][[1]][k])
    }
    total[b] <- sum(m_i * c(3, 2, 3, 2, 2, 3))
  }
  return(total)
}

predict_points <- function(shots, makes, p, q, pi_w, pi_z, B, K, n_z, n_w){
  require(dplyr)
  require(ggplot2)
  require(tidyr)
  
  N <- sum(shots)
  
  Z_probs <- numeric(n_z)
  W_probs <- numeric(n_w)
  total <- numeric(B)
  
  for (b in 1:B) {
    if(b %% 5000 == 0){
      cat(sprintf("Iteration %s at %s \n", b, Sys.time()))
    }
    for(x in 1:n_z){
      Z_probs[x] <- prod(dbinom(unlist(makes[1, ]), 
                                unlist(shots[1, ]), 
                                prob = unlist(p[[b]][x])))*pi_z[[b]][x]
    }
    Z_pp <- Z_probs/sum(Z_probs)
    
    for(y in 1:n_w){
      W_probs[y] <- dmultinom(unlist(shots[1, ]), 
                              sum(shots[1, ]),
                              unlist(q[[b]][y]))*pi_w[[b]][y]
    }
    W_pp <- W_probs/sum(W_probs)
    
    z_i <- which(rmultinom(1, 1, Z_pp) == 1)
    w_i <- which(rmultinom(1, 1, W_pp) == 1)
    
    ## Use w_i to sample from multinomial 
    n_i <- as.vector(rmultinom(1, N, unlist(q[[b]][w_i])))
    
    m_i <- rep(NA, len = K)
    for(k in 1:K){
      m_i[k] <- rbinom(1, n_i[k], p[[b]][z_i][[1]][k])
    }
    total[b] <- sum(m_i * c(3, 2, 3, 2, 2, 3, 1))
  }
  return(total)
}

predict_points_by_team <- function(index, N, z, w, q, p, B, K, n_z, n_w){
  require(dplyr)
  require(ggplot2)
  require(tidyr)
  
  total <- numeric(B)
  
  for (b in 1:B) {
    if(b %% 5000 == 0){
      cat(sprintf("Iteration %s at %s \n", b, Sys.time()))
    }
    Z_pp <- z[[b]][index, ]
    W_pp <- w[[b]][index, ]
    
    z_i <- which(rmultinom(1, 1, Z_pp) == 1)
    w_i <- which(rmultinom(1, 1, W_pp) == 1)
    
    ## Use w_i to sample from multinomial 
    n_i <- as.vector(rmultinom(1, N, unlist(q[[b]][w_i])))
    
    #K <- 7
    m_i <- rep(NA, len = K)
    for(k in 1:K){
      m_i[k] <- rbinom(1, n_i[k], p[[b]][z_i][[1]][k])
    }
    total[b] <- sum(m_i * c(3, 2, 3, 2, 2, 3, 1))
  }
  return(total)
}

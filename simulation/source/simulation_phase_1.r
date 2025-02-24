###################################
############# Phase 1 #############
###################################

# Run simulation ----------------------------------------------------------
simulation.phase1 <- function(n, local, censored_prop, cor_inp = NULL, scaling = F) {
  # n <- N # 2000 # number of data points
  # local = LOCAL # F
  # censored_prop <- CENPROP # c(0.1, 0.9)
  # cor_inp = CORR #0.2
  pop_info <- genDataInfo(p = 2, cor_inp = cor_inp)
  uncensored_X <- genData(n,pop_info$mu, pop_info$sig2)
  uncensored_info <- getDataInfo(uncensored_X)
  
  if(local) {
    cen.point_bi <- cenPoint(
      x = uncensored_X,
      lower_quant = c(censored_prop[1], censored_prop[1]),
      upper_quant = c(censored_prop[2], censored_prop[2]))
    
  } else {
    cen.point_bi <- cenPoint(
      x = uncensored_X,
      lower_quant = censored_prop[1],
      upper_quant = censored_prop[2])
  }
  
  censored_X <- cenData(x = uncensored_X, cen_point = cen.point_bi)
  censored_info <- getDataInfo(censored_X)
  
  tobit_info <- cMulti(data = censored_X, bounds = cen.point_bi)
  
  uc_info = uncensored_info
  tob_info = tobit_info
  cen_info = censored_info
  
  tibble(
    params = names(unlist(uc_info)),
    uc_info = unlist(uc_info),
    tob_info = unlist(tob_info),
    cen_info = unlist(cen_info)
  ) %>% 
    filter(!params %in% c("sig23","corr1","corr3","corr4"))
}



#--------------------------------------------------------------------------
# Utils -------------------------------------------------------------------
#--------------------------------------------------------------------------
# gen_mvrinfo(5)
genMvrinfo <- function(n=2) {
  # 
  # A <- matrix(runif(n^2)*2-1, ncol=n) 
  # Sigma <- t(A) %*% A
  stopifnot(n == 2)
  
  sig2 <- runif(2, 1, 4)
  mat <- bimat(c(0,0,0,0))
  diag(mat) <- sig2
  # mat[lower.tri(mat)]
  mat[c(2,3)] <- runif(1, -0.8, 0.8) * prod(sqrt(sig2))
  
  list(mu = round(runif(n, 0, 5), 2), sig2 = mat)
}


# gen bi_data -------------------------------------------------------------
genData <- function(n, mu, sig2) {
  MASS::mvrnorm(n=n,mu, sig2, empirical = T)
}

# censor ------------------------------------------------------------------
cenPoint <- function(x, lower_quant, upper_quant) {
  
  if(length(lower_quant)==1 & length(upper_quant)==1) {
    
    x <- as.matrix(x)
    
    cen.point <- c(quantile(x, lower_quant), quantile(x, upper_quant))
    cen.point_bi <- vector("list",ncol(x))
    lapply(cen.point_bi, function(x) x <- cen.point)
  } else {
    
    x <- as.matrix(x)
    
    # lower_quant <- c(lower_quant, lower_quant)
    # upper_quant <- c(upper_quant, upper_quant)
    
    cen.point_bi <- lapply(1:ncol(x), function(i) {
      c(quantile(x[,i], lower_quant[i]), quantile(x[,i], upper_quant[i]))
    })
    cen.point_bi
  }
}

# Censored data------------------------------------------------------------
cenData <- function(x, cen_point) {
  
  if(length(cen_point) == 1) {
    
    x[x[ , 1] <= cen_point[[1]][1] , 1] <- cen_point[[1]][1]
    x[x[ , 1] >= cen_point[[1]][2] , 1] <- cen_point[[1]][2]
    
    censored_x <- x
  } else {
    
    censored_x <- sapply(1:ncol(x), function(ii) {
      
      x[x[ , ii] <= cen_point[[ii]][1] , ii] <- cen_point[[ii]][1]
      x[x[ , ii] >= cen_point[[ii]][2] , ii] <- cen_point[[ii]][2]
      
      x[, ii]
    })
  }
  
  censored_x
}

#--------------------------------------------------------------------------
# Simulation---------------------------------------------------------------
#--------------------------------------------------------------------------
# gen data info -----------------------------------------------------------
# genDataInfo()
genDataInfo <- function(p = 2, cor_inp = NULL) {
  
  mu <- round(runif(p, -5, -1), 2)
  vars <- round(runif(p, 0.5, 4), 2)
  
  if(is.null(cor_inp)) {
    cor.xy <- sample(c(-1, 1),1)*round(runif(1, 0.1, 0.8), 2)
  } else {
    cor.xy = cor_inp
  }

  cov.xy <- cor.xy * (sqrt(vars[1])*sqrt(vars[2]))
  
  sig2 <- bimat(c(vars[1], cov.xy, cov.xy, vars[2]))
  corr <- cov2cor(sig2)
  
  list(mu = mu, sig2 = sig2, corr = corr)
  
}
# bivariate ---------------------------------------------------------------
ll_censored_bi <- function(theta, XY, bounds, fixed = NULL) {
  
  # theta = startv ; # c(rho_xy)
  # fixed = c(mu_x = 1, mu_y = 2, sig_x = 1, sig_y = 1)
  # rho_xy = unknown
  # XY = N x 2 matrix
  # XY = matrix(runif(100,-10, 10), ncol = 2)
  # bounds = cen.point
  
  # Benning & Gange (2015). Methods for comparing correlations involving left-censored laboratory data
  # dnorm() = PDF ;  pnorm() = CDF
  # dmvnorm / pmvnorm
  
  # param
  # rho <- theta[1]
  
  # Given data --------------------------------------------------------------
  X <- XY[,1]
  Y <- XY[,2]  
  
  # Bounds ----------------------------------------------------------------
  # if(length(bounds) == 2) {
    # lower_x <- bounds[[1]][1]
    # upper_x <- bounds[[1]][2]
    
    lower_x <- ifelse(is.na(bounds[[1]][1]), -Inf, bounds[[1]][1])
    upper_x <- ifelse(is.na(bounds[[1]][2]), Inf, bounds[[1]][2])
    
    # lower_y <- bounds[[2]][1]
    # upper_y <- bounds[[2]][2]
    
    lower_y <- ifelse(is.na(bounds[[2]][1]), -Inf, bounds[[2]][1])
    upper_y <- ifelse(is.na(bounds[[2]][2]), Inf, bounds[[2]][2])
    
  # } else {
  #   lower_x <- bounds[[1]][1]
  #   upper_x <- bounds[[1]][2]
  #   
  #   lower_y <- -Inf
  #   upper_y <- Inf
  # }
  
  
  # Separate sample ---------------------------------------------------------
  ## n1. Both uncensored
  n1 <-XY[
    (XY[,1] > lower_x & XY[,1] < upper_x) & 
      (XY[,2] > lower_y & XY[,2] < upper_y),
  ]
  
  n1 <- matrix(n1, ncol = 2)
  n_n1 <- nrow(n1)
  
  ## n2. X < CL & Y ><
  n2 <-XY[
    (XY[,1] <= lower_x) & (XY[,2] > lower_y & XY[,2] < upper_y),
  ]
  
  n2 <- matrix(n2, ncol = 2)
  n_n2 <- nrow(n2)
  
  ## n3. X > CU & Y ><
  n3 <-XY[
    (XY[,1] >= upper_x) & (XY[,2] > lower_y & XY[,2] < upper_y),
  ]
  
  n3 <- matrix(n3, ncol = 2)
  n_n3 <- nrow(n3)
  
  ## n4. X >< & Y < CL
  n4 <-XY[
    (XY[,1] > lower_x & XY[,1] < upper_x) & (XY[,2] <= lower_y),
  ]
  
  n4 <- matrix(n4, ncol = 2)
  n_n4 <- nrow(n4)
  
  ## n5. X >< & Y > CU
  n5 <- XY[
    (XY[,1] > lower_x & XY[,1] < upper_x) & (XY[,2] >= upper_y),
  ]
  
  n5 <- matrix(n5, ncol = 2)
  n_n5 <- nrow(n5)
  
  ## n6. X > CU & Y > CU
  n6 <-XY[
    (XY[,1] >= upper_x) & (XY[,2] >= upper_y),
  ]
  
  n6 <- matrix(n6, ncol = 2)
  n_n6 <- nrow(n6)
  
  ## n7. X < CL & Y > CU
  n7 <-XY[
    (XY[,1] <= lower_x) & (XY[,2] >= upper_y),
  ]
  
  n7 <- matrix(n7, ncol = 2)
  n_n7 <- nrow(n7)
  
  ## n8. X > CU & Y < CL
  n8 <-XY[
    (XY[,1] >= upper_x) & (XY[,2] <= lower_y),
  ]
  
  n8 <- matrix(n8, ncol = 2)
  n_n8 <- nrow(n8)
  
  ## n9. X < CL & Y < CL
  n9 <-XY[
    (XY[,1] <= lower_x) & (XY[,2] <= lower_y),
  ]
  
  n9 <- matrix(n9, ncol = 2)
  n_n9 <- nrow(n9)
  
  # Fixed param -------------------------------------------------------------
  if(!is.null(fixed)) {
    
    Mu_x <- fixed["mu1"]
    Mu_y <- fixed["mu2"]
    Sig2_x <- fixed["sig21"]
    Sig2_y <- fixed["sig22"]
    
    Sig_x <- sqrt(Sig2_x)
    Sig_y <- sqrt(Sig2_y)
  } else {
    
    Mu_x <- theta[2]
    Mu_y <- theta[3]
    Sig2_x <- theta[4]
    Sig2_y <- theta[5]
    
    Sig_x <- sqrt(Sig2_x)
    Sig_y <- sqrt(Sig2_y)
  }
  
  # Parameter --------------------------------------------------------------
  # theta <- startv
  rho_xy <- theta[1]
  cov_xy <- rho_xy * (Sig_x*Sig_y)
  
  given_mu <- c(Mu_x, Mu_y)
  given_sigma <- matrix(c(Sig2_x, cov_xy, cov_xy, Sig2_y), ncol = 2)
  
  # Transformed parameters --------------------------------------------------
  # Mu_xy = Mu_x + (rho_xy*sig_x*(Y - Mu_y)) / Simg_y
  # Sig_xy = Sig_x * sqrt(1-rho_xy^2)
  # 
  # Mu_yx = Mu_y + (rho_xy*sig_y*(X - Mu_x)) / Simg_x
  # Sig_yx = Sig_y * sqrt(1-rho_xy^2)
  
  # calculation Blocks ------------------------------------------------------
  ## n1. X >< & Y >< ---------------------
  a1 <- sum(dmvnorm(n1, 
            mean = given_mu, 
            sigma = given_sigma, 
            log = T, 
            checkSymmetry = TRUE))
  
  ## n2. X < CL & Y >< ---------------------
  Y <- n2[,2]
  
  if(length(Y) == 0) {
    ll_n2 <- 0
  } else {
    Mu_xy = Mu_x + (rho_xy*Sig_x*(Y - Mu_y)) / Sig_y
    Sig_xy = Sig_x * sqrt(1-rho_xy^2)
    X_lower <- (lower_x - Mu_xy) / Sig_xy
    # X_lower <- trans_cp(Y, Mu_x, Mu_y, Sig_x, Sig_y, rho_xy, lower_x)
    # sum(log(pnorm(lower_x, Mu_xy, Sig_xy)))
    
    ll_n2 <- sum(log(pnorm(X_lower))) + ll_fun(Y, Mu_y, Sig_y)
  }
  a2 <- ll_n2
  
  ## n3. X > CU & Y >< ---------------------
  Y <- n3[,2]
  
  if(length(Y) == 0) {
    ll_n3 <- 0
  } else {
    Mu_xy = Mu_x + (rho_xy*Sig_x*(Y - Mu_y)) / Sig_y
    Sig_xy = Sig_x * sqrt(1-rho_xy^2)
    X_upper <- (upper_x - Mu_xy) / Sig_xy
    # X_upper <- trans_cp(Y, Mu_x, Mu_y, Sig_x, Sig_y, rho_xy,upper_x)
    # sum(log(1-pnorm(upper_x, Mu_xy, Sig_xy)))
    
    ll_n3 <- sum(log(1-pnorm(X_upper))) + ll_fun(Y, Mu_y, Sig_y)
  }
  a3 <- ll_n3
  
  ## n4. X >< & Y < CL ---------------------
  X <- n4[,1]
  
  if(length(X) == 0) {
    ll_n4 <- 0
  } else {
    Mu_yx = Mu_y + (rho_xy*Sig_y*(X - Mu_x)) / Sig_x
    Sig_yx = Sig_y * sqrt(1-rho_xy^2)
    Y_lower <- (lower_y - Mu_yx) / Sig_yx
    # Y_lower <- trans_cp(X, Mu_y, Mu_x, Sig_y, Sig_x, rho_xy,lower_y)
    # sum(log(pnorm(lower_y, Mu_yx, Sig_yx)))
    
    ll_n4 <- ll_fun(X, Mu_x, Sig_x) + sum(log(pnorm(Y_lower)))
  }
  a4 <- ll_n4
  
  ## X >< & Y > CU ---------------------
  X <- n5[,1]
  
  if(length(X) == 0) {
    ll_n5 <- 0
  } else {
    Mu_yx = Mu_y + (rho_xy*Sig_y*(X - Mu_x)) / Sig_x
    Sig_yx = Sig_y * sqrt(1-rho_xy^2)
    Y_upper <- (upper_y - Mu_yx) / Sig_yx
    # Y_upper <- trans_cp(X, Mu_y, Mu_x, Sig_y, Sig_x, rho_xy, upper_y)
    # sum(log(1-pnorm(upper_y, Mu_yx, Sig_yx)))
    
    ll_n5 <- ll_fun(X, Mu_x, Sig_x) + sum(log(1-pnorm(Y_upper)))
  }
  a5 <- ll_n5
  
  # Below is just a proportion value
  ## X > CU & Y > CU ---------------------
  Y <- n6[,2]
  
  if(length(Y) == 0) {
    a6 = 0
  } else {
    a6 <- mvtnorm::pmvnorm(
      lower = c(upper_x, upper_y), 
      upper = c(Inf, Inf), 
      given_mu, 
      sigma = given_sigma)
    
    a6 <- ifelse(is.nan(log(a6)), 0, n_n6*log(a6))
    
    # a6 <- n_n6*log(a6)
  }
  ## X < CL & Y > CU ---------------------
  Y <- n7[,2]
  
  if(length(Y) == 0) {
    a7 = 0
  } else {
    a7 <- mvtnorm::pmvnorm(
      lower = c(-Inf, upper_y), 
      upper = c(lower_x, Inf), 
      given_mu, 
      sigma = given_sigma)
    
    a7 <- ifelse(is.nan(log(a7)), 0, n_n7*log(a7))
    
    # a7 <- n_n7*log(a7)
  }
  
  ## X > CU & Y < CL ---------------------
  Y <- n8[,2]
  
  if(length(Y) == 0) {
    a8 = 0
  } else {
    
    a8 <- mvtnorm::pmvnorm(
      lower = c(upper_x, -Inf), 
      upper = c(Inf, lower_y), 
      given_mu, 
      sigma = given_sigma)
    
    a8 <- ifelse(is.nan(log(a8)), 0, n_n8*log(a8))
    
    # a8 <- n_n8*log(a8) 
  }
  
  ## X < CL & Y < CL ---------------------
  Y <- n9[,2]
  
  if(length(Y) == 0) {
    a9 = 0
  } else {

    a9 <- mvtnorm::pmvnorm(
      lower = c(-Inf, -Inf), 
      upper = c(lower_x, lower_y), 
      given_mu, 
      sigma = given_sigma)
    
    a9 <- ifelse(is.nan(log(a9)), 0, n_n9*log(a9))
    
    # a9 <- n_n9*log(a9)
    
  }
  # -------------------------------------------------------------------------
  ll <- -(a1 + a2 + a3 + a4 + a5 + a6 + a7 + a8 + a9)
  # ll <- ll* 1 / (1 - abs(theta[1]))
  
  return(ll)
}
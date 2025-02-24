#--------------------------------------------------------------------------
# Estimate ----------------------------------------------------------------
#--------------------------------------------------------------------------
# log-lik --------------------
ll_fun <- function(Y, MU, SIG) {
  # -.5*N*log(2*pi) -.5*N*log(SIG) - (1/(2*SIG))*sum((Y - MU)**2)
  sum(dnorm(Y, MU, SIG, log = T))
}
# censored univariate --------------------
cUniv <- function(startv, data, bounds) {
  
  # o <- optim(par = startv,
  #            fn = ll_censored, 
  #            X = data, 
  #            bounds = bounds#,
  #            # method = "BFGS"
  #            # method = "L-BFGS-B",
  #            # lower=c(-Inf, 0),
  #            # upper=c(Inf, Inf)
  # )
  
  data <- data[!is.na(data)]
  
  o <- nlminb(start = startv,
              objective = ll_censored,
              X = data,
              bounds = bounds,
              lower=c(-Inf, 0),
              upper=c(Inf, Inf)
  )
  
  o$par
  
}
# censored bivariate --------------------
cBiv <- function(startv, data, bounds, fixed = NULL, scaling = F) {
  
  if(!is.null(fixed)) {
    # starts : correlation
    
    # o <- optim(par = startv,
    #            fn = ll_censored_bi, 
    #            XY = data, 
    #            bounds = bounds,
    #            fixed = fixed#,
    #            # method = "L-BFGS-B",
    #            # lower=c(-Inf),
    #            # upper=c(0.99)
    # )
    
    data <- data[complete.cases(data), ]
    
    o <- nlminb(start = startv,
                objective = ll_censored_bi,
                XY = data,
                bounds = bounds,
                fixed = fixed,
                lower = -0.99, 
                upper = 0.99
    )

    out <- o$par

    if(scaling) {
      
      # no need for Scaling  !!!!
      out <- out
      # out <- out*(0.2*abs(out) + 0.8)
      
      
      
    }

    out <- out * (sqrt(fixed[3]) * sqrt(fixed[4]))
    out <- list(
      mu = c(fixed[1],
             fixed[2]),
      sig2 = bimat(c(fixed[3], out, out, fixed[4]))
    )

  } else {
    # starts : Cov, Mu, Sig2
    
    # o <- optim(par = startv,
    #       fn = ll_censored_bi, 
    #       XY = data, 
    #       bounds = bounds,
    #       fixed = NULL#,
    #       # method = "L-BFGS-B",
    #       # lower = c(-1, -Inf, -Inf, 0,0),
    #       # upper = c(1, Inf, Inf, Inf, Inf)
    # )
    
    # nlminb(start = 
    # c(censored_info$sig2[2],censored_info$mu,censored_info$sig2[c(1,4)]), 
    #        objective = ll_censored_bi,
    #        XY = data, 
    #        bounds = cen.point_bi,
    #        fixed = NULL,
    #        lower = c(-1, -Inf,-Inf, 0, 0), upper = c(1, Inf,Inf, Inf, Inf)
    # )
    
    # out <- o$par
    # out <- list(mu = out[2:3],
    #      sig2 = bimat(out[c(4,1,1,5)]))
  }
  
  out
}

# vector to bivariate-matrix ----------------------------------------------
bimat <- function(x) {
  matrix(x, ncol=2)
}

# get dist info -----------------------------------------------------------
getDataInfo<- function(x) {
  list(mu = colMeans(x, na.rm = T), 
       sig2 = cov(x, use='pairwise'), 
       corr = cor(x, use='pairwise'))
}


# -------------------------------------------------------------------------

breakSample <- function(XY, lower_x, upper_x, lower_y, upper_y) {
  
  # Separate sample ---------------------------------------------------------
  ## n1. Both uncensored
  X_mid_Y_mid <-XY[
    (XY[,1] > lower_x & XY[,1] < upper_x) & (XY[,2] > lower_y & XY[,2] < upper_y),
  ]
  
  X_mid_Y_mid <- matrix(X_mid_Y_mid, ncol = 2)
  n_X_mid_Y_mid <- nrow(X_mid_Y_mid)
  
  ## n2. X < CL & Y ><
  X_cl_Y_mid <-XY[
    (XY[,1] <= lower_x) & (XY[,2] > lower_y & XY[,2] < upper_y),
  ]
  
  X_cl_Y_mid <- matrix(X_cl_Y_mid, ncol = 2)
  n_X_cl_Y_mid <- nrow(X_cl_Y_mid)
  
  ## n3. X >< & Y < CL
  X_mid_Y_cl <-XY[
    (XY[,1] > lower_x & XY[,1] < upper_x) & (XY[,2] <= lower_y),
  ]
  
  X_mid_Y_cl <- matrix(X_mid_Y_cl, ncol = 2)
  n_X_mid_Y_cl <- nrow(X_mid_Y_cl)
  
  ## n4. X > CU & Y ><
  X_cu_Y_mid <-XY[
    (XY[,1] >= upper_x) & (XY[,2] > lower_y & XY[,2] < upper_y),
  ]
  
  X_cu_Y_mid <- matrix(X_cu_Y_mid, ncol = 2)
  n_X_cu_Y_mid <- nrow(X_cu_Y_mid)
  
  ## n5. X >< & Y > CU
  X_mid_Y_cu <- XY[
    (XY[,1] > lower_x & XY[,1] < upper_x) & (XY[,2] >= upper_y),
  ]
  
  X_mid_Y_cu <- matrix(X_mid_Y_cu, ncol = 2)
  n_X_mid_Y_cu <- nrow(X_mid_Y_cu)
  
  ## n6. X > CU & Y > CU
  X_cu_Y_cu <-XY[
    (XY[,1] >= upper_x) & (XY[,2] >= upper_y),
  ]
  
  X_cu_Y_cu <- matrix(X_cu_Y_cu, ncol = 2)
  n_X_cu_Y_cu <- nrow(X_cu_Y_cu)
  
  ## n7. X < CL & Y > CU
  X_cl_Y_cu <-XY[
    (XY[,1] <= lower_x) & (XY[,2] >= upper_y),
  ]
  
  X_cl_Y_cu <- matrix(X_cl_Y_cu, ncol = 2)
  n_X_cl_Y_cu <- nrow(X_cl_Y_cu)
  
  ## n8. X > CU & Y < CL
  X_cu_Y_cl <-XY[
    (XY[,1] >= upper_x) & (XY[,2] <= lower_y),
  ]
  
  X_cu_Y_cl <- matrix(X_cu_Y_cl, ncol = 2)
  n_X_cu_Y_cl <- nrow(X_cu_Y_cl)
  
  ## n9. X < CL & Y < CL
  X_cl_Y_cl <-XY[
    (XY[,1] <= lower_x) & (XY[,2] <= lower_y),
  ]
  
  X_cl_Y_cl <- matrix(X_cl_Y_cl, ncol = 2)
  n_X_cl_Y_cl <- nrow(X_cl_Y_cl)
}

# ----------------
trans_cp <- function(Y, Mu_x, Mu_y, Sig_x, Sig_y, rho_xy, upper_x) {
  Mu_xy = Mu_x + (rho_xy*Sig_x*(Y - Mu_y)) / Sig_y
  Sig_xy = Sig_x^2 * sqrt(1-rho_xy^2)
  Sig_xy = sqrt(Sig_xy)
  X_upper <- (upper_x - Mu_xy) / Sig_xy
  
  X_upper
}




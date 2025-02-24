# Univariate censored ---------------------
ll_censored <- function(theta, X, bounds) {
  
  # theta = par
  # bounds = cen.point
  
  # Censored MLE: book equation 3.3.1
  # dnorm = PDF ;  pnorm = CDF
  # L = {cl * F(lower)} * {cu * (1-F(upper))} * {n * NORMAL}
  
  lower <- ifelse(is.na(bounds[1]), -Inf, bounds[1])
  upper <- ifelse(is.na(bounds[2]), Inf, bounds[2])
  
  n_lower <- length(X[X <= lower])
  n_upper <- length(X[X >= upper])
  
  X_mid <- X[X > lower & X < upper]
  n_mid <- length(X_mid)
  
  Mu <- theta[1]
  Sigma2 <- theta[2]
  Sigma <- sqrt(Sigma2)
  
  X_lower <- (lower - Mu) / Sigma
  X_upper <- (upper - Mu) / Sigma
  # X_mid   <- (X_mid - Mu) / Sigma 
  
  ll_lower <- n_lower * log(pnorm(X_lower))
  ll_lower <- ifelse(is.nan(ll_lower), 0, ll_lower)
  ll_upper <- n_upper * log(1 - pnorm(X_upper))
  ll_upper <- ifelse(is.nan(ll_upper), 0, ll_upper)
  
  ll_mid <- ll_fun(X_mid, Mu, Sigma)
  
  ll <- -(ll_mid + ll_lower + ll_upper)
  return(ll)
}
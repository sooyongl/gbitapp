if(FALSE) {
  
  # https://en.wikipedia.org/wiki/Truncated_normal_distribution
  
  # notation
  X_lower <- (lower - Mu) / Sigma
  X_mid   <- (X - Mu) / Sigma 
  X_upper <- (upper - Mu) / Sigma
  Z <- pnorm(X_upper) - pnorm(X_lower)
  
  # PDF
  dnorm(X_mid) / (Sigma * Z)
  
  # CDF
  (pnorm(X_mid) - pnorm(X_lower)) / Z
  
  # Mean
  Mu + ((dnorm(X_lower) - dnorm(X_upper))/Z) * Sigma
  
  # Variance
  Sigma^2 * (
    1 + ((X_lower*dnorm(X_lower) - X_upper*dnorm(X_upper))/Z) -
      ((dnorm(X_lower) - dnorm(X_upper))/Z)^2
  )
  
  
  
  
}


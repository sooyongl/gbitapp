cMulti <- function(data, bounds) {
  
  # bounds = list(c(2,5),c(2,5),c(2,5),c(NA,NA),c(NA,NA))
  
  data0 <- as.matrix(data)
  n <- nrow(data0)
  p <- ncol(data0)
  
  stopifnot(length(bounds) == p)
  
  data_info <- getDataInfo(data0)
  
  # univariate estimate -----------------------------------------------------
  # pi <- 1
  mu_sig2 <- vector("list", p)
  for(pi in 1:p) {
    if(all(is.na(bounds[[pi]]))) {
      
      mu_sig2[[pi]] <- c(data_info$mu[pi], diag(data_info$sig2)[pi])
    } else {
      mu_sig2[[pi]] <- 
        cUniv(startv = c(data_info$mu[pi],diag(data_info$sig2)[pi]),
              data = data0[,pi],
              bounds = bounds[[pi]])
    }
    
  }
  
  # covariate estimate ------------------------------------------------------
  # cBiv(c(censored_info$sig2[2],censored_info$mu,censored_info$sig2[c(1,4)]),
  #      censored_X,
  #      bounds = cen.point_bi)
  covm <- matrix(rep(0, p*p), ncol = p)
  
  for(i in 1:(p-1)) {
    for(j in (i+1):p) {
      # print(i)
      # print(j)
      # i = 1; j = 3
      
      if(all(is.na(bounds[[i]])) & all(is.na(bounds[[j]]))) {
        
        covm[i, j] <- data_info$sig2[i, j]
        
      } else {
        
        x1 <- mu_sig2[[i]]
        x1 <- unname(x1)
        y1 <- mu_sig2[[j]]
        y1 <- unname(y1)
        
        cen.point_ <- bounds[c(i,j)]
        # selected <- !unlist(lapply(cen.point_, function(x) all(is.na(x))))
        # cen.point_ <- cen.point_[selected]
        
        cov_xy <- cBiv(
          startv = data_info$corr[i, j],
          data = data0[,c(i,j)],
          bounds = cen.point_,
          fixed = c(mu1 = x1[1], 
                    mu2 = y1[1], 
                    sig21 = x1[2], 
                    sig22 = y1[2]),
          scaling = F)
        
        
        covm[i, j] <- cov_xy$sig2[3]
        
      }
    }
  }
  
  Muv <- unlist(lapply(mu_sig2, function(x) x[1]))
  sig2v <- unlist(lapply(mu_sig2, function(x) x[2]))
  
  diag(covm) <- sig2v
  covm[lower.tri(covm)] <- t(covm)[lower.tri(covm)]
  
  corr <- cov2cor(covm)
  
  list(Mu = Muv, Sig2 = covm, corr = corr)
}

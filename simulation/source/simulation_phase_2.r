###################################
############# Phase 2 #############
###################################

# s3classmethod <- function(model_info, ...){
#   UseMethod("s3classmethod", model_info)
# }

# Run Function ------------------------------------------------------------
simulation.phase2 <- function(model_info) {
  # data generation ---------------------------------------------
  model_info <- SEM.DataGen(model_info = model_info)
  
  # censoring ------------------------------------------------------
  model_info <- cenLGM(model_info)
  
  # estimation -----------------------------------------------------
  model_info$tobit_info <- cMulti(data = model_info$censored_X, 
                                  bounds = model_info$bounds0)
  
  model_info <- genModels(model_info, model = 'quad', covariates = F)
  ori.fit.qaud <- runLGM(model_info, estimator = "original")
  cen.fit.qaud <- runLGM(model_info, estimator = "censored")
  tob.fit.qaud <- runLGM(model_info, estimator = "tobit")
  
  model_info <- genModels(model_info, model = 'linear', covariates = F)
  ori.fit <- runLGM(model_info, estimator = "original")
  cen.fit <- runLGM(model_info, estimator = "censored")
  tob.fit <- runLGM(model_info, estimator = "tobit")
  
  model_info <- genModels(model_info, model = 'linear', covariates = T)
  ori.fit.cov <- runLGM(model_info, estimator = "original", "ML")
  cen.fit.cov <- runLGM(model_info, estimator = "censored", "ML")
  # summary(cen.fit.cov)
  # Warning message:
  # In lavaan::lavaan(model = model_syn, sample.cov = input_cov, sample.mean = input_mu,  :
  #                     lavaan WARNING:
  #                     the optimizer warns that a solution has NOT been found!
  # Warning message:
  #   In lavaan::lavaan(model = model_syn, sample.cov = input_cov, sample.mean = input_mu,  :
  #                       lavaan WARNING:
  #                       the optimizer (NLMINB) claimed the model converged, but not all
  #                     elements of the gradient are (near) zero; the optimizer may not
  #                     have found a local solution use check.gradient = FALSE to skip
  #                     this check.
  tob.fit.cov <- runLGM(model_info, estimator = "tobit")
  
  res_fit <- bind_rows(
    chiDiffTest(ori.fit, ori.fit.qaud) %>% mutate(estimator = "origin"),
    chiDiffTest(cen.fit, cen.fit.qaud) %>% mutate(estimator = "cen"),
    chiDiffTest(tob.fit, tob.fit.qaud) %>% mutate(estimator = "gbit")
  )
  
  res <- extFitInfo(ori.fit, "params") %>% 
    mutate(estimator = "population",.before=lhs) %>% 
    bind_rows(
      extFitInfo(cen.fit, "params") %>% 
        mutate(estimator = "censored",.before=lhs),
      extFitInfo(tob.fit, "params") %>% 
        mutate(estimator = "gbit",.before=lhs)
    ) %>% 
    mutate(path = paste0(lhs, op, rhs), .before = lhs)
  
  res.cov <- extFitInfo(ori.fit.cov, "params") %>% 
    mutate(estimator = "population",.before=lhs) %>% 
    bind_rows(
      extFitInfo(cen.fit.cov, "params") %>% 
        mutate(estimator = "censored",.before=lhs),
      extFitInfo(tob.fit.cov, "params") %>% 
        mutate(estimator = "gbit",.before=lhs)
    ) %>% 
    mutate(path = paste0(lhs, op, rhs, ".cov"), .before = lhs) %>% 
    select(estimator, path, est) %>% 
    filter(!str_detect(path, "^y"))
  
  res_estimates <- res %>% select(estimator, path, est)
  res_estimates <- res_estimates %>% 
    filter(!str_detect(path, "^y")) %>% 
    bind_rows(res.cov) %>% 
    pivot_wider(names_from = estimator, values_from = est)
  
  list(res_estimates = res_estimates, res_fit = res_fit)
}


# Extract information -----------------------------------------------------
extFitInfo <- function(fit, type = "inspect") {
  
  if(type == "inspect") {
    lavInspect(fit, what = 'est')
  }
  
  if(type == "params") {
    parameterestimates(fit) %>% 
      filter(!is.na(pvalue))
  }
  
}


# Fit compare --------------------------------------------------------------
chiDiffTest <- function(lavfit1, lavfit2) {
  
  fit1_info <- fitmeasures(lavfit1)[c("npar","chisq", "df")]
  fit2_info <- fitmeasures(lavfit2)[c("npar","chisq", "df")]
  
  fitinfo <- fit1_info - fit2_info
  
  cri_val <- qchisq(0.95, fitinfo["df"], lower.tail=TRUE)
  
  selected <- ifelse(fitinfo["chisq"] > cri_val, "quad", "linear")
  
  tibble(lmodel = list(fit1_info), qmodel = list(fit2_info), 
         selected = selected)
}


# Make model data ---------------------------------------------------------

inpMat <- function(infomat, ncol = 1) {
  matrix(infomat, ncol = ncol, byrow = T)
}

makeLamMat <- function(timepoints = 4, ncov = 2) {
  
  numrow <- timepoints + ncov
  numcol <- 2 + ncov
  
  mat <- matrix(0, nrow = numrow, ncol = numcol)
  mat[1:timepoints, 1:2] <- cbind(rep(1, timepoints),
                                  0:(timepoints-1))
  if(ncov > 0) 
    mat[(timepoints+1):numrow, 3:numcol] <- diag(ncov)
  
  mat
}

makeThetaMat <- function(LAMBDA, PSI, BETA, ICC = 0.6) {
  
  # THETA <- makeThetaMat(LAMBDA, PSI, BETA, ICC = 0.6)
  numrow <- nrow(LAMBDA)
  timepoints <- nrow(LAMBDA) - 2
  
  I <- diag(nrow(PSI))
  I.BETA <- I-BETA
  IB.inv <- solve(I.BETA)
  LAMBDA..IB.inv <- LAMBDA %*% IB.inv

  explained_var <- (LAMBDA %*% IB.inv) %*% PSI %*% t(LAMBDA %*% IB.inv)
  explained_var <- diag(explained_var)[1:ntimepoint.input] * (1 - ICC) / (ICC)

  mat <- diag(0,numrow)
  mat[1:timepoints, 1:timepoints] <- diag(explained_var)
  
  
  # mat <- diag(0,numrow)
  # mat[1:timepoints, 1:timepoints] <- diag(1, timepoints)
  mat
}

makePsiMat <- function(vGrowth=c(vI=1, vS=0.5, cIS=0.2), vCovar=c(1,1)) {
  
  ncov <- length(vCovar)
  numcol <- 2 + ncov
  mat <- diag(numcol)
  
  mat[1,1] <- vGrowth[1];
  mat[2,2] <- vGrowth[2];
  mat[1,2] <- mat[2,1] <- vGrowth[3];
  mat
}

makeBetaMat <- function(x.eff, growth.eff) {
  
  numcol <- length(x.eff) + length(growth.eff)
  mat <- diag(0,numcol)
  
  mat[3,1:2] <- growth.eff
  mat[1:2,4] <- x.eff
  mat
}
makeNuMat <- function(timepoints, ncov) {
  inpMat(rep(0, timepoints+ncov), ncol = 1)
}

makeAlphaMat <- function(mGrowth, mCovar) {
  inpMat(c(mGrowth,mCovar), ncol = 1)
}

mkModelInfo <- function(N, LAMBDA, PSI, BETA, NU, ALPHA, ICC, pop_cenprop, local) {
  
  model_info <- list()
  model_info$N <- N
  
  model_info$pop_info$LAMBDA <- LAMBDA
  model_info$pop_info$PSI <- PSI
  model_info$pop_info$BETA <- BETA
  model_info$pop_info$NU <- NU
  model_info$pop_info$ALPHA <- ALPHA
  
  model_info$pop_info$THETA <- makeThetaMat(LAMBDA, PSI, BETA, ICC = ICC)
  
  model_info$pop_cenprop <- pop_cenprop
  model_info$local = local
  # model_info <- append(class(model_info), "phase2")
  return(model_info)
}


# Compute Population Mu and Cov based on parameters -----------------------
ComputeCovMu <- function(LAMBDA, THETA, PSI, BETA, NU, ALPHA) {
  nvar <- nrow(LAMBDA)
  nr <- nrow(PSI)
  
  mBETA <- -BETA
  diag(mBETA) <- 1  # tmp[1L + (seq_len(nr) - 1L) * (nr + 1L)] <- 1
  IB.inv <- solve(mBETA)
  LAMBDA..IB.inv <- LAMBDA %*% IB.inv
  
  # (LAMBDA %*% IB.inv) %*% PSI %*% t(LAMBDA %*% IB.inv) + THETA
  COV.hat <- tcrossprod(LAMBDA..IB.inv %*% PSI, LAMBDA..IB.inv) + THETA
  # (LAMBDA %*% IB.inv) %*% ALPHA
  MU.hat <- NU + LAMBDA..IB.inv %*% ALPHA
  
  list(COV.hat = COV.hat, MU.hat = MU.hat)
}



# LGM Data Generation -----------------------------------------------------
SEM.DataGen <- function(model_info) {
  
  N <- model_info$N
  LAMBDA <- model_info$pop_info$LAMBDA
  THETA <- model_info$pop_info$THETA
  PSI <- model_info$pop_info$PSI
  BETA <- model_info$pop_info$BETA
  NU <- model_info$pop_info$NU
  ALPHA <- model_info$pop_info$ALPHA
  
  # fit <- lavaan::growth(model = "
  # I =~ 1*v1+1*v2+1*v3+1*v4
  # S =~ 0*v1+1*v2+2*v3+3*v4
  # I ~~ 1*I
  # S ~~ 0.5*S
  # I ~~ 0.2*S
  # I ~ 3*1
  # S ~ 0.5*1
  # I ~ 0.5*x1
  # S ~ 0.2*x1
  # z1 ~ -0.2*I + 0.1*S
  # x1 ~~ 1*x1 + 0.1*z1
  # z1 ~~ 1*z1
  # x1 ~ 0*1
  # z1 ~ 0*1")
  # model_info <- lavaan::lavInspect(fit, what = "est")
  # Sigma.hat <- lavaan:::computeSigmaHat(lavmodel = fit@Model)[[1]]
  # Mu.hat <- lavaan:::computeMuHat(lavmodel = fit@Model)[[1]]
  
  cov_info <- ComputeCovMu(LAMBDA, THETA, PSI, BETA, NU, ALPHA)
  
  data <- MASS::mvrnorm(n = N, 
                        mu = cov_info$MU.hat,
                        Sigma = cov_info$COV.hat,
                        empirical = F)
  data <- data.frame(data)
  nr <- nrow(LAMBDA) - 2
  names(data)[1:nr] <- paste0("y",1:nr)
  names(data)[(nr+1):ncol(data)] <- c("z1","x1")
  
  model_info$uncensored_X <- data
  model_info$uncensored_info <- cov_info
  
  model_info
}


# censoring ---------------------------------------------------------------

cenLGM <- function(model_info) {
  
  target <- model_info$uncensored_X %>% select(starts_with("y"))
  if(model_info$local) {
    # local -
    censored_prop <- list(rep(model_info$pop_cenprop["left"], ncol(target)),
                          rep(model_info$pop_cenprop["right"], ncol(target)))
    model_info$cen.point_ <- cenPoint(x = target,
                                      lower_quant = censored_prop[[1]],
                                      upper_quant = censored_prop[[2]])
  } else {
    # global -
    censored_prop <- c(model_info$pop_cenprop['left'], 
                       model_info$pop_cenprop['right'])
    model_info$cen.point_ <- cenPoint(x = target,
                                      lower_quant = censored_prop[1],
                                      upper_quant = censored_prop[2])
  }
  
  model_info$censored_X <- cenData(x = target, 
                                   cen_point = model_info$cen.point_) %>% 
    data.frame() %>% 
    set_names(names(target))
  
  min_v <- apply(model_info$censored_X, 2, min)
  min_v <- unname(min_v)
  max_v <- apply(model_info$censored_X, 2, max)
  max_v <- unname(max_v)
  
  for(i in 1:length(model_info$cen.point_)) {
    model_info$cen.point_[[i]][1] <- 
      ifelse(model_info$cen.point_[[i]][1] < min_v[i], 
             NA,
             model_info$cen.point_[[i]][1])
    
    model_info$cen.point_[[i]][2] <- 
      ifelse(model_info$cen.point_[[i]][2] > max_v[i], 
             NA,
             model_info$cen.point_[[i]][2])
  }
  
  model_info$censored_X <- model_info$censored_X %>% 
    bind_cols(model_info$uncensored_X %>% select(-starts_with("y")))
  
  
  model_info$censored_info <- getDataInfo(model_info$censored_X)
  model_info$bounds0 <- append(model_info$cen.point_, list(c(NA,NA),c(NA,NA)))
  
  model_info
}


# Run LGM -----------------------------------------------------------------
# library(lavaan)
# # https://groups.google.com/g/lavaan/c/KZFGMlCuCDo?pli=1
# HS.model <- ' visual  =~ x1 + x2 + x3
#               textual =~ x4 + x5 + x6
#               speed   =~ x7 + x8 + x9 '
# nobs <- nrow(HolzingerSwineford1939)
# input_cov <- cov(HolzingerSwineford1939 %>% select(matches("^x")))
# input_mu <- colMeans(HolzingerSwineford1939 %>% select(matches("^x")))
# 
# fit <- cfa(HS.model, 
#            sample.nobs = nobs,
#            sample.cov = input_cov, sample.mean = input_mu,
#            meanstructure = T,
#            # data = HolzingerSwineford1939, 
#            estimator = "WLSMV")
# 
# WLS.V=lavInspect(fit, "wls.v")
# NACOV=lavInspect(fit, "gamma")

runLGM <- function(model_info, estimator = "tobit", 
                   lav.estimator = "GLS") {
  
  model_syn <- model_info$model_syn
  covariates <- model_info$covariates
  
  nobs <- model_info$N
  var_names <- rownames(model_info$censored_info$sig2)
  n_timeindi <- sum(str_detect(var_names, "^y"))
  
  if(estimator == "tobit") {
    input_mu <- model_info$tobit_info$Mu
    input_cov <- model_info$tobit_info$Sig2
    
  } else if(estimator == "censored") {
    input_mu <- model_info$censored_info$mu
    input_cov <- model_info$censored_info$sig2
    
  } else if(estimator == "original") {
    input_mu <- model_info$uncensored_info$MU.hat
    input_cov <- model_info$uncensored_info$COV.hat
  }
  
  names(input_mu) <- var_names
  rownames(input_cov) <- var_names
  colnames(input_cov) <- var_names
  
  if(!covariates) {
    input_mu <- input_mu[1:n_timeindi]
    input_cov <- input_cov[1:n_timeindi, 1:n_timeindi]
  }
  
  fit <- growth(model_syn,
             sample.nobs = nobs,
             sample.cov = input_cov, 
             sample.mean = input_mu,
             meanstructure = T,
             estimator = lav.estimator)
  
  return(fit)
}

# LGM model generation ----------------------------------------------------
genModels <- function(model_info, model = 'linear', covariates = T) {
  library(glue)
  
  data <- model_info$censored_X
  time_indi <- data %>% select(starts_with('y'))
  # time_indi <- data.frame(y1 = 1,y2 = 1,y3 = 1,y4 = 1)
  
  for_int <- paste(paste0("1*", names(time_indi)), collapse = " + ")
  for_slp <- paste(paste0(c(0:(ncol(time_indi)-1)), "*", names(time_indi)), 
                   collapse = " + ")
  for_quad <- paste(paste0(c(0:(ncol(time_indi)-1))^2, "*", names(time_indi)), 
                    collapse = " + ")
  
  if(model == "linear") {
    model_syn <- glue(
      "
      I =~ {for_int}
      S =~ {for_slp}
      
      I ~~ I + S
      S ~~ S
")
  }  
  
  if(model == "quad") {
    model_syn <- glue(
      "
      I =~ {for_int}
      S =~ {for_slp}
      Q =~ {for_quad}
      
      I ~~ I + S + Q
      S ~~ S + Q
      Q ~~ Q
")
  }  
  
  
  if(covariates) {
    model_syn <- glue(
      "
      {model_syn}
      
      I ~ x1
      S ~ x1
      
      z1 ~ I + S
")
  }
  
  
  model_info$model <- model
  model_info$covariates <- covariates
  model_info$model_syn <- model_syn
  return(model_info)
}
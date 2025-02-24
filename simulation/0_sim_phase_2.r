for(source.code in list.files("simulation/source",full.names = T)) {source(source.code)}
source_codes <- ls()


# To replicate this simulation, F -> T

if(F) {
  # Conditions -------------------------------------------------------
  nreps <- 1:1000
  
  # Fixed -----------
  growth_mean <- list(c(3,0.5))
  covar_var <- list(c(1,1))
  
  ICC <- c(0.5, 0.7)
  
  x1_to_growth <- list(
    # c(-1, 0.71), # 50% explain growth factor variance
    c(-0.71, 0.5) # 25% explain growth factor variance
  )
  
  growth_to_z1 <- list(
    # gf_02 <- bimat(c(-0.71, 0, 0, 0.5)) %*% bimat(c(1, 0, 0, 1)) %*% t(bimat(c(-0.71, 0, 0, 0.5)))  + bimat(c(1, 0.2, 0.2, 0.5))
    # 
    # gf_00 <-bimat(c(-0.71, 0, 0, 0.5)) %*% bimat(c(1, 0, 0, 1)) %*% t(bimat(c(-0.71, 0, 0, 0.5)))  + bimat(c(1, 0, 0, 0.5))
    # 
    # gf_m02<- bimat(c(-0.71, 0, 0, 0.5)) %*% bimat(c(1, 0, 0, 1)) %*% t(bimat(c(-0.71, 0, 0, 0.5)))  + bimat(c(1, -0.2, -0.2, 0.5))
    
    # matrix(c(0.65, .6),ncol=2) %*% gf_02 %*% matrix(c(0.65, .6))
    c(.65, .6)#,
    # matrix(c(0.7, .6),ncol=2) %*% gf_00 %*% matrix(c(0.7, .6))
    # c(.7, .6)
  )
  
  # Manipulated ---------------
  nobs <- c(200, 500, 1000, 2000)
  ntimepoint <- c(5, 7, 9)
  cenprop = 
    list(
      c(left = 0.05, right = 0.95),
      c(left = 0.1, right = 0.9),
      c(left = 0.2, right = 0.8),
      c(left = 0.3, right = 0.7),
      c(left = 0.4, right = 0.6)
    )
  
  growth_var <- list(
    # Intercept  |    Slope    |    Covariance
    # c(1, 0.5, -0.2),
    # c(1, 0.5,  0),
    c(1, 0.5,  0.2)
  )
  
  
  growth_and_z1<- tibble(growth_var, growth_to_z1)
  
  # condition table ---------------------------------------------------------
  cond_table0 <- 
    crossing(nobs, ntimepoint, cenprop, growth_mean, ICC,
             covar_var, x1_to_growth, growth_and_z1) %>% 
    mutate(cond_num = row_number())
  
  cond_table0$cenprop_chr <- 
    unlist(lapply(cond_table0$cenprop, function(x) paste(x, collapse = "-")))
  cond_table0$growthvar_chr <- 
    unlist(lapply(cond_table0$growth_var, function(x) paste(x, collapse = "_")))
  cond_table0$growthtoz1_chr <- 
    unlist(lapply(cond_table0$growth_to_z1, function(x) paste(x, collapse = "_")))
  cond_table0$x1togrowth_chr <- 
    unlist(lapply(cond_table0$x1_to_growth, function(x) paste(x, collapse = "_")))
  
  cond_table <- cond_table0 %>% crossing(., reps = nreps)
  cond_table
  saveRDS(cond_table0, "results/cond_table0.rds")
  
  # Population info ---------------------------------------------------------
  library(foreach)
  library(doSNOW)
  library(parallel)
  
  # cores <- parallel::detectCores(logical = F)
  # doParallel::registerDoParallel(cores=cores)
  
  cores <- parallel::detectCores() - 2
  cl <- parallel::makeCluster(cores)
  doSNOW::registerDoSNOW(cl)
  
  pb <- txtProgressBar(max=nrow(cond_table), style=3)
  progress <- function(n) setTxtProgressBar(pb, n)
  opts <- list(progress=progress)
  
  
  container <- foreach(irep = 1:nrow(cond_table),
                       .errorhandling = 'remove',
                       .options.snow = opts,
                       # .export = source_codes,
                       .packages = c("glue","lavaan", "dplyr", "purrr",
                                     "tidyr","stringr","mvtnorm")
  ) %dopar% {
    # irep = 1
    
    reps.input <- cond_table$reps[irep]
    cond_num.input <- cond_table$cond_num[irep]
    
    nobs.input         <- cond_table$nobs[irep]
    ntimepoint.input   <- cond_table$ntimepoint[irep]
    
    cenprop.input  <- cond_table$cenprop[[irep]]
    # cenprop.input  <- c(left = 0.4, right = 0.6)
    growth_mean.input  <- cond_table$growth_mean[[irep]]
    growth_var.input   <- cond_table$growth_var[[irep]]
    ICC.input   <- cond_table$ICC[[irep]]
    
    covar_var.input    <- cond_table$covar_var[[irep]]
    growth_to_z1.input <- cond_table$growth_to_z1[[irep]]
    x1_to_growth.input <- cond_table$x1_to_growth[[irep]]
    
    model_info <- 
      mkModelInfo(N           = nobs.input,
                  LAMBDA      = makeLamMat(ntimepoint.input, ncov = 2),
                  PSI         = makePsiMat(growth_var.input, 
                                           covar_var.input),
                  BETA        = makeBetaMat(growth_to_z1.input, 
                                            x1_to_growth.input),
                  NU          = makeNuMat(ntimepoint.input, ncov = 2),
                  ALPHA       = makeAlphaMat(growth_mean.input, c(0,0)),
                  ICC         = ICC.input,
                  pop_cenprop = cenprop.input,
                  local       = FALSE
      )
    
    res <- simulation.phase2(model_info)
    tibble(estimates = list(res$res_estimates),
           fits = list(res$res_fit), 
           cond_num = cond_num.input, 
           reps = reps.input)
    
    # print(irep)
  }
  stopCluster(cl)
  
  simres <- bind_rows(container)
  # simres <- simres %>% left_join(cond_table0, by = "cond_num")
  saveRDS(simres, "simulation/results/phase2_simres_new.rds")
  
}



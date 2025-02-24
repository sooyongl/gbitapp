# UNLIST <- function(x, path) {
#   x %>% 
#     filter(path == path) %>% 
#     select(population, censored, gbit) %>% 
#     unlist()
# }
# function(x) {
#   # x = fitted_covar[[1]]
#   
#   ntp <- unique(unlist(x$ntimepoint))
#   tmat <- makeLamMat(ntp, 0)
#   
#   Iresi <- x %>% filter(path == "I~~I.cov") %>% select(population, censored, gbit) %>% unlist()
#   Sresi <- x %>% filter(path == "S~~S.cov") %>% select(population, censored, gbit) %>% unlist()
#   CovIS <- x %>% filter(path == "I~~S.cov") %>% select(population, censored, gbit) %>% unlist()
#   Varx1<- x %>% filter(path == "x1~~x1.cov") %>% select(population, censored, gbit) %>% unlist()
#   Varz1 <- x %>% filter(path == "z1~~z1.cov") %>% select(population, censored, gbit) %>% unlist()
#   x1toI <- x %>% filter(path == "I~x1.cov") %>% select(population, censored, gbit) %>% unlist()
#   x1toS <- x %>% filter(path == "S~x1.cov") %>% select(population, censored, gbit) %>% unlist()
#   Itoz1 <- x %>% filter(path == "z1~I.cov") %>% select(population, censored, gbit) %>% unlist()
#   Stoz1 <- x %>% filter(path == "z1~S.cov") %>% select(population, censored, gbit) %>% unlist()
#   InterI <- x %>% filter(path == "I~1.cov") %>% select(population, censored, gbit) %>% unlist()
#   InterS <- x %>% filter(path == "S~1.cov") %>% select(population, censored, gbit) %>% unlist()
#   
#   VarI <- Varx1^2 + Iresi
#   VarS <- Varx1^2 + Sresi
#   
#   xvals <- c(-1, 0, 1)
#   
#   
#   tmat
#   
#   InterI["population"] + xvals[1]
#   InterS["population"] + xvals[1]
#   
#   sqrt(VarI["population"])
#   
#   sqrt(VarS["population"])
#   
#   
#   
#   pop_line  <- tmat %*% x$population
#   cen_line  <- tmat %*% x$censored
#   gbit_line <- tmat %*% x$gbit
#   
#   
#   rbind(pop_line, cen_line, gbit_line) %>% 
#     data.frame(
#       value = .,
#       time = 1:ntp,
#       cenprop = unique(unlist(x$cenprop_chr)),
#       nobs = unique(unlist(x$nobs)),
#       ntimepoint = unique(unlist(x$ntimepoint)),
#       reps = unique(unlist(x$reps)),
#       estimator = 
#         rep(c("POP","CEN","GBIT"), each = ntp)
#     )
# }
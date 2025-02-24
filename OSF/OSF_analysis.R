library(lavaan)
library(clavaan) # devtools::install_github("sooyongl/clavaan")

empdata <- read.csv("OSF_empirical_data.csv")

# Unconditional ---------------------------------------------------

model = "
yI =~ 1*y1 + 1*y2 + 1*y3 + 1*y4
yS =~ 0*y1 + 1*y2 + 2*y3 + 3*y4
"

cfit <- 
  a2 %>% 
  select(matches("^y")) %>% 
  clavaan::cgrowth(model = model, 
                   data = ., 
                   bounds = list(y1 = c(0, 100), 
                                 y2 = c(0, 100), 
                                 y3 = c(0, 100), 
                                 y4 = c(0, 100)))


fit <- lavaan::growth(model = model, data = empdata)

summary(cfit, fit.measures = T, standardized = T)
summary(fit, fit.measures = T, standardized = T)

# Conditional ---------------------------------------------------

model = "
yI =~ 1*y1 + 1*y2 + 1*y3 + 1*y4
yS =~ 0*y1 + 1*y2 + 2*y3 + 3*y4

yI ~ gen + race
yS ~ gen + race

yI ~~ yS

income ~ gen + race + yI + yS

gen ~ 1
race ~ 1
income ~ 1
"
cfit <- clavaan::cgrowth(model = model, 
                         data = empdata, 
                         bounds = list(y1 = c(0, 100), 
                                       y2 = c(0, 100), 
                                       y3 = c(0, 100), 
                                       y4 = c(0, 100)))


fit <- lavaan::growth(model = model, data = empdata)

summary(cfit, fit.measures = T)
summary(fit, fit.measures = T)

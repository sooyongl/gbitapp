library(fs)
library(glue)
library(tidyverse)
library(data.table)
library(MplusAutomation)
select <- dplyr::select
searchCB <- function(target) {
  cat(codebook1[str_detect(codebook1, target)][1])
}

# -------------------------------------------------------------------------
codebook <- readLines("data/default.cdb")
codebook0 <- paste(codebook, collapse = "\n")
codebook1 <- str_split(codebook0, "--------------------------------------------------------------------------------", simplify = T)
# cat(codebook1[1,1])

# -------------------------------------------------------------------------

varnames <- codebook[str_detect(codebook, "Survey Year")]

# Regular expression for strings within square brackets
bracket_pattern <- "\\[([^]]+)\\]"

# Regular expression for the year
year_pattern <- "(?<=Survey Year: )\\d{4}"

# Extracting strings within square brackets
bracket_contents <- str_extract_all(varnames, bracket_pattern, simplify = TRUE)
bracket_contents <- str_remove_all(bracket_contents, "\\[|\\]")
# Extracting the year
years <- str_extract(varnames, year_pattern)
varnames <- paste0(bracket_contents,"_", years)

# -------------------------------------------------------------------------
empdata <- read_csv("data/default.csv")
names(empdata) <-  varnames

names(empdata)[1] <- "PUBID"
names(empdata)

searchCB("YSAQ")

a1 <- empdata %>% 
  select(matches("YSAQ"))

hist(a1[5])

library(clavaan)

a1 <- empdata %>% 
  select(matches("YSAQ-513_1997|YSAQ-514_1997|YSAQ-516_1997"))


a2 <- a1 %>% 
  mutate_all(~ if_else(.x < 0, NA, .x/10)) %>% 
  set_names(c("a","b","c"))

a2 <- a2 %>% 
  filter_all(~ !is.na(.x))

cfit <- clavaan::csem(model = "a ~ b + c
              b ~ c", a2, bounds = 
                list(c(0, 10), c(0, 10),c(0, 10)))


fit <- lavaan::sem(model = "a ~ b + c
              b ~ c", a2)

library(lavaan)
summary(cfit)
summary(fit)


a1 <- empdata %>% 
  select(matches("YSAQ-513"), matches("YSAQ-514"),  matches("SEX"))

a2 <- a1 %>% 
  # mutate_all(~ if_else(.x < 0, NA, .x/10)) %>% 
  mutate_at(
    vars(matches("YSAQ")),
    ~ if_else(.x < 0, NA, .x/10)) %>%
  set_names(c("y1","y2","y3","y4","x1","x2","x3","x4","gen"))

a2 <- a2 %>%
  filter_all(~ !is.na(.x))

model = "
yI =~ 1*y1 + 1*y2 + 1*y3 + 1*y4
yS =~ 0*y1 + 1*y2 + 2*y3 + 3*y4

xI =~ 1*x1 + 1*x2 + 1*x3 + 1*x4
xS =~ 0*x1 + 1*x2 + 2*x3 + 3*x4

# I ~ x1 + gen
# S ~ x1 + gen
# 
# x1 ~ 1
# gen ~ 1
"

cfit <- clavaan::cgrowth(model = model, a2, bounds = 
                list(c(0, 10), c(0, 10),c(0, 10),c(0, 10),
                     c(0,10),c(0,10),c(0,10),c(0,10),
                     c(-Inf,Inf)))


fit <- lavaan::growth(model = model, a2, missing = "fiml")
summary(cfit, fit.measures = T, standardized = T)
summary(fit, fit.measures = T, standardized = T)





# -------------------------------------------------------------------------
temp <- empdata %>% 
  select('PUBID', str_which(names(empdata), "(YSAQ-GRIT|YEND-SAQ)"))

temp <- temp %>% 
  mutate_all(~ if_else(.x < 0, NA, .x)) %>% 
  mutate(
    GRIT2013 = rowMeans(across(ends_with("2013")), na.rm = T),
    GRIT2015 = rowMeans(across(ends_with("2015")), na.rm = T),
    GRIT2017 = rowMeans(across(ends_with("2017")), na.rm = T)
  ) %>% 
  select(PUBID, starts_with("GRIT"))


  

a1$GRIT2017


















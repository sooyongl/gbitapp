library(flextable)
library(officer)
library(fs)
library(glue)
library(tidyverse)
library(data.table)
library(MplusAutomation)
library(lavaan)
library(clavaan)
select <- dplyr::select
searchCB <- function(target) {
  cat(codebook1[str_detect(codebook1, target)][1])
}
source("empirical/table_source.R")


empdata <- data.table::fread("cleaneddata/empirical_data_agewise.csv") %>%
  tibble()

table(empdata$birth_year)

a2 <- empdata %>%
  filter(birth_year == 1980) %>%
  # filter(birth_year == 1983) %>%
  # filter_all(~ !is.na(.x)) %>%
  mutate(income = income * 5)
# mutate(income = income )
# mutate_at()
# mutate_at(
#   vars(matches("^y")),
#   ~ .x/10)

# mutate_at(
#   vars(matches("^y")),
#   ~ if_else(.x < 0, NA, .x/10))

a2 <- a2 %>% 
  select(
    
    y1=a17, y2=a18, y3=a19, y4=a20, # 1980
    # y1=a14, y2=a15, y3=a16, y4=a17, # 1983
    gen, race, income) %>% 
  filter_all(~ !is.na(.x))


cenp <- 100
# unconditional ---------------------------------------------------
model = "
yI =~ 1*y1 + 1*y2 + 1*y3 + 1*y4
yS =~ 0*y1 + 1*y2 + 2*y3 + 3*y4
"

cfit <- 
  a2 %>% 
  select(matches("^y")) %>% 
  clavaan::cgrowth(model = model, 
                   data = ., 
                   bounds = list(y1 = c(0, cenp), 
                                 y2 = c(0, cenp), 
                                 y3 = c(0, cenp), 
                                 y4 = c(0, cenp)))


fit <- lavaan::growth(model = model, a2) #, missing = "fiml")

summary(cfit, fit.measures = T, standardized = T)
summary(fit, fit.measures = T, standardized = T)

uncondres <- parameterestimates(cfit) %>% 
  select(1:3, cest = est, cse = se, cz = z, cpvalue = pvalue) %>% 
  bind_cols(
    parameterestimates(fit) %>% 
      select(mest = est, mse = se, mz = z, mpvalue = pvalue)
  )

# fit indces
res_fit <- bind_rows(
  round(fitmeasures(cfit)[c("npar","chisq","df","pvalue", "cfi","tli","rmsea","srmr")],3),
  round(fitmeasures(fit)[c("npar","chisq","df","pvalue", "cfi","tli","rmsea","srmr")],3)
) %>% 
  mutate(model = c("gbit","mle"), .before = npar)

t_fit <- res_fit %>% 
  mk_tbl("Table. Fit")


# uncond results
res_uncond <- uncondres %>% 
  filter(
    !str_detect(op, "=~")
  ) %>% 
  filter(
    !(str_detect(lhs, "^y\\d{1}") & op == "~1")
  ) %>% 
  mutate_if(is.numeric, round, 3) %>% 
  mutate(
    cest = round(cest,2),
    cse = round(cse,2),
    cz = round(cz,2),
    mest = round(mest,2),
    mse = round(mse,2),
    mz = round(mz,2)
  )

t_uncond <- res_uncond %>% 
  mk_tbl("Table. unconditional")

# conditional ---------------------------------------------------

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

cfit <- 
  a2 %>% 
  # select(matches("^y")) %>% 
  clavaan::cgrowth(model = model, 
                   data = ., 
                   bounds = list(y1 = c(0, cenp), 
                                 y2 = c(0, cenp), 
                                 y3 = c(0, cenp), 
                                 y4 = c(0, cenp)))


fit <- lavaan::growth(model = model, a2) #, missing = "fiml")

summary(cfit, fit.measures = T)
summary(fit, fit.measures = T)

condres <- parameterestimates(cfit) %>% 
  select(1:3, cest = est, cse = se, cz = z, cpvalue = pvalue) %>% 
  bind_cols(
    parameterestimates(fit) %>% 
      select(mest = est, mse = se, mz = z, mpvalue = pvalue)
  )

# cond results
res_cond <- condres %>% 
  filter(
    str_detect(lhs, "dep|income|yS|yI")
  ) %>% 
  filter(
    !str_detect(op, "=~")
  ) %>% 
  filter(
    !str_detect(op, "(~1|~~)")
  ) %>% 
  mutate_if(is.numeric, round, 3) %>% 
  mutate(
    cest = round(cest,3),
    cse = round(cse,3),
    cz = round(cz,3),
    mest = round(mest,3),
    mse = round(mse,3),
    mz = round(mz,3)
  )

t_cond <- res_cond %>% 
  mk_tbl("Table. Conditional")

# Save --------------------------------------------------------------------
my.doc <- read_docx()

table_add(my.doc,t_fit)
table_add(my.doc,t_uncond)
table_add(my.doc,t_cond)

print(my.doc, target = "results/emp_tables.docx")











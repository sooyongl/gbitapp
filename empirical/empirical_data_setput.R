library(fs)
library(glue)
library(tidyverse)
library(data.table)
library(MplusAutomation)
select <- dplyr::select
searchCB <- function(target) {
  cat(codebook1[str_detect(codebook1, target)][1])
}

# # -------------------------------------------------------------------------
# codebook <- readLines("data/covariates.cdb")
# codebook0 <- paste(codebook, collapse = "\n")
# codebook1 <- str_split(codebook0, "--------------------------------------------------------------------------------", simplify = T)
# # cat(codebook1[1,1])
# cat(codebook1[str_detect(codebook1, "YINC")])
# # -------------------------------------------------------------------------
# 
# varnames <- codebook[str_detect(codebook, "Survey Year")]
# 
# # Regular expression for strings within square brackets
# bracket_pattern <- "\\[([^]]+)\\]"
# 
# # Regular expression for the year
# year_pattern <- "(?<=Survey Year: )\\d{4}"
# 
# # Extracting strings within square brackets
# bracket_contents <- str_extract_all(varnames, bracket_pattern, simplify = TRUE)
# bracket_contents <- str_remove_all(bracket_contents, "\\[|\\]")
# # Extracting the year
# years <- str_extract(varnames, year_pattern)
# varnames <- paste0(bracket_contents,"_", years)
# 
# # -------------------------------------------------------------------------
# empdata <- read_csv("data/covariates.csv")
# 
# covdata <- empdata %>% 
#   mutate_all(~ if_else(.x < 0, NA, .x)) %>%
#   # mutate_at(
#   #   vars(matches("YSAQ")),
#   #   ~ if_else(.x < 0, NA, .x/10)) %>%
#   set_names("PUBID", c("d1","d2","d3","d4","d5"),
#               paste0("r", 1:4)) %>% 
#   mutate(
#     dep = rowSums(across(matches("^d")), na.rm = T),
#     resp = rowMeans(across(matches("^r")), na.rm = T)
#   )


# -------------------------------------------------------------------------
codebook <- readLines("data/default.cdb")
codebook0 <- paste(codebook, collapse = "\n")
codebook1 <- str_split(codebook0, "--------------------------------------------------------------------------------", simplify = T)
# cat(codebook1[1,1])
cat(codebook1[str_detect(codebook1, "YINC")])
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

searchCB("YSAQ-513")
searchCB("YHEA")


table(empdata$`KEY!BDATE_Y_1997`)

empdata <- empdata %>% 
  mutate(
    birth_year = `KEY!BDATE_Y_1997`,
    age = 1997 - birth_year
  )


a1 <- empdata %>% 
  select(PUBID,
         matches("YSAQ-513"), 
         matches("YINC-1700_2002"),  
         matches("YINC-1700_2003"),  
         matches("YINC-1700_2004"),  
         matches("YINC-1700_2005"),  
         matches("YINC-1700_2006"),  
         matches("YINC-1700_2007"),  
         matches("YINC-1700_2008"),  
         matches("YINC-1700_2009"),  
         matches("YINC-1700_2010"),  
         matches("YINC-1700_2011"),  
         matches("YINC-1700_2013"),  
         matches("YINC-1700_2015"),  
         matches("YINC-1700_2017"),  
         matches("YINC-1700_2019"),  
         matches("YINC-1700_2021"),  
         matches("SEX"),
         matches("ETHNI"),
         matches("birth_year"),
         matches("age")
         )

# a1 <- a1 %>% 
#   left_join(covdata %>% select(PUBID, dep), by = "PUBID")


a2 <- a1 %>% 
  mutate_all(~ if_else(.x < 0, NA, .x)) %>%
  # select(-PUBID) %>% 
  # mutate_at(
  #   vars(matches("YSAQ")),
  #   ~ if_else(.x < 0, NA, .x/10)) %>%
  set_names(c("id","y1","y2","y3","y4",
              paste0("x", 1:sum(str_detect(names(a1), "YINC"))),
              "gen","race","birth_year","age")) %>% 
  mutate(gen = gen - 1,
         race = case_when(
           race == 4 ~ 0, 
           TRUE ~ 1 # Black / Hispanic
         ))

a2 %>%
  mutate(
    income = rowMeans(across(matches("^x")), na.rm = T)
  ) %>% select(-matches("^x")) %>% 
  mutate(income = if_else(is.nan(income), NA, income)) %>%
  # filter_at(vars(matches("^y")), ~ !is.na(.x)) %>% 
  data.table::fwrite(., "cleaneddata/empirical_rawdata.csv")


a2 <- a2 %>% 
  mutate_at(vars(matches("^x")), ~ if_else(.x == 0, 1, .x)) %>% 
  mutate_at(vars(matches("^x")), ~ log(.x))


if(T) {
  library(mice)
  set.seed(1000)
  
  temp <- a2 %>% select(matches("^x"), gen, race)
  temp0 <- temp %>%
    mice(., m=1 , method="pmm")
  
  # temp0$data[temp0$where[,'dep'], 'dep']  <- temp0$imp[['dep']]
  for(i in paste0('x',1:14)) {
    temp0$data[temp0$where[,i], i]  <- temp0$imp[[i]]
  }
  
  # dep <- temp0$data %>% pull(dep)
  
  income <- temp0$data %>%
    mutate(
      income = rowMeans(across(matches("^x")), na.rm = T)
    ) %>% pull(income)
}

a3 <- a2 %>% 
  mutate(
    income = income
    # dep = dep
    ) %>% 
  select(-matches("^x")) %>% 
  relocate(income, .after = race)

data.table::fwrite(a3, "cleaneddata/empirical_data.csv")


# Age-wise ----------------------------------------------------------------

a4 <- lapply(1:nrow(a3), function(i) {
  temp <- a3[i,]
  names(temp)[2:5] <- paste0("a", temp$age:(temp$age+3))
  temp
})

a4 <- bind_rows(a4)
a4 <- a4 %>% relocate(id, a13, a14, a15, a16, a17, a18, a19, a20)

apply(a4, 2, function(x) sum(is.na(x)))

a4 %>% filter(birth_year == 1984)

a4 %>% filter(birth_year == 1984) %>% 
  apply(., 2, function(x) sum(is.na(x)))


data.table::fwrite(a4, "cleaneddata/empirical_data_agewise.csv")















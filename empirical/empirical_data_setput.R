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

a1 <- empdata %>% 
  select(matches("YSAQ-513"), 
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
         matches("SEX"),
         matches("ETHNI"))


a2 <- a1 %>% 
  mutate_all(~ if_else(.x < 0, NA, .x)) %>%
  # mutate_at(
  #   vars(matches("YSAQ")),
  #   ~ if_else(.x < 0, NA, .x/10)) %>%
  set_names(c("y1","y2","y3","y4",
              paste0("x", 1:sum(str_detect(names(a1), "YINC"))),
              "gen","race")) %>% 
  mutate(gen = gen - 1,
         race = case_when(
           race == 4 ~ 0, 
           TRUE ~ 1 # Black / Hispanic
         )) %>% 
  mutate_at(vars(matches("^x")), ~ if_else(.x == 0, 1, .x)) %>% 
  mutate_at(vars(matches("^x")), ~ log(.x))
  
if(F) {
  library(mice)
  set.seed(1000)
  
  temp <- a2 %>% select(matches("^x"), gen, race)
  temp0 <- temp %>%
    mice(., m=1 , method="pmm")
  
  for(i in paste0('x',1:14)) {
    temp0$data[temp0$where[,i], i]  <- temp0$imp[[i]]
  }
  
  income <- temp0$data %>% 
    mutate(
      income = rowMeans(across(matches("^x")), na.rm = T)
    ) %>% pull(income)
}

a2 <- a2 %>% 
  mutate(income = income) %>% 
  select(-matches("^x"))

data.table::fwrite(a2, "cleaneddata/empirical_data.csv")

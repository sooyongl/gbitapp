library(flextable); library(officer)
library(fs)
library(glue)
library(tidyverse)
library(data.table)
library(MplusAutomation)
library(moments)
source("empirical/table_source.R")
select <- dplyr::select

rawdata <- fread("cleaneddata/empirical_rawdata.csv") %>% tibble() #%>% 
# mutate(income = exp(income))

data <- data.table::fread("cleaneddata/empirical_data_agewise.csv") %>%
  tibble() %>% 
  left_join(
    rawdata %>% select(id, raw_income = income), by = "id"
  )


data <- data %>% filter(birth_year == 1980)

data <- data %>% 
  select(
    id,
    Age17=a17, Age18=a18, Age19=a19, Age20=a20, # 1980
    # y1=a14, y2=a15, y3=a16, y4=a17, # 1983
    gen, race, income) %>% 
  filter_all(~ !is.na(.x))


data <- data %>% 
  left_join(
    rawdata %>% select(id, raw_income = income), by = "id"
  ) %>% 
  select(-id, -income) %>% 
  rename(income = raw_income)


prop_ceiling <- apply(data[1:4], 2, function(x) {
  xx <- x[!is.na(x)]
  round(sum(xx == max(xx, na.rm = T))/length(xx),3)
})

prop_floor <- apply(data[1:4], 2, function(x) {
  xx <- x[!is.na(x)]
  round(sum(xx == min(xx, na.rm = T))/length(xx),3)
})

psych::describe(data) %>% 
  # slice(1:4) %>% 
  select(mean, sd, skew, kurtosis) %>% 
  mutate(
    ceiling = c(prop_ceiling, NA, NA,NA),
    floor = c(prop_floor, NA, NA,NA)
  ) %>% 
  mutate_all(round, 3) %>% 
  mutate(vars = c("Age17","Age18","Age19","Age20", "Gender","Race","Income"),
         .before = mean, 
         ) %>% 
  mk_tbl("")


long_data <- data %>% 
  select(Age17:Age20) %>% 
  mutate(id = row_number()) %>% 
  gather("time","value", -id) %>% 
  arrange(id, time)
  

long_data %>% 
  filter(!is.na(value)) %>% 
  ggplot(aes(x = value)) +
  geom_histogram(bins = 15, color = 'white') +
  facet_wrap(. ~ time) +
  labs(x = "", y = "Freq") +
  cowplot::theme_cowplot(font_size = 14)

ggsave("results/emp_censprop.png", height = 4, width = 6)

long_data %>% 
  filter(
    id %in% sample(1:dim(data)[1], 500)
  ) %>% 
  ggplot(aes(y = value, x = time)) +
  geom_line(aes(group = id), alpha = 0.1) +
  labs(x = "Time", y = "Expectation") +
  cowplot::theme_cowplot(font_size = 14)

ggsave("results/emp_lines.png", height = 4, width = 6)

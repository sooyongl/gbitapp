library(flextable); library(officer)
library(fs)
library(glue)
library(tidyverse)
library(data.table)
library(MplusAutomation)
library(moments)
source("empirical/table_source.R")
select <- dplyr::select

data <- fread("cleaneddata/empirical_data.csv") %>% tibble() #%>% 
  # mutate(income = exp(income))

data <- data %>%
  filter_all(~ !is.na(.x))


prop_ceiling <- apply(data[1:4], 2, function(x) {
  xx <- x[!is.na(x)]
  round(sum(xx == max(xx, na.rm = T))/length(xx),3)
})

prop_floor <- apply(data[1:4], 2, function(x) {
  xx <- x[!is.na(x)]
  round(sum(xx == min(xx, na.rm = T))/length(xx),3)
})

psych::describe(data) %>% 
  slice(1:4) %>% 
  select(mean, sd, skew, kurtosis) %>% 
  mutate(
    ceiling = prop_ceiling,
    floor = prop_floor
  ) %>% 
  mutate_all(round, 3) %>% 
  mutate(vars = c("y1","y2","y3","y4"),
         .before = mean, 
         ) %>% 
  mk_tbl("Table. Desci")


long_data <- data %>% 
  select(y1:y4) %>% 
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

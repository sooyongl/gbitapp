library(fs)
library(glue)
library(tidyverse)
library(data.table)
library(MplusAutomation)
library(moments)

select <- dplyr::select

data <- fread("cleaneddata/cleaned.csv") %>% tibble()

psych::describe(data)


prop_ceiling <- apply(data[1:4], 2, function(x) {
  xx <- x[!is.na(x)]
  round(sum(xx == max(xx, na.rm = T))/length(xx),3)
})

prop_floor <- apply(data[1:4], 2, function(x) {
  xx <- x[!is.na(x)]
  round(sum(xx == min(xx, na.rm = T))/length(xx),3)
})

prop_ceiling
prop_floor

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

long_data %>% 
  filter(
    id %in% sample(1:dim(data)[1], 1000)
  ) %>% 
  ggplot(aes(y = value, x = time)) +
  geom_line(aes(group = id), alpha = 0.1) +
  labs(x = "Time", y = "Expectation") +
  cowplot::theme_cowplot(font_size = 14)

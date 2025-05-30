library(flextable)
library(officer)
for(source.code in list.files("simulation/source",full.names = T)) {source(source.code)}
source_codes <- ls()

# Conditions -------------------------------------------------------
cond_table0 <- readRDS("simulation/results/cond_table0.rds")

# cleaning ----------------------------------------------------------------
simres <- readRDS("simulation/results/phase2_simres_new.rds")

simres <- simres %>% left_join(cond_table0, by = "cond_num")

res_estimates <- 
  simres %>% 
  select(-fits) %>% 
  unnest(estimates)

est_data <- res_estimates %>% 
  mutate(
    error_cen = censored - population,
    error_gbit = gbit - population,
    
    rerror_cen = error_cen /  population,
    rerror_gbit = error_gbit / population
  )

bias_dt <- est_data %>% 
  # filter(cenprop_chr == "0.4-0.6") %>%
  group_by(cond_num, 
           nobs, ntimepoint, cenprop_chr, ICC, 
           growthvar_chr, growthtoz1_chr,
           x1togrowth_chr,
           path) %>% 
  
  summarise(
    pop = round(mean(population), 4),
    bias_cen = mean(error_cen, na.rm = T),
    bias_gbit = mean(error_gbit, na.rm = T),
    
    rbias_cen = mean(rerror_cen, na.rm = T),
    rbias_gbit = mean(rerror_gbit, na.rm = T),
    
    sd_cen = sd(censored, na.rm = T),
    
    sd_gbit = sd(gbit, na.rm = T),
    
    mse_cen = mean(error_cen^2, na.rm = T),
    mse_gbit = mean(error_gbit^2, na.rm = T)
  ) %>% 
  ungroup() %>% 
  mutate(
    
    mse_ratio = mse_gbit / mse_cen,
    sd_ratio = sd_gbit / sd_cen,
    
    sd_diff = sd_gbit - sd_cen
    
    # sd_diff = (sd_diff -mean(sd_diff, na.rm = T)) / sd(sd_diff, na.rm = T)
    ) # %>% 
  # mutate_if(is.numeric, ~ round(.x, 3))

min(bias_dt$sd_cen, na.rm = T)
max(bias_dt$sd_cen, na.rm = T)

min(bias_dt$sd_gbit, na.rm = T)
max(bias_dt$sd_gbit, na.rm = T)

# -------------------------------------------------------------------------
filter.inp = "I~1"
filter.inp = "S~1"
filter.inp = "I~~I"
filter.inp = "S~~S"
filter.inp = "I~~S"
filter.inp = "I~x1.cov"
filter.inp = "S~x1.cov"
filter.inp = "z1~I.cov"
filter.inp = "z1~S.cov"

pick_res <- function(filter.inp) {
  
  bias_dt %>%
    
    filter(path == filter.inp) %>% 
    # filter(nosb)
    filter(ntimepoint != "9") %>%
    # filter(cenprop_chr == "0.4-0.6") %>%
    select(nobs, ntimepoint, cenprop_chr, ICC, path,
           starts_with("rbias"), 
           starts_with("sd")) %>% 
    
    select(nobs, ntimepoint, cenprop_chr, ICC,
           starts_with("rbias"), 
           starts_with("sd")) %>% 
    group_by(nobs, cenprop_chr) %>%
    summarise(
      
      
      rbias_cen = mean(rbias_cen),
      rbias_gbit = mean(rbias_gbit),
      
      sd_cen = mean(sd_cen),
      sd_gbit = mean(sd_gbit),
      
      
      
      # var_ratio  = mean(var_ratio)
      sd_diff = mean(sd_diff)
    ) %>%
    
    pivot_wider(names_from = "cenprop_chr", 
                values_from = c("rbias_cen", "rbias_gbit", 
                                "sd_cen", "sd_gbit", "sd_diff")
    ) %>%  
    select(-matches("sd_cen|sd_gbit")) %>% 
    select(nobs,
           matches("0.05-0.95"),
           matches("0.1-0.9"),
           matches("0.2-0.8"),
           matches("0.3-0.7"),
           matches("0.4-0.6")
    ) %>% 
    mutate(type = filter.inp, .before = nobs) %>% 
    ungroup()
}


table_uncond <- pick_res(filter.inp = "I~1") %>% 
  bind_rows(
    pick_res("S~1"),
    pick_res("I~~I"),
    pick_res("S~~S"),
    pick_res("I~~S")
  )
table_uncond <- table_uncond %>% 
  table.phase2(caption = "Table. Relative bias and variance difference between GBIT and MLE for unconditional LGM parameters")

table_cond <- pick_res("I~x1.cov") %>% 
  bind_rows(
    pick_res("S~x1.cov"),
    pick_res("z1~I.cov"),
    pick_res(filter.inp = "z1~S.cov")
  )
table_cond <- table_cond %>% table.phase2(caption = "Table. Relative bias and variance difference between GBIT and MLE for conditional LGM parameters")

# Tables ------------------------------------------------------
my.doc <- read_docx()

tables <- ls()[str_detect(ls(), "table_(un|con)")]

table_add(my.doc, eval(as.name(tables[1])), landscape = T)
table_add(my.doc, eval(as.name(tables[2])), landscape = T)

# for(i in tables[-1]) {
#   table_add(my.doc, i, landscape = T)
# }

print(my.doc, target = "simulation/results/phase2_table.docx")













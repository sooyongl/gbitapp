est_data %>% 
  filter(path == "I~~I.cov") %>% 
  # filter(nosb)
  filter(ntimepoint != "9") %>%
  filter(cenprop_chr == "0.3-0.7") %>%
  # filter(reps == 1) %>% 
  select(nobs, ntimepoint, cenprop_chr, ICC, path, reps,
         population, censored, gbit,
         starts_with("rerror")) %>% 
  group_by(nobs, cenprop_chr) %>% 
  summarise(
    mean(population),
    mean(censored),
    mean(gbit),
    mean(rerror_cen),
    mean(rerror_gbit)
  )


est_data %>% 
  filter(path == "I~~I.cov") %>% 
  # filter(nosb)
  filter(ntimepoint != "9") %>%
  filter(cenprop_chr == "0.1-0.9") %>%
  filter(reps == 1) %>% 
  select(nobs, ntimepoint, cenprop_chr, ICC, path, reps,
         population, censored, gbit,
         starts_with("rerror"))

est_data %>% 
  filter(path == "I~~I.cov") %>% 
  # filter(nosb)
  filter(ntimepoint != "9") %>%
  filter(cenprop_chr == "0.2-0.8") %>%
  filter(reps == 1) %>% 
  select(nobs, ntimepoint, cenprop_chr, ICC, path, reps,
         population, censored, gbit,
         starts_with("rerror"))

est_data %>% 
  filter(path == "I~x1.cov") %>% 
  # filter(nosb)
  filter(ntimepoint != "9") %>%
  filter(cenprop_chr == "0.1-0.9") %>%
  filter(reps == 1) %>% 
  select(nobs, ntimepoint, cenprop_chr, ICC, path, reps,
         population, censored, gbit,
         starts_with("rerror"))

est_data %>% 
  filter(path == "I~~I") %>% 
  # filter(nosb)
  filter(ntimepoint != "9") %>%
  filter(cenprop_chr == "0.1-0.9") %>%
  filter(reps == 1) %>% 
  select(nobs, ntimepoint, cenprop_chr, ICC, path, reps,
         population, censored, gbit,
         starts_with("rerror"))

est_data %>% 
  filter(path == "I~~I") %>% 
  # filter(nosb)
  filter(ntimepoint != "9") %>%
  filter(cenprop_chr == "0.2-0.8") %>%
  filter(reps == 1) %>% 
  select(nobs, ntimepoint, cenprop_chr, ICC, path, reps,
         population, censored, gbit,
         starts_with("rerror"))

est_data %>% 
  filter(path == "I~x1.cov") %>% 
  # filter(nosb)
  filter(ntimepoint != "9") %>%
  filter(cenprop_chr == "0.2-0.8") %>%
  filter(reps == 1) %>% 
  select(nobs, ntimepoint, cenprop_chr, ICC, path, reps,
         population, censored, gbit,
         starts_with("rerror"))

bias_dt %>% 
  filter(path == "I~x1.cov") %>% 
  # filter(nosb)
  filter(ntimepoint != "9") %>%
  filter(cenprop_chr == "0.2-0.8") %>%
  select(nobs, ntimepoint, cenprop_chr, ICC, path,
         starts_with("rbias"), starts_with("mse")) %>% 
  
  select(nobs, ntimepoint, cenprop_chr, ICC,
         starts_with("rbias"), starts_with("mse")) %>% 
  
  print(n=100)
  group_by(nobs, cenprop_chr) %>%
  summarise(
    rbias_cen = mean(rbias_cen),
    rbias_gbit = mean(rbias_gbit),
    mse_cen = mean(mse_cen),
    mse_gbit = mean(mse_gbit),
    mse_ratio  = mean(mse_ratio)
  ) %>% 
  
  pivot_wider(names_from = "cenprop_chr", 
              values_from = c("rbias_cen", "rbias_gbit", 
                              "mse_cen", "mse_gbit", "mse_ratio")
  ) %>%  
  select(-matches("mse_cen|mse_gbit")) %>% 
  select(nobs,
         matches("0.05-0.95"),
         matches("0.1-0.9"),
         matches("0.2-0.8"),
         matches("0.3-0.7"),
         matches("0.4-0.6")
  )
  

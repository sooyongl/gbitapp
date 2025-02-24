for(source.code in list.files("simulation/source",full.names = T)) {source(source.code)}
source_codes <- ls()

# Conditions -------------------------------------------------------
cond_table0 <- readRDS("simulation/results/cond_table0.rds")

# cleaning ----------------------------------------------------------------
# simres <- readRDS("G:/My Drive/project/00dissert_censored_LGM/code/results/phase2_simres.rds")
simres <- readRDS("simulation/results/phase2_simres_new.rds")

simres <- simres %>% left_join(cond_table0, by = "cond_num")

simres %>% 
  count(cond_num) %>% 
  filter(n < 1000)

res_fits <- 
  simres %>% 
  select(-estimates) %>% 
  unnest(fits)

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
    
    mse_cen = mean(error_cen^2, na.rm = T),
    mse_gbit = mean(error_gbit^2, na.rm = T)
  ) %>% 
  ungroup() %>% 
  mutate(mse_ratio = mse_gbit / mse_cen) %>% 
  mutate_if(is.numeric, ~ round(.x, 3))

# Fail to converged -----------------------------------------
phase2_table_nonconv <- res_estimates %>% 
  filter(path != "x1~~x1.cov") %>% 
  filter(is.na(censored)) %>% 
  count(cond_num, reps) %>% 
  count(cond_num) %>% 
  left_join(cond_table0 %>% 
              select(cond_num, ICC, 
                     nobs, ntimepoint, 
                     # ends_with("_chr")
                     cenprop_chr
              ), by = "cond_num") %>% 
  select(-cond_num) %>% 
  pivot_wider(
    names_from = cenprop_chr,
    values_from = n,
    values_fill = 0
  ) %>% 
  select(N=nobs, TP=ntimepoint, ICC, everything()) %>% 
  
  flextable() %>% 
  flextable::font(fontname = "Times", part = "all") %>%
  padding(., padding.top = 0.1, padding.bottom = 0.1, 
          padding.left=0,padding.right=0, part = "header") %>% 
  padding(., padding.top = 0.1, padding.bottom = 0.1, 
          padding.left=0,padding.right=5, part = "body") %>% 
  fontsize(size = 11, part = "header") %>%
  fontsize(size = 11, part = "body") %>% 
  width(width = 1) %>% 
  set_caption("Table 4.2. Frequency of non-converged replications.")

# plotting ------------------------------------------------------------

phase2_figure_fit <- plotFit.phase2(res_fits, c("200","500",
                                                "1000","2000"))
# phase2_figure_fit_small <- plotFit.phase2(res_fits, c("200","500"))
# phase2_figure_fit_large <- plotFit.phase2(res_fits, c("1000","2000"))

dev.size("in")

# Estimates Results ------------------------------------------
x.width = 0.55
phase2_table_int <- bias_dt %>% 
  table.phase2(filter.inp = "I~1", x.width = x.width,
               caption = "Table 4.3. Relative bias and MSE ratio between GBIT and cML for the means of growth intercept")

phase2_table_slp <- bias_dt %>% 
  table.phase2(filter.inp = "S~1", x.width = x.width,
               caption = "Table 4.4. Relative bias and MSE ratio between GBIT and cML for the means of growth slope")

phase2_table_intv <- bias_dt %>% 
  table.phase2(filter.inp = "I~~I", x.width = x.width,
               caption = "Table 4.5. Relative bias and MSE ratio between GBIT and cML for the variances of growth intercepts")

phase2_table_slpv <- bias_dt %>% 
  table.phase2(filter.inp = "S~~S", x.width = x.width,
               caption = "Table 4.6. Relative bias and MSE ratio between GBIT and cML for the variances of growth slope")

phase2_table_iscov <- bias_dt %>% 
  table.phase2(filter.inp = "I~~S", x.width = x.width,
               caption = "Table 4.7. Relative bias and MSE ratio between GBIT and cML for the covariances between growth factors")

phase2_table_x1toi <- bias_dt %>% 
  table.phase2(filter.inp = "I~x1.cov", x.width = x.width,
               caption = "Table 4.8. Relative bias and MSE ratio between GBIT and cML for the covariate coefficient on growth intercept across the sample sizes and the number of timepoints")

phase2_table_x1tos <- bias_dt %>% 
  table.phase2(filter.inp = "S~x1.cov", x.width = x.width,
               caption = "Table 4.9. Relative bias and MSE ratio between GBIT and the censored data for the covariate coefficient on growth slope across the sample sizes and the number of timepoints")

phase2_table_itoz1 <- bias_dt %>% 
  table.phase2(filter.inp = "z1~I.cov", x.width = x.width,
               caption = "Table 4.10. Relative bias and MSE ratio between GBIT and the censored data for the effects of growth intercept on the distal outcome across the sample sizes and the number of timepoints")

phase2_table_stoz1 <- bias_dt %>% 
  table.phase2(filter.inp = "z1~S.cov", x.width = x.width,
               caption = "Table 4.11. Relative bias and MSE ratio between GBIT and the censored data for the effects of growth slope on the distal outcome across the sample sizes and the number of timepoints")

# ---------------------------------------------------------
phase2_figure_int <- plotBoxPlot.phase2(res_estimates,"I~1",ICC.add = F);
dev.size("in")
phase2_figure_slp <- plotBoxPlot.phase2(res_estimates,"S~1",ICC.add = F)

phase2_figure_intv <- plotBoxPlot.phase2(res_estimates,"I~~I",ICC.add = F)
phase2_figure_slpv <- plotBoxPlot.phase2(res_estimates,"S~~S",ICC.add = F)
phase2_figure_iscov <- plotBoxPlot.phase2(res_estimates,"I~~S",ICC.add = F)

phase2_figure_x1toi <- plotBoxPlot.phase2(res_estimates,"I~x1.cov",ICC.add = F)
phase2_figure_x1tos <- plotBoxPlot.phase2(res_estimates,"S~x1.cov",ICC.add = F)
# phase2_figure_itoz1 <- plotBoxPlot.phase2(res_estimates,"z1~I.cov",ICC.add = F)
phase2_figure_itoz1 <- plotBoxPlot.phase2(res_estimates,"z1~I.cov",ICC.add = F)

phase2_figure_stoz1 <- plotBoxPlot.phase2(res_estimates,"z1~S.cov",ICC.add = F)

# names(res_estimates)
# res_estimates %>%
#   filter(path == "z1~I.cov") %>%
#   # arrange(censored) %>%
#   ggplot() +
#   geom_boxplot(aes(cenprop_chr, censored)) +
#   facet_grid(nobs ~ ICC + ntimepoint)

# ---------------------------------------------------------------------
# -------------------------------------------------------------------------
# Tables --------------------------------------------------------------
my.doc <- read_docx()

tables <- ls()[str_detect(ls(), "phase2_table_")]

tables <- c("phase2_table_nonconv",
            
            "phase2_table_int",
            "phase2_table_slp",
            "phase2_table_intv",
            "phase2_table_slpv",
            
            "phase2_table_iscov",   
            
            "phase2_table_x1toi",
            "phase2_table_x1tos",
            "phase2_table_itoz1",     
            "phase2_table_stoz1")   

table_add(my.doc,phase2_table_nonconv)

table_add(my.doc, eval(as.name(tables[2])), landscape = T)
table_add(my.doc, eval(as.name(tables[3])), landscape = T)
table_add(my.doc, eval(as.name(tables[4])), landscape = T)
table_add(my.doc, eval(as.name(tables[5])), landscape = T)
table_add(my.doc, eval(as.name(tables[6])), landscape = T)
table_add(my.doc, eval(as.name(tables[7])), landscape = T)
table_add(my.doc, eval(as.name(tables[8])), landscape = T)
table_add(my.doc, eval(as.name(tables[9])), landscape = T)
table_add(my.doc, eval(as.name(tables[10])), landscape = T)
# for(i in tables[-1]) {
#   table_add(my.doc, i, landscape = T)
# }

print(my.doc, target = "results/phase2_table.docx")
# file.show("results/phase2_table.docx")

# Figures -------------------------------------------------------
figures <- ls()[str_detect(ls(), "phase2_figure_")]
figures <- c("phase2_figure_fit",
             
             "phase2_figure_int",
             "phase2_figure_slp",
             "phase2_figure_intv",
             "phase2_figure_slpv",
             
             "phase2_figure_iscov",   
             
             "phase2_figure_x1toi",
             "phase2_figure_x1tos",
             "phase2_figure_itoz1",     
             "phase2_figure_stoz1") 

ggSAVE("phase2_figure_fit",  phase2_figure_fit,    16, 8)

ggSAVE("phase2_figure_int",  phase2_figure_int,    12, 10)
ggSAVE("phase2_figure_slp",  phase2_figure_slp,    12, 10)
ggSAVE("phase2_figure_intv",  phase2_figure_intv,  12, 10)
ggSAVE("phase2_figure_slpv",  phase2_figure_slpv,  12, 10)
ggSAVE("phase2_figure_iscov", phase2_figure_iscov, 12, 10)
ggSAVE("phase2_figure_x1toi", phase2_figure_x1toi, 12, 10)
ggSAVE("phase2_figure_x1tos", phase2_figure_x1tos, 12, 10)
ggSAVE("phase2_figure_itoz1", phase2_figure_itoz1, 12, 10) # 12 16 or 12 10
ggSAVE("phase2_figure_stoz1", phase2_figure_stoz1, 12, 10)


file_paths <- paste0("simulation/results/",figures, ".png")

my.doc <- read_docx()
img_add(my.doc, file_paths[1], 
        p.title = glue("Figure 4.4. Results of identification rates for linear or quadratic patterns across the sample sizes, the size of ICC, the number of timepoints, and censoring proportion"),
        height = 5.15, width = 9.1, landscape = T)

img_captions <- c(
  #1
  "Results of the errors of the means of growth intercepts across the sample sizes and the number of timepoints.",
  #2
  "Results of the errors of the means of growth slopes across the sample sizes and the number of timepoints.",
  #3
  "Results of the errors of the variances of growth intercepts across the sample sizes and the number of timepoints.",
  #4
  "Results of the errors of the variances of growth slopes across the sample sizes and the number of timepoints.",
  #5
  "Results of the errors of the covariances between growth factors across the sample sizes and the number of timepoints.",
  #6
  "Results of the errors of the covariate coefficient on growth intercept across the sample sizes and the number of timepoints.",
  #7
  "Results of the errors of the covariate coefficient on growth slope across the sample sizes and the number of timepoints.",
  #8
  "Results of the errors of the effects of growth intercept on the distal outcome across the sample sizes and the number of timepoints.",
  #9
  "Results of the errors of the effects of growth slope on the distal outcome across the sample sizes and the number of timepoints."
)
i <- 4
j <- 0
for(ppp in file_paths[-1]) {
  
  # cp <- str_remove(ppp, "results/phase2_")
  # cp <- str_remove(cp, ".png")
  
  i <- i + 1
  j <- j + 1
  cp <- img_captions[j]
  
  # if(str_detect(ppp, "figure_stoz1|figure_itoz1")) {
  #   
  #   img_add(my.doc, ppp,
  #           p.title = glue("Figure 4.{i}. {cp}"),
  #           height = 8.5, width = 5.8, landscape = F)
  #   # 8.27 x 11.6 inc; 1.25 margin
  #   
  # } else {
  img_add(my.doc, ppp,
          p.title = glue("Figure 4.{i}. {cp}"),
          height = 6.0, width = 5.8, landscape = F)
  # 8.27 x 11.6 inc; 1.25 margin
  # }
}

print(my.doc, target = "simulation/results/phase2_figure.docx")

img_add <- function(x.doc, file_path, p.title,
                    height, width, landscape = F) {
  # 8.5 x 11 inc; 1.25 margin
  if(landscape)
    x.doc <- body_end_section_portrait(x.doc)
  
  body_add_img(x.doc, src = file_path, height = height, width = width ) %>% 
    body_add_par(value = p.title, style = "Image Caption")
  
  body_add_par(x.doc, " ")
  
  if(landscape)
    x.dox <- body_end_section_landscape(x.doc)
  
}

plot_add <- function(x.doc, gg, p.title, width = 6.5, height = 8,
                     landscape = F) {
  
  if(landscape)
    x.doc <- body_end_section_portrait(x.doc)
  
  body_add_gg(x.doc, gg,
              width = width, height = height) %>%
    body_add_par(value = p.title, style = "Image Caption")
  
  body_add_par(x.doc, " ")
  
  if(landscape)
    x.dox <- body_end_section_landscape(x.doc)
}


ggSAVE <- function(file_name, p, width = 6.5, height = 8) {
  # assign(file_name, p)
  filename <- file.path("results", paste0(file_name,".png"))
  ggsave(filename, # 8.5 x 11 inc; 1.25 margin
         p,
         width = width,  height = height,  units = "in")
}

ggARANGE <- function(p1, p2) {
  library(ggpubr)
  library(gridExtra)
  library(grid)
  library("ggplotify")
  
  get_legend<-function(a.gplot){
    tmp <- ggplot_gtable(ggplot_build(a.gplot))
    leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
    legend <- tmp$grobs[[leg]]
    return(legend)}
  
  p2_legend <- get_legend(p2)
  
  grid.arrange(
    arrangeGrob(p1 + theme(legend.position="none"), 
                p2 + theme(legend.position="none"), 
                ncol=2), 
    p2_legend,
    nrow=2,heights=c(10, 1)) %>% 
    as.ggplot()
  
}

# Phase 1 -----------------------------------------------------------------
boxPlot.phase1 <- function(selected_param) {
  theme_set(theme_bw())
  
  selected_param <- selected_param# "sig22"
  
  temp_max <- simres_0 %>% 
    filter(params == selected_param) %>% pull(value) %>% max()
  
  get_box_stats <- function(y, upper_limit = temp_max) {
    return(data.frame(
      y = 0.95 * upper_limit,
      label = paste(
        "Mean =", round(mean(y), 2), "\n",
        "Median =", round(median(y), 2), "\n"
      )
    ))
  }
  
  simres_0 %>% 
    filter(params == selected_param) %>%
    ggplot(aes(as.factor(estimator), value)) +
    geom_boxplot( 
      width=0.5,
      outlier.colour="black", outlier.shape=16,
      outlier.size=2, notch=FALSE
    ) +
    stat_summary(fun=mean, geom="point", shape=23, size=4) +
    stat_summary(fun.data = get_box_stats, geom = "text", 
                 size = 3,
                 hjust = 0.5, vjust = 0.9) +
    # geom_point(data = sum_res_0,
    #            aes(as.factor(estimator), value)) +
    # ggforce::geom_sina(
    #     aes(as.factor(corr), value,
    #         color = estimator, shape = estimator), size = 3,alpha = 0.8
    # ) +
    # geom_point(
    #   aes(as.factor(corr), value, 
    #       color = estimator, shape = estimator), size = 3,
    #   position = position_jitter(width = 0.4),
    #   alpha = 0.8
  # ) +
  ggh4x::facet_grid2(nobs ~ cenprop_chr, scales = "free")
  
}

dotPlot.phase1 <- function(simres_1, selected_param, alpha = 0.8, 
                           title = "", txt_size = 14) {
  
  windowsFonts(Times = windowsFont("Times New Roman"))
  
  theme_set(theme_bw(base_family = "Times", base_size = txt_size))
  
  p_data <- simres_1 %>% 
    filter(params %in% selected_param) %>% 
    mutate(
      estimator = case_when(
        estimator == "cen_info" ~ "No treatment",
        TRUE ~ "GBIT"
      ),
      estimator = factor(estimator, levels = c("GBIT","No treatment"))
    ) %>% 
    sample_frac(size = 0.2) %>% 
    ggplot()
  

  
  # if(selected_param == "corr2") {
  # p_data +
  #   geom_point(aes(value, uc_info, color = estimator), alpha = alpha) +
  #   ggh4x::facet_grid2(nobs + cenprop_chr ~  corr, axes = "all",
  #                      scales = "free") +
  #   scale_color_brewer(type = "qual") +
  #   lims(y = c(0,1))
  # } else {
  p_data +
    geom_point(aes(uc_info, value, 
                   shape = estimator,
                   color = estimator,
                   alpha = estimator), 
    ) +
    geom_abline(slope = 1, intercept = 0, alpha = 0.5, 
                size = 1.2) +
    ggh4x::facet_grid2(cenprop_chr  ~  nobs
                       # , 
                       # scales = "free"
    ) +
    
    scale_color_brewer(
      name = "Estimator",
      palette = 'Set1', type = "qual") +
    
    # scale_color_manual(
    #   name = "Estimator",guide = 'none',
    #   values = c("grey95","black")) +
    
    scale_alpha_manual(
      name = "Estimator",guide = 'none',
      values = c(alpha, 0.6)) +
    
    scale_shape_manual(
      name = "Estimator",
      values = c(20, 4)) +
    
    labs(x = "Population", y = "Estimates",
         # title = expression(italic(title))
         title = paste0("*",title,"*")
         )+
    theme(aspect.ratio = 1) +
    
    theme(plot.title = ggtext::element_markdown()) +
    
    theme(legend.text = element_text(size = 14),
          legend.title = element_text(size = 15, face = "bold"),
          # legend.key.size = unit(5, 'point'),
          legend.position = 'bottom') +
    guides(shape = guide_legend(override.aes = list(size = 5)))
  
  # }
  
  
}




# Phase 2 --------------------------------------------------
plotFit.phase2 <- function(res_fits, filter.sample) {
  
  windowsFonts(Times = windowsFont("Times New Roman"))
  
  # theme_set()
  
  
  NREPS <- length(unique(res_fits$reps))
  res_fits <- res_fits %>% 
    mutate(estimator = as.factor(estimator),
           selected = as.factor(selected),
           cond_num = as.factor(cond_num),
           
           nobs = as.factor(nobs),
           ntimepoint = as.factor(ntimepoint),
           ICC = as.factor(ICC),
           cenprop_chr = as.factor(cenprop_chr),
           
           growthvar = as.factor(growthvar_chr),
           growthtoz1 = as.factor(growthtoz1_chr),
           x1togrowth = as.factor(x1togrowth_chr)
           
    ) %>% 
    filter(nobs %in% filter.sample)
  
  temp <- res_fits %>% 
    count(nobs, 
          ntimepoint, 
          cenprop_chr, 
          ICC,
          estimator) %>% 
    rename(totaln = n)
  
  res_fits %>% 
    group_by(nobs, 
             ntimepoint, 
             cenprop_chr, 
             ICC,
             estimator,selected) %>% 
    summarise(n = n()) %>% 
    left_join(temp, by = c("nobs","ntimepoint","cenprop_chr","ICC","estimator")) %>% 
    
    mutate(
      prop = round(n / totaln,2), # NREPS,
      
      n = n ,
      totaln = totaln ,
      
      txt_col = case_when(
        # selected == "linear" ~ "black", 
        selected == "linear" & prop != 0 ~ "black",
        selected == "linear" & prop == 0 ~ "transparent",
        selected == "quad" & prop != 0 ~ "white",
        selected == "quad" & prop == 0 ~ "transparent"
        # TRUE ~"white"
      ),
      txt_vjust = case_when(
        prop == 0 ~ 1, 
        prop == 1 ~ 1,
        TRUE ~ 0.5),
    ) %>% 
    # filter(cenprop_chr == filter.input) %>% 
    ggplot(aes(x=estimator, y = n   , fill=selected)) +
    geom_col(
      color = 'white',
      width = .8) + 
    
    geom_text(aes(
      label = paste0(100*round(prop,3),""),
      color = txt_col#,
      # vjust = txt_vjust
    ),
    position = position_stack(vjust = .5),
    # fill = NA, # label.color = NA, # vjust = 1,
    size = 3.2, show.legend = FALSE,
    fontface='bold'
    ) +
    
    scale_fill_grey(
      labels = c("Linear","Quadratic"),
      start = 0.7,
      end = 0.4) +
    labs(y = "Freq", 
         fill = "", 
         x = "Estimator"
         # title = 
         #   glue("Censored proportion: {filter.input}")
    ) +
    scale_color_manual(
      
      values = c("black",'transparent',"white")) +
    # facet_wrap(. ~ cond_num) +
    # ggh4x::facet_grid2(ntimepoint + growthvar ~  nobs, 
    #                    scales = "free") +
    ggh4x::facet_nested(ICC + nobs ~ cenprop_chr + ntimepoint, 
                        nest_line = element_line(linetype = 1)
    ) +
    
    scale_x_discrete(labels = c("C",
                                "G",
                                "P")
                     # , 
                     # guide = guide_axis(n.dodge=3)
    ) +
    labs(
      x = ""#,
      # caption = "C: Censored; G: GBIT; P: Population"
    ) +
    theme_bw(base_family = "Times", base_size = 16) +
    theme(
      text = element_text(color = "black"),
      plot.title = 
        element_text(
          family = "Times", size = 14, face = 'bold'
        ),
      # legend.position = c(0, 1),
      legend.position = 'bottom',
      legend.justification = "center",
      legend.background = element_rect(fill = 'transparent'),
      # legend.justification = c("left", "top"),
      # legend.box.just = "left",
      # legend.key.size = unit(5, 'point'),
      # legend.title = element_text(size = 15, face = "bold"),
      legend.text = element_text(size = 14, face = "bold"),
      legend.margin = margin(6, 6, 6, 6)
    ) +
    theme(
      strip.text = element_text(face = 'bold', size = 12),
      strip.background = element_rect(fill = "grey95"),
      ggh4x.facet.nestline = element_line(colour = "black"))
  
  
  #+
  # theme(
  #   axis.text.y = element_markdown(color = "black", size = 14),
  #   axis.text.x = element_markdown(color = "black", size = 14),
  #   plot.caption = element_markdown(lineheight = 1.2)
  # ) +
  # theme(
  # plot.background = element_rect(fill = "black"),
  # panel.background = element_rect(fill = "grey95")
  # )
}

plotEstimates.phase2 <- function(res_estimates, 
                                 given.path = "I~~I", 
                                 filter.input ="0.1-0.7",
                                 frac.size = 0.1) {
  # res_estimates = bias_dt
  # given.path = "z1~S.cov" #"I~~I"
  # filter.input = cenprop.val
  # frac.size = 0.1
  
  windowsFonts(Times = windowsFont("Times New Roman"))
  
  # theme_set()
  
  full_dt <- res_estimates %>% 
    filter(path == given.path) %>% 
    pivot_longer(
      cols = c(censored, gbit),
      names_to = "estimator", values_to = "values") %>% # print(n=20)
    filter(cenprop_chr == filter.input) %>% 
    mutate(
      error = values - population
    )
  
  
  full_dt <- full_dt %>% 
    mutate(
      path = 
        case_when(
          str_detect(path, "I~~I$") ~ "Intercept variance",
          str_detect(path, "S~~S$") ~ "Slope variance",
          str_detect(path, "I~~S$") ~ "Covariance between growth factors",
          str_detect(path, "I~1$") ~ "Intercept mean",
          str_detect(path, "I~1$") ~ "Slope mean",
          
          str_detect(path, "I~x1.cov$") ~ "Effects of covariate on intercept",
          str_detect(path, "S~x1.cov$") ~ "Effects of covariate on slope",
          str_detect(path, "z1~I.cov$") ~ "Effects of intercept on distal outcome",
          str_detect(path, "z1~S.cov$") ~ "Effects of slope on distal outcome",
        )
    )
  pathname <- unique(full_dt$path)
  
  
  plot_dt <- full_dt %>% 
    group_split(cond_num) %>% 
    map_df(., ~ .x %>%  sample_frac(size = frac.size))
  
  if(str_detect(given.path, "cov")) {
    # growthtoz1_chr
    # x1togrowth_chr
    if(str_detect(given.path, "x1")) {
      
      facet_form <- as.formula("x1togrowth_chr + nobs ~ ntimepoint + growthvar_chr")
    } else {
      facet_form <- as.formula("nobs ~ ntimepoint + growthvar_chr")
    }
    
  } else {
    facet_form <- as.formula("nobs ~ ntimepoint + growthvar_chr")
  }
  
  if(min(plot_dt$error, na.rm = T) < -2 |
     max(plot_dt$error, na.rm = T) > 2) {
    
    error_limit <- c(-2, 2)
  } else {
    error_limit <- NULL
  }
  
  plot_dt %>% 
    ggplot(aes(estimator, error)) +
    geom_hline(
      aes(yintercept = 0), 
      linetype = "dashed", color = "blue", alpha = 1) +
    # geom_text(
    #   aes(y = population, x = -Inf, 
    #       # label = round(population,3), 
    #       label = "population"),
    #   color = 'black',
    #   hjust = -0.5,
    #   vjust = 1
    # ) +
    # geom_violin() +
    # geom_boxplot() +
    ggforce::geom_sina(alpha = 0.1) +
    stat_summary(
      data = full_dt,
      fun=mean, geom="point", shape=17, size=2, 
      fill = 'red',
      color = 'red') +
    
    # ggh4x::facet_wrap2(. ~ cond_num, scales = "free") +
    ggh4x::facet_nested(facet_form
                        ,
                        nest_line = element_line(linetype = 1)
    ) +
    # ggh4x::facet_grid2(facet_form,
    # , 
    # scales = "free"
    # ) +
    # theme(
    #   strip.background = element_blank(),
    #   strip.text = element_textbox(
    #     size = 12,
    #     color = "white", fill = "#5D729D", box.color = "#4A618C",
    #     halign = 0.5, linetype = 1, r = unit(5, "pt"), width = unit(1, "npc"),
    #     padding = margin(2, 0, 1, 0), margin = margin(3, 3, 3, 3)
  #   )
  # ) +
  labs(
    y = "Error",
    x = "Estimator",
    title = glue("Censored proprotion: {filter.input}; Parameter: {pathname}")) +
    scale_x_discrete(labels = c("Without treatment",
                                "GBIT"),
                     guide = guide_axis(n.dodge=2)
    ) +
    theme(strip.background = element_rect(fill = "grey95"),
          ggh4x.facet.nestline = element_line(colour = "black")) +
    coord_cartesian(ylim=error_limit) +
    theme_bw(base_family = "Times", base_size = 16)
  
  # theme(legend.text = element_text(size = 14),
  #       legend.title = element_text(size = 15, face = "bold"),
  #       # legend.key.size = unit(5, 'point'),
  #       legend.position = 'bottom') +
  #   guides(shape = guide_legend(override.aes = list(size = 5)))
  
  
  # list(plot_data = full_dt, plot_p = p)
}


plotBoxPlot.phase2 <- function(res_estimates, 
                               given.path = "I~~I",
                               frac.size = 0.1,
                               ICC.add = T,
                               my.facet = NULL) {
  
  # given.path = "z1~S.cov"
  # filter.input = cenprop.val
  # frac.size = 0.1
  
  windowsFonts(Times = windowsFont("Times New Roman"))
  
  # theme_set(theme_bw(base_family = "Times", base_size = 19))
  
  
  full_dt <- res_estimates %>% 
    filter(path == given.path) %>% 
    pivot_longer(
      cols = c(censored, gbit),
      names_to = "estimator", values_to = "values") %>% # print(n=20)
    # filter(cenprop_chr == filter.input) %>% 
    mutate(
      error = (values - population) / abs(population)
    ) %>% 
    filter(!is.na(error))
  
  
  full_dt <- full_dt %>% 
    mutate(
      path = 
        case_when(
          str_detect(path, "I~~I$") ~ "Intercept variance",
          str_detect(path, "S~~S$") ~ "Slope variance",
          str_detect(path, "I~~S$") ~ "Covariance between growth factors",
          str_detect(path, "I~1$") ~ "Intercept mean",
          str_detect(path, "I~1$") ~ "Slope mean",
          
          str_detect(path, "I~x1.cov$") ~ "Effects of covariate on intercept",
          str_detect(path, "S~x1.cov$") ~ "Effects of covariate on slope",
          str_detect(path, "z1~I.cov$") ~ "Effects of intercept on distal outcome",
          str_detect(path, "z1~S.cov$") ~ "Effects of slope on distal outcome",
        )
    ) %>% 
    mutate(
      estimator = 
        case_when(
          estimator == "censored" ~ "cML",
          estimator == "gbit" ~ "GBIT"
        ),
      estimator = factor(estimator, levels = c("GBIT","cML"))
    )
  
  pathname <- unique(full_dt$path)
  
  if(is.null(my.facet)) {
    
    if(ICC.add) {
      sum_dt <- full_dt %>% 
        group_by(nobs, ntimepoint, cenprop_chr, ICC, estimator) %>% 
        summarise(
          error = mean(error, na.rm = T)
        )
      
      facet_form <- as.formula("ntimepoint + ICC ~ nobs")
      
    } else {
      sum_dt <- full_dt %>% 
        group_by(nobs, ntimepoint, cenprop_chr, estimator) %>% 
        summarise(
          error = mean(error, na.rm = T)
        )
      
      facet_form <- as.formula("ntimepoint ~ nobs")
    }
  } else {
    
    facet_form <- as.formula(my.facet)
    columns <- str_remove_all(my.facet, " ")
    columns <- str_split(columns, "~")[[1]]
    columns <- unlist(str_split(columns, "\\+"))
    
    sum_dt <- full_dt %>% 
      group_by(vars(one_of(columns)))
      summarise(
        error = mean(error, na.rm = T)
      )
    
  }
  
  plot_dt <- full_dt
  # if(str_detect(given.path, "cov")) {
  #   # growthtoz1_chr
  #   # x1togrowth_chr
  #   if(str_detect(given.path, "x1")) {
  #     
  #     facet_form <- 
  #       as.formula("ntimepoint + ICC ~ nobs")
  #     
  #   } else {
  #     facet_form <- as.formula("ntimepoint + ICC ~ nobs")
  #   }
  #   
  # } else {
  #   facet_form <- as.formula("ntimepoint + ICC ~ nobs")
  # }
  
  if(min(plot_dt$error, na.rm = T) < -2.5 |
     max(plot_dt$error, na.rm = T) > 2.5) {
    
    
    mn <- ifelse(min(sum_dt$error) > -1.5, -1.5, min(sum_dt$error))
    mx <- ifelse(max(sum_dt$error) < 1.5, 1.5, max(sum_dt$error))
    
    error_limit <- c(mn, mx)
    
  } else {
    error_limit <- NULL
  }
  
  p <- plot_dt %>% 
    ggplot(aes(cenprop_chr, error, fill = estimator)) +
    geom_hline(
      aes(yintercept = 0), 
      linetype = "dashed", color = "blue", alpha = 1) +
    
    geom_boxplot(aes(middle = mean(error)),
                 position = position_dodge(width = 0.7),
                 fatten=0.5,
                 
                 outlier.alpha = 0.15)
  
  # dat <- ggplot_build(p)$data[[1]]
  # p <- p + geom_segment(data=dat, 
  #                       aes(x=xmin, xend=xmax, y=middle, yend=middle), 
  #                       colour="red", linewidth=2)
  p +
    geom_point(
      data = sum_dt,
      aes(cenprop_chr, error, shape = estimator),
      color = '#FF3636', size = 1.2,
      position = position_dodge(width = 0.7)
    ) +
    
    ggh4x::facet_nested(
      facet_form,
      nest_line = element_line(linetype = 1)
    ) +
    labs(
      y = "Relative Error",
      x = "Censoring Proportion",
      fill = "", shape = "",
      # title = glue("Parameter: {pathname}")
    ) +
    scale_x_discrete(
      #guide = guide_axis(n.dodge=2),
      labels = c("10%","20%","40%","60%","80%")) +
    
    scale_fill_grey(start = 1, end = 0.4) +
    
    annotate("rect", 
             xmin = -Inf, 
             xmax = Inf, 
             ymin = -0.1, ymax = 0.1,
             alpha = .08) +
    
    
    coord_cartesian(ylim=error_limit) +
    
    cowplot::theme_minimal_hgrid(
      font_size = 19, font_family = "Times") +
    cowplot::panel_border(color = "black") +
    theme(
      legend.position = 'bottom',
      legend.justification = "center",
      strip.background = element_rect(fill = "grey90"),
      ggh4x.facet.nestline = element_line(colour = "black"),
      strip.text = element_text(size = 14,face = 'bold'),
      axis.text.y = element_text(size = 12)
      
    ) +
    scale_y_continuous(n.breaks = 10)
  #theme(aspect.ratio = 1/2) 
  
  # theme(legend.text = element_text(size = 14),
  #       legend.title = element_text(size = 15, face = "bold"),
  #       # legend.key.size = unit(5, 'point'),
  #       legend.position = 'bottom') +
  #   guides(shape = guide_legend(override.aes = list(size = 5)))
  
  
  # list(plot_data = full_dt, plot_p = p)
}


# colnames(res_estimates)
expectLinear.phase2 <- function(x) {
  # x = xx[[1]]
  ntp <- unique(unlist(x$ntimepoint))
  tmat <- makeLamMat(ntp, 0)
  
  pop_line  <- tmat %*% x$population
  cen_line  <- tmat %*% x$censored
  gbit_line <- tmat %*% x$gbit
  
  rbind(pop_line, cen_line, gbit_line) %>% 
    data.frame(
      value = .,
      time = 1:ntp,
      cenprop = unique(unlist(x$cenprop_chr)),
      nobs = unique(unlist(x$nobs)),
      ntimepoint = unique(unlist(x$ntimepoint)),
      
      growthvar = unique(unlist(x$growthvar_chr)),
      growthtoz1 = unique(unlist(x$growthtoz1_chr)),
      x1togrowth = unique(unlist(x$x1togrowth_chr)),
      
      reps = unique(unlist(x$reps)),
      estimator = 
        rep(c("POP","CEN","GBIT"), each = ntp)
    )
}

fittedPlot.phase2 <- function(x, frac.prop = 0.5) {
  # xx[[1]]
  rep_sel <- sample(unique(x$reps), 
                    round(length(unique(x$reps)) * frac.prop,0))
  rep_sel <- c("1", rep_sel)
  
  x %>% 
    # filter(reps %in% rep_sel) %>% 
    ggplot() + 
    geom_path(
      aes(x = time, y = value, alpha = estimator,
          size = estimator,
          linetype = estimator,
          group = estimator, color = estimator)) +
    # scale_color_brewer(palette = "Set1") +
    scale_color_manual(values = c("red", "blue", "black")) +
    scale_linetype_manual(values = c("dashed", "dotted", "solid")) +
    scale_size_manual(values = c(0.4, 0.4, 1.2)) +
    scale_alpha_manual(values = c(0.4, 0.4, 1)) +
    facet_grid2(
      nobs + cenprop ~ growthvar,
      scales = 'free'
    )
}

# theme(legend.position = 'none')


#######################
##### Plot Colors #####
#######################
cc_palettes <- list(
  # BottleRocket1 = c("#A42820", "#5F5647", "#9B110E", "#3F5151", "#4E2A1E", "#550307", "#0C1707"),
  # https://colorhunt.co/palettes/random
  cc1 = c("#C4B7CB","#BBC7CE","#BFEDEF","#98E2C6","#545C52"),
  cc2 = c("#2A3990","#9C254D","#D23369","#00005C","#F06292","#A3C7D6"),
  # Dark
  dark1 = c("#00005C","#3B185F","#C060A1","#F0CAA3","#735F32","#A3C7D6"),
  # Light
  light1 = c("#A5F1E9","#FFF6BF","#BA94D1","#E97777","#B6E2A1","#354259"),
  # UT austin
  utaustin1 = c("#DC5F00","#D8D9CF","#3C2C3E")
)

#################################
### If want to design facets ####
#################################
if(FALSE) {
  "https://stackoverflow.com/questions/54471816/remove-three-sides-of-border-around-ggplot-facet-strip-label"
}


#######################
#### ggplot2 Theme ####
#######################
if(FALSE) {
  font_family <- "sans"   #assign font family up front
  font_size = 10
  
  base_line_size = 1
  base_rect_size = 1
  
  ggplot(data = diamonds[1:1000,], aes(x, z, color = color)) +
    geom_point() +
    facet_wrap(cut ~ .) +
    labs(x = "xx", y = "yy") +
    # theme_void() %+replace%    #replace elements we want to change
    
    theme(
      
      line =               element_line(
        colour = "black",
        linewidth = base_line_size,
        linetype = 1,
        lineend = "butt"
      )
      ,
      rect =               element_rect(
        fill = "transparent",
        colour = "black",
        linewidth = base_rect_size,
        linetype = 1
      )
      ,
      text =               element_text(
        family = font_family, face = "plain",
        colour = "black", size = font_size,
        lineheight = 0.9, hjust = 0.5, vjust = 0.5, angle = 0,
        margin = margin(), debug = FALSE
      )
      ,
      
      # x and y axis line
      axis.line =    NULL, # element_blank(); element_line(color = 'blue')
      axis.line.x =  element_line(color = 'blue',linewidth = 2), # NULL,
      axis.line.y =  element_line(color = 'blue'),
      
      axis.title =   element_text(size = rel(1.2), face = 'bold'),
      axis.title.x = element_text(vjust = 1, hjust = 0.5, margin = margin(t = 10)),
      
      axis.title.y = element_text(vjust = 1, hjust = 0.5,
                                  angle = 90,
                                  margin = margin(r = 10)),
      
      axis.text    = element_text(size = rel(1), colour = "grey30"),
      
      
      axis.ticks =         element_line(colour = "red"),
      axis.ticks.length =  unit(10, "pt"),
      
      
      
      legend.background =  element_rect(colour = "black", fill = 'yellow'),
      legend.spacing =     unit(1, "pt"),
      legend.spacing.x =   NULL,
      legend.spacing.y =   NULL,
      legend.margin =      margin(1, 1, 1, 1, "cm"),
      
      legend.key =         element_rect(fill = "blue", colour='white'),
      legend.key.size =    unit(1.2, "lines"),
      legend.key.height =  NULL,
      legend.key.width =   NULL,
      
      legend.text =        element_text(size = rel(0.8)),
      legend.text.align =  NULL,
      
      legend.title =       element_text(hjust = 0),
      legend.title.align = NULL,
      
      legend.position =    "right",
      legend.direction =   NULL,
      legend.justification = "center",
      
      legend.box =         NULL,
      legend.box.margin =  margin(1, 1, 1, 1, "cm"),
      legend.box.background = element_blank(),
      legend.box.spacing = unit(2 * 1, "pt"),
      
      panel.background =   element_rect(fill = NA, colour = NA),
      panel.border =       element_rect(fill = NA, colour = "blue"),
      # panel.grid.major =
      panel.grid.major.x = element_line(color = 'grey90', linewidth = 0.2),
      panel.grid.major.y = element_line(color = 'red', linewidth = 0.2),
      # panel.grid.minor =   element_line(color = 'red', linewidth = 0.1, linetype = 'dotted'), # element_blank()
      panel.grid.minor.x =   element_line(color = 'red', linewidth = 0.1, linetype = 'dotted'),
      panel.grid.minor.y =   element_line(color = 'red', linewidth = 0.1, linetype = 'dotted'),
      
      panel.spacing =      unit(10, "pt"),
      panel.spacing.x =    NULL,
      panel.spacing.y =    NULL,
      panel.ontop    =     FALSE,
      
      strip.background =   element_rect(fill = "white", colour = "white"),
      # strip.background.x = element_rect(colour = "black"),
      # strip.background.y = element_rect(colour = "black"),
      
      strip.clip =         "off", #inherit or on or off?
      strip.text =         element_text(
        colour = "black",
        size = rel(0.8),
        margin = margin(0.8, 0.8, 0.8, 0.8)
      ),
      
      strip.text.x =       element_text(color = "blue", face = 'bold',
                                        margin = margin(t = 0.5,r = 0,b = 0,l = 0, "cm")), # NULL,
      strip.text.y =       element_text(angle = -90),
      strip.placement =    "inside",
      strip.placement.x =  NULL,
      strip.placement.y =  NULL,
      strip.switch.pad.grid = unit(5, "pt"),
      strip.switch.pad.wrap = unit(5, "pt"),
      
      
      complete = TRUE
    )
  # 
  # ggplot2:::ggplot_global$theme_all_null %+replace% t
  
  # theme_gppr <- function(){
  #   font <- "Georgia"   #assign font family up front
  #   
  #   theme_minimal() %+replace%    #replace elements we want to change
  #     
  #     theme(
  #       
  #       #grid elements
  #       panel.grid.major = element_blank(),    #strip major gridlines
  #       panel.grid.minor = element_blank(),    #strip minor gridlines
  #       axis.ticks = element_blank(),          #strip axis ticks
  #       
  #       #since theme_minimal() already strips axis lines,
  #       #we don't need to do that again
  #       
  #       #text elements
  #       plot.title = element_text(             #title
  #         family = font,            #set font family
  #         size = 20,                #set font size
  #         face = 'bold',            #bold typeface
  #         hjust = 0,                #left align
  #         vjust = 2),               #raise slightly
  #       
  #       plot.subtitle = element_text(          #subtitle
  #         family = font,            #font family
  #         size = 14),               #font size
  #       
  #       plot.caption = element_text(           #caption
  #         family = font,            #font family
  #         size = 9,                 #font size
  #         hjust = 1),               #right align
  #       
  #       axis.title = element_text(             #axis titles
  #         family = font,            #font family
  #         size = 10),               #font size
  #       
  #       axis.text = element_text(              #axis text
  #         family = font,            #axis famuly
  #         size = 9),                #font size
  #       
  #       axis.text.x = element_text(            #margin for axis text
  #         margin=margin(5, b = 10))
  #       
  #       #since the legend often requires manual tweaking
  #       #based on plot content, don't define it here
  #     )
  # }
}

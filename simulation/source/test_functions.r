# Phase 1---------------------------------------------------------------
## Likelihood test function ------------------
likePlot <- function(n = 1000, cor_inp = 0.5, censored_prop = c(0.2, 0.8), text_size = 14) {
  # n <- n # number of data points
  # censored_prop <- c(0.2, 0.8)
  library(gridExtra)
  
  
  pop_info <- genDataInfo(cor_inp = cor_inp)
  
  # if(TRUE) {
  uncensored_X <- genData(n,pop_info$mu, pop_info$sig2)
  uncensored_info <- getDataInfo(uncensored_X)
  
  cen.point_bi <- cenPoint(x = uncensored_X,
                           lower_quant = censored_prop[1],
                           upper_quant = censored_prop[2])
  
  censored_X <- cenData(x = uncensored_X, cen_point = cen.point_bi)
  censored_info <- getDataInfo(censored_X)
  
  x1 <- cUniv(c(censored_info$mu[1],censored_info$sig2[1]),
              censored_X[,1],cen.point_bi[[1]])
  
  y1 <- cUniv(c(censored_info$mu[2],censored_info$sig2[4]),
              censored_X[,2],cen.point_bi[[2]])
  
  uncen_corr <- uncensored_info$corr[3]
  
  if(abs(uncen_corr) < 0.1 ) {
    ranges <- sort(c(uncen_corr - 0.1, uncen_corr + 0.1))
  } else {
    ranges <- sort(c(uncen_corr - uncen_corr*1/2, uncen_corr + uncen_corr*1/2))
  }
  
  ranges[ranges < -0.95] = -0.95
  ranges[ranges > 0.95] = 0.95
  
  thetas <- c(runif(1000, ranges[1], ranges[2]), uncen_corr)
  likelihoods <- sapply(thetas, function(x) {
    ll_censored_bi(theta = x, 
                   XY = censored_X, 
                   bounds = cen.point_bi, 
                   fixed = c(mu1 = x1[1], mu2 = y1[1], 
                             sig21 = x1[2], sig22 = y1[2])) 
  })
  
  like_data <- tibble(likelihoods) %>% 
    mutate(thetas = thetas)
  
  like_points <- like_data %>% 
    filter(likelihoods == min(likelihoods) |
             thetas == uncen_corr) %>% 
    mutate(colors = case_when(thetas == uncen_corr ~ "population",
                              TRUE ~ "estimated"))
  
  estimated <- like_points %>% 
    filter(likelihoods == min(likelihoods)) %>% 
    pull(thetas)
  tobit_cov <- estimated * (sqrt(x1[2])*sqrt(y1[2]))
  
  tobit_info <- list()
  tobit_info$mu <- c(x1[1],y1[1])
  tobit_info$sig2 <- bimat(c(x1[2],tobit_cov,tobit_cov,y1[2]))
  
  est_table  <- tibble(
    var = names(unlist(uncensored_info[1:2])),
    uncensored = unlist(uncensored_info[1:2]),
    tobit = unlist(tobit_info),
    censored = unlist(censored_info[1:2])
  ) %>% 
    filter(var != "sig23") %>% 
    mutate_if(is.numeric, ~ round(.x, 3)) %>% 
    mutate(
      var = 
        case_when(var == "mu1" ~ "Mean of x",
                  var == "mu2" ~ "Mean of y",
                  var == "sig21" ~ "Variance of x",
                  var == "sig22" ~ "Variance of y",
                  var == "sig24" ~ "Covariance (x,y)")
    )
  
  
  theme_set(theme_classic(base_size = text_size))
  
  like_data %>% 
    ggplot() +
    geom_line(aes(thetas, likelihoods, group = 1)) +
    geom_point(data = like_points,
               aes(thetas, likelihoods, color = colors), size = 3) +
    geom_vline(xintercept = uncen_corr, alpha = 0.8, 
               linetype = "dotted", color = "blue") +
    scale_x_continuous(breaks = round(like_points$thetas, 3),
                       guide = guide_axis(n.dodge=2)) + 
    annotation_custom(tableGrob(est_table,rows=NULL), 
                      xmin=max(like_data$thetas)-0.5, 
                      xmax=max(like_data$thetas), 
                      ymin=max(like_data$likelihoods) - 15, 
                      ymax=max(like_data$likelihoods)) +
    labs(title = "Likelihoods for correlation",color = "") +
    theme(
      legend.position = c(0,0.5), # top left position
      legend.justification = c(0, 1), # top left justification
      legend.box.margin = margin(5, l = 5, unit = "mm") # small margin
    ) +
    labs(x = "estimates")
  # }
}

# test function for Phase 1
test_phase1 <- function(n, local, censored_prop, cor_inp = NULL, scaling = F) {
  # n <- N # 2000 # number of data points
  # local = LOCAL # F
  # censored_prop <- CENPROP # c(0.1, 0.9)
  # cor_inp = CORR #0.2
  pop_info <- genDataInfo(p = 2, cor_inp = cor_inp)
  uncensored_X <- genData(n,pop_info$mu, pop_info$sig2)
  uncensored_info <- getDataInfo(uncensored_X)
  
  if(local) {
    cen.point_bi <- cenPoint(
      x = uncensored_X,
      lower_quant = c(censored_prop[1], censored_prop[1]),
      upper_quant = c(censored_prop[2], censored_prop[2]))
    
  } else {
    cen.point_bi <- cenPoint(
      x = uncensored_X,
      lower_quant = censored_prop[1],
      upper_quant = censored_prop[2])
  }
  
  censored_X <- cenData(x = uncensored_X, cen_point = cen.point_bi)
  censored_info <- getDataInfo(censored_X)
  
  tobit_info <- cMulti(data = censored_X, bounds = cen.point_bi)
  
  uc_info = uncensored_info
  tob_info = tobit_info
  cen_info = censored_info
  
  tibble(
    params = names(unlist(uc_info)),
    uc_info = unlist(uc_info),
    tob_info = unlist(tob_info),
    cen_info = unlist(cen_info)
  ) %>% 
    filter(!params %in% c("sig23","corr1","corr3","corr4"))
}



# Plot function ----------------------------------------------
densityPlot <- function(selected) {
  # mu1; mu2; sig21; sig22; sig24; corr2
  res %>% 
    filter(params == selected) %>%
    gather("estimator","value", -params, -reps) %>% 
    ggplot(aes(value)) +
    geom_density(aes(fill = estimator), alpha = 0.2) +
    # ggforce::geom_mark_ellipse(
    #    aes(fill = estimator, label = estimator)) +
    facet_wrap(. ~ params)# +
}


by_reps <- function(selected='sig22') {
  
  data.range <- as.numeric(unique(res$reps))
  
  res %>% 
    # filter(reps %in% paste(1:10)) %>% 
    filter(params == selected) %>%
    gather("estimator","value", -params, -reps) %>% 
    ggplot(aes(reps, value)) +
    geom_point(aes(color = estimator, shape = estimator)) +
    # ggforce::geom_mark_ellipse(
    #    aes(fill = estimator, label = estimator)) +
    facet_wrap(. ~ params) +
    scale_x_discrete(breaks = 
                       seq(from = data.range[1], 
                           to = data.range[2], 
                           by = 10)) +
    coord_flip()
}

errorPlot <- function(selected='sig22') {
  
  data.range <- as.numeric(unique(res$reps))
  
  res %>% 
    mutate(error_tobit = tob_info - uc_info,
           error_censor = cen_info - uc_info) %>% 
    select(-ends_with("info")) %>% 
    filter(params == selected) %>%
    gather("estimator","value", -params, -reps) %>% 
    ggplot(aes(reps, value)) +
    geom_hline(yintercept = 0, color = 'red', size = 1.2, alpha = 0.3) +
    geom_point(aes(color = estimator, shape = estimator)) +
    # ggforce::geom_mark_ellipse(
    #    aes(fill = estimator, label = estimator)) +
    facet_wrap(. ~ params) +
    scale_x_discrete(breaks = 
                       seq(from = data.range[1], 
                           to = data.range[2], 
                           by = 10))
}


boxPlot <- function(selected_param) {
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
  ggh4x::facet_grid2(nobs + cenprop_chr ~  corr, scales = "free")
  
}

dotPlot <- function(simres_1, selected_param, alpha = 0.8) {
  theme_set(theme_bw())
  
  p_data <- simres_1 %>% 
    filter(params %in% selected_param) %>% 
    mutate(
      estimator = case_when(
        estimator == "cen_info" ~ "No treatment",
        TRUE ~ "GBIT"
      )
    ) %>% ggplot()
  
  # if(selected_param == "corr2") {
    # p_data +
    #   geom_point(aes(value, uc_info, color = estimator), alpha = alpha) +
    #   ggh4x::facet_grid2(nobs + cenprop_chr ~  corr, axes = "all",
    #                      scales = "free") +
    #   scale_color_brewer(type = "qual") +
    #   lims(y = c(0,1))
  # } else {
    p_data +
      geom_point(aes(uc_info, value, color = estimator), alpha = alpha) +
      geom_abline(slope = 1, intercept = 0, alpha = 0.7) +
      ggh4x::facet_grid2(cenprop_chr ~  corr, 
                         scales = "free") +
      scale_color_brewer(type = "qual") +
      labs(x = "Population", y = "Estimates")
  # }
  
}


# Phase 2 -------------------------------
test_phase2 <- function(model_info) {
  # data generation ---------------------------------------------
  model_info <- SEM.DataGen(model_info = model_info)
  
  # censoring ------------------------------------------------------
  model_info <- cenLGM(model_info)
  
  # estimation -----------------------------------------------------
  model_info$tobit_info <- cMulti(data = model_info$censored_X, 
                                  bounds = model_info$bounds0)
  
  model_info <- genModels(model_info, model = 'quad', covariates = F)
  ori.fit.qaud <- runLGM(model_info, estimator = "original")
  cen.fit.qaud <- runLGM(model_info, estimator = "censored")
  tob.fit.qaud <- runLGM(model_info, estimator = "tobit")
  
  model_info <- genModels(model_info, model = 'linear', covariates = F)
  ori.fit <- runLGM(model_info, estimator = "original")
  cen.fit <- runLGM(model_info, estimator = "censored")
  tob.fit <- runLGM(model_info, estimator = "tobit")
  
  res <- extFitInfo(ori.fit, "params") %>% 
    mutate(estimator = "population",.before=lhs) %>% 
    bind_rows(
      extFitInfo(cen.fit, "params") %>% mutate(estimator = "censored",.before=lhs),
      extFitInfo(tob.fit, "params") %>% mutate(estimator = "gbit",.before=lhs)
    ) %>% 
    mutate(path = paste0(lhs, op, rhs), .before = lhs)
  
  res_fit <- bind_rows(
    chiDiffTest(ori.fit, ori.fit.qaud) %>% mutate(estimator = "origin"),
    chiDiffTest(cen.fit, cen.fit.qaud) %>% mutate(estimator = "cen"),
    chiDiffTest(tob.fit, tob.fit.qaud) %>% mutate(estimator = "gbit")
  )
  res_estimates <- res %>% select(estimator, path, est)
  res_estimates <- res_estimates %>% 
    filter(!str_detect(path, "^y")) %>% 
    pivot_wider(names_from = estimator, values_from = est)
  
  
  list(res_estimates = res_estimates, res_fit = res_fit)
}

plotFit <- function(res_fit) {
  res_fit %>% 
    mutate(estimator = as.factor(estimator),
           selected = as.factor(selected)) %>% 
    count(estimator, selected, .drop = FALSE) %>% 
    mutate(
      prop = n / nreps,
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
    ggplot(aes(x=estimator, y = n, fill=selected)) +
    geom_col(
      color = 'white', linewidth = 1,
      width = .5) + 
    # geom_richtext(aes(
    geom_text(aes(
      label = paste0(100*round(prop,3),"%"),
      color = txt_col#,
      # vjust = txt_vjust
    ),
    position = position_stack(vjust = .5),
    # fill = NA,
    # label.color = NA,
    # vjust = 1,
    size = 4, show.legend = FALSE,fontface='bold') +
    scale_fill_grey(  start = 0.7,
                      end = 0.4) +
    labs(y = "Freq", fill = "", x = "Estimator") +
    scale_color_manual(values = c("black",'transparent',"white")) +
    theme(
      legend.position = c(0, 1),
      legend.background = element_rect(fill = 'transparent'),
      legend.justification = c("left", "top"),
      legend.box.just = "left",
      legend.margin = margin(6, 6, 6, 6)
    ) 
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

plotEstimates <- function(res_estimates) {
  res_estimates %>% 
    pivot_longer(
      cols = c(censored, gbit),
      names_to = "estimator", values_to = "values") %>% # print(n=20)
    ggplot(aes(estimator, values)) +
    geom_hline(
      aes(yintercept = population), 
      linetype = "dashed", color = "blue", alpha = 0.5) +
    geom_text(
      aes(y = population, x = -Inf, 
          # label = round(population,3), 
          label = "population"),
      color = 'black',
      hjust = -0.5,
      vjust = 1
    ) +
    geom_violin() +
    # geom_boxplot() +
    ggforce::geom_sina(alpha = 0.5) +
    stat_summary(fun=mean, geom="point", shape=17, size=4, 
                 fill = 'red',
                 color = 'red') +
    ggh4x::facet_wrap2(. ~ path, scales = "free") +
    theme(
      strip.background = element_blank(),
      strip.text = element_textbox(
        size = 12,
        color = "white", fill = "#5D729D", box.color = "#4A618C",
        halign = 0.5, linetype = 1, r = unit(5, "pt"), width = unit(1, "npc"),
        padding = margin(2, 0, 1, 0), margin = margin(3, 3, 3, 3)
      )
    )
}


expectLinear <- function(x, ntp) {
  
  tmat <- makeLamMat(ntp, 0)
  
  pop_line  <- tmat %*% x$population
  cen_line  <- tmat %*% x$censored
  gbit_line <- tmat %*% x$gbit
  
  rbind(pop_line, cen_line, gbit_line) %>% 
    data.frame(
      value = .,
      time = 1:ntp,
      reps = unique(unlist(x$nrep)),
      estimator = 
        rep(c("POP","CEN","GBIT"), each = ntp)
    )
}

fittedPlot <- function(x) {
  x %>% 
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
    theme(
      legend.position = c(0,1), # top left position
      legend.justification = c(0, 1), # top left justification
      legend.box.margin = margin(5, l = 5, unit = "mm") # small margin
    )
}



tp <- matrix(c(1,1,1,1,0,1,2,3), ncol = 2)

bind_rows(
  tibble(
    Age = c(17, 18, 19, 20),
    Fitted = c(tp %*% res_uncond$cest[c(8,9)]),
    Estimator = "GBIT"
  ),
  
  tibble(
    Age = c(17, 18, 19, 20),
    Fitted = c(tp %*% res_uncond$mest[c(8,9)]),
    Estimator = "MLE"
  )
) %>% 
  ggplot(aes(x = Age, Fitted, color = Estimator)) +
  geom_line(aes(linetype = Estimator), linewidth = 1.4) +
  geom_point(aes(shape = Estimator),  size = 5) +
  scale_color_grey(start = 0.1, end = 0.5) +
  cowplot::theme_cowplot(font_size = 18) +
  theme(legend.position = c(0.1, 0.9))







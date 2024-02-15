table_add <- function(x.doc, x.tb, landscape = F) {
  
  if(landscape)
    x.doc <- body_end_section_portrait(x.doc)
  
  flextable::body_add_flextable(
    x.doc,
    value = x.tb,
    align = "left"
  )
  
  body_add_par(x.doc, " ")
  
  if(landscape)
    x.dox <- body_end_section_landscape(x.doc)
}

mk_tbl <- function(obj, caption) {
  obj %>% 
    flextable() %>% 
    flextable::font(fontname = "Times", part = "all") %>%
    padding(., padding.top = 0.1, padding.bottom = 0.1, 
            padding.left=0,padding.right=0, part = "header") %>% 
    padding(., padding.top = 0.1, padding.bottom = 0.1, 
            padding.left=0,padding.right=5, part = "body") %>% 
    fontsize(size = 11, part = "header") %>%
    fontsize(size = 11, part = "body") %>% 
    # width(width = 1) %>% 
    set_caption(caption) %>% 
    autofit()
}



# # length(4:dim(ft_data)[2])
# # Header 1
# header1 <- c()
# header1[1:3] <- c("")
# header1[4:dim(ft_data)[2]] <- 
#   rep(c("0.05-0.95",
#         "0.1-0.9",
#         "0.2-0.8",
#         "0.3-0.7",
#         "0.4-0.6"), each = 3)
# 
# # Header 2
# header2 <- c()
# header2[1:3] <- c("")
# header2[4:dim(ft_data)[2]] <- 
#   # rep(c(rep(c("Bias","MSE"), each = 2),"MSER"), 5)
#   rep(c(rep(c("rBias"), each = 2),"MSER"), 5)
# # length(header2)
# 
# # Header 3
# header3 <- c()
# header3[1:3] <- c("N", "TP","ICC")
# header3[4:dim(ft_data)[2]] <- 
#   # rep(c(rep(c("Cen","GBIT"), 2),"."), 5)
#   rep(c(rep(c("cML","GBIT"), 1),"."), 5)
# 
# 
# multiple_header <- data.frame(
#   col_keys = names(ft_data),
#   header1 = header1,
#   header2 = header2,
#   header3 = header3,
#   stringsAsFactors = FALSE )
# 
# mser_pos <- str_which(names(ft_data), "mse_ratio")
# temp_dt  <- ft_data %>% 
#   select(contains("mse_ratio"))
# 
# coloring <- function(x, coln, temp_dt, mser_pos) {
#   bg(x,
#      i = which(temp_dt[,coln] < 1), 
#      j = mser_pos[coln], 
#      bg = "#DCDCDC",#"#D3D3D3", 
#      part = "body")
# }
# 
# bolding <- function(x, coln, temp_dt, mser_pos) {
#   bold(x,
#        i = which(temp_dt[,coln] < 1), 
#        j = mser_pos[coln])
# }
# 
# n_rows <- nrow(ft_data)
# inline_row <- seq(from = 2, to = n_rows-2, 2)
# 
# ft_1 <- ft_data %>% flextable( . )
# ft_1 <- set_header_df(ft_1, mapping = multiple_header, key = "col_keys" )
# ft_1 <- merge_h(ft_1, i = 1, part = "header")
# ft_1 <- merge_h(ft_1, i = 2, part = "header")
# 
# ft_1 %>% 
#   padding(., padding.top = 0.1, padding.bottom = 0.1, 
#           padding.left=0,padding.right=0, part = "header") %>% 
#   padding(., padding.top = 0.1, padding.bottom = 0.1, 
#           padding.left=0,padding.right=5, part = "body") %>% 
#   flextable::font(fontname = "Times", part = "all") %>%
#   fontsize(size = 11, part = "header") %>%
#   fontsize(size = 11, part = "body") %>% 
#   bold(i = c(1:3), part = 'header') %>% 
#   bold(j = c(1:3), part = 'body') %>% 
#   flextable::border(.,
#                     border.top = fp_border(color = "black", width = 1.5),
#                     border.bottom = fp_border(color = "black", width = 1.5),
#                     part = "header"# partname of the table (all body header footer)
#   ) %>% 
#   align(align = "center", part = "header") %>% 
#   align(align = "right", part = "body") %>% 
#   flextable::border(.,
#                     border.right = fp_border(color = "black", width = .75),
#                     j =c(3,6,9,12,15)
#                     
#   ) %>% 
#   flextable::border(.,
#                     border.bottom = fp_border(color = "black", width = .75),
#                     i =c(6, 12, 18)
#                     
#   ) %>% 
#   flextable::border(.,
#                     border.bottom = fp_border(color = "black", width = .75),
#                     i = inline_row
#                     
#   ) %>% 
#   merge_v(j = c(1:3)) %>% 
#   
#   width(j = 4:18, width = x.width) %>%
#   width(j = 1:3, width = 0.5) %>%
#   set_caption(caption = caption) %>%
#   add_footer_lines(values = "Note. N: sample size; TP: the number of timepoints; GBIT: generalized tobit estimator; cML: ML estimator with censored data") %>%
#   
#   coloring(1, temp_dt, mser_pos) %>% 
#   coloring(2, temp_dt, mser_pos) %>% 
#   coloring(3, temp_dt, mser_pos) %>% 
#   coloring(4, temp_dt, mser_pos) %>% 
#   coloring(5, temp_dt, mser_pos) %>% 
#   
#   bolding(1, temp_dt, mser_pos) %>% 
#   bolding(2, temp_dt, mser_pos) %>% 
#   bolding(3, temp_dt, mser_pos) %>% 
#   bolding(4, temp_dt, mser_pos) %>% 
#   bolding(5, temp_dt, mser_pos) %>% 
#   
#   fix_border_issues()
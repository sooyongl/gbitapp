###################################
#### Steps to make Word tables ####
###################################
# my.doc <- read_docx()
# table_add(my.doc, typeierror)
# print(my.doc, target = "results/simulation_table.docx")


# Add Table into Word -----------------------------------------------------
table_add <- function(x.doc, x.tb, landscape = F) {
  
  # if(landscape)
    x.doc <- body_end_section_portrait(x.doc)
  
  flextable::body_add_flextable(
    x.doc,
    value = x.tb,
    align = "left"
  )
  
  body_add_par(x.doc, " ")
  
  # if(landscape)
  #   x.dox <- body_end_section_landscape(x.doc)
}

# Phase 2 table -------------------------------------------------
table.phase2 <- function(ft_data, caption="", x.width = 1) {
  # ft_data <- table_cond
  # filter.inp = "I~1"
  # x.width = 1
  # caption = "Table 4-2. Intercept"
  
  hlines <- sapply(1:(dim(ft_data)[1] / 4), function(x) (x-1)*4)[-1]
  
  ft_data <- ft_data %>% 
    mutate_if(is.numeric, round, 3) %>% 
    select(type, nobs,
           starts_with("rbias"), 
           starts_with("sd")) %>% 
    select(-matches("sd_cen|sd_gbit")) %>% 
    select(type, nobs,
           # matches("^bias"),
           # matches("^mse_"),
           # matches("^mse_ratio")
           matches("0.05-0.95"),
           matches("0.1-0.9"),
           matches("0.2-0.8"),
           matches("0.3-0.7"),
           matches("0.4-0.6")
    )
  
  # length(4:dim(ft_data)[2])
  # Header 1
  header1 <- c()
  header1[1:2] <- c("")
  header1[3:dim(ft_data)[2]] <- 
    rep(c("0.05-0.95",
          "0.1-0.9",
          "0.2-0.8",
          "0.3-0.7",
          "0.4-0.6"), each = 3)
  
  # Header 2
  header2 <- c()
  header2[1:2] <- c("")
  header2[3:dim(ft_data)[2]] <- 
    # rep(c(rep(c("Bias","MSE"), each = 2),"MSER"), 5)
    rep(c(rep(c("rBias"), each = 2),"sdD"), 5)
  # length(header2)
  
  # Header 3
  header3 <- c()
  header3[1:2] <- c("type", "N")
  header3[3:dim(ft_data)[2]] <- 
    # rep(c(rep(c("Cen","GBIT"), 2),"."), 5)
    rep(c(rep(c("ML","GBIT"), 1),"."), 5)
    
  
  multiple_header <- data.frame(
    col_keys = names(ft_data),
    header1 = header1,
    header2 = header2,
    header3 = header3,
    stringsAsFactors = FALSE )
  
  mser_pos <- str_which(names(ft_data), "sd_diff")
  temp_dt  <- ft_data %>% 
    select(contains("sd_diff"))
  
  coloring <- function(x, coln, temp_dt, mser_pos) {
    bg(x,
      i = which(temp_dt[,coln] <= 0), 
      j = mser_pos[coln], 
      bg = "#DCDCDC",#"#D3D3D3", 
      part = "body")
  }

  bolding <- function(x, coln, temp_dt, mser_pos) {
    bold(x,
       i = which(temp_dt[,coln] < 1), 
       j = mser_pos[coln])
  }
  
  n_rows <- nrow(ft_data)
  inline_row <- seq(from = 2, to = n_rows-2, 2)
  
  ft_1 <- ft_data %>% flextable( . )
  ft_1 <- set_header_df(ft_1, mapping = multiple_header, key = "col_keys" )
  ft_1 <- merge_h(ft_1, i = 1, part = "header")
  ft_1 <- merge_h(ft_1, i = 2, part = "header")
  
  ft_1 %>% 
    padding(., padding.top = 0.1, padding.bottom = 0.1, 
            padding.left=0,padding.right=0, part = "header") %>% 
    padding(., padding.top = 0.1, padding.bottom = 0.1, 
            padding.left=0,padding.right=5, part = "body") %>% 
    flextable::font(fontname = "Times", part = "all") %>%
    fontsize(size = 11, part = "header") %>%
    fontsize(size = 11, part = "body") %>% 
    bold(i = c(1:3), part = 'header') %>% 
    bold(j = c(1:2), part = 'body') %>% 
    flextable::border(.,
           border.top = fp_border(color = "black", width = 1.5),
           border.bottom = fp_border(color = "black", width = 1.5),
           part = "header"# partname of the table (all body header footer)
    ) %>% 
    align(align = "center", part = "header") %>% 
    align(align = "right", part = "body") %>% 
    flextable::border(.,
           border.right = fp_border(color = "black", width = .75),
           j =c(3,6,9,12,15) -1
           
    ) %>% 
    flextable::border(.,
           border.bottom = fp_border(color = "black", width = .75),
           i = hlines
           
    ) %>% 
    merge_v(j = c(1:2)) %>% 
  
    width(j = 3:17, width = 0.5) %>%
    width(j = 1:2, width = 0.5) %>%
    set_caption(caption = caption) %>%
    add_footer_lines(values = "Note. N: sample size; GBIT: generalized tobit estimator; ML: ML estimator with censored data; Number of timepoints and ICC were averaged") %>%
    
    coloring(1, temp_dt, mser_pos) %>% 
    coloring(2, temp_dt, mser_pos) %>% 
    coloring(3, temp_dt, mser_pos) %>% 
    coloring(4, temp_dt, mser_pos) %>% 
    coloring(5, temp_dt, mser_pos) %>% 
    
    bolding(1, temp_dt, mser_pos) %>% 
    bolding(2, temp_dt, mser_pos) %>% 
    bolding(3, temp_dt, mser_pos) %>% 
    bolding(4, temp_dt, mser_pos) %>% 
    bolding(5, temp_dt, mser_pos) %>% 
    
    # colformat_double(digits = 4) %>% 
    fix_border_issues()
  
  
  # bg(x, i = NULL, j = NULL, bg, part = "body", source = j)
}

# Word Table format -------------------------------------------------------
mk_emp_table <- function(ft_data, caption="", tablenote = "", x.width = 1) {
  
  keep_data = ft_data
  ft_data = ft_data %>% select(-bdetect, -mdetect, -mgdetect, -DIFtype)
  # Header 1
  header1 <- c()
  header1[1] <- c("")
  header1[2:dim(ft_data)[2]] <- rep(c("BMIMIC","MIMIC","MG"), each = 3)
  
  # Header 2
  header2 <- c()
  header2[1] <- c("Item")
  header2[2:dim(ft_data)[2]] <- c("Mean","Cr2.5","Cr97.5","Est","SE","p-val","chisq2","df","p")
  
  multiple_header <- data.frame(
    col_keys = names(ft_data),
    header1 = header1,
    header2 = header2,
    stringsAsFactors = FALSE )
  
  ft_1 <- ft_data %>% flextable( . )
  ft_1 <- set_header_df(ft_1, mapping = multiple_header, key = "col_keys" )
  ft_1 <- merge_h(ft_1, i = 1, part = "header")
  
  ft_1 %>% 
    bold(
      i = which(keep_data$bdetect == 1),
      j = 2:4
    ) %>%
    bg(i = which(keep_data$bdetect == 1),
       j = 2:4,
       bg = "#D3D3D3D3", 
       part = "body") %>% 
    bold(
      i = which(keep_data$mdetect == 1),
      j = 5:7
    ) %>% 
    bg(i = which(keep_data$mdetect == 1),
       j = 5:7,
       bg = "#D3D3D3D3", 
       part = "body") %>% 
    bold(
      i = which(keep_data$mgdetect == 1),
      j = 8:10
    ) %>% 
    bg(i = which(keep_data$mgdetect == 1),
       j = 8:10,
       bg = "#D3D3D3D3", 
       part = "body") %>% 
    padding(., padding.top = 0.1, padding.bottom = 0.1, 
            padding.left=0,padding.right=0, part = "header") %>% 
    padding(., padding.top = 0.1, padding.bottom = 0.1, 
            padding.left=0,padding.right=5, part = "body") %>% 
    font(fontname = "Times", part = "all") %>%
    fontsize(size = 11, part = "header") %>%
    fontsize(size = 11, part = "body") %>% 
    bold(i = c(1), part = 'header') %>% 
    border(.,
           border.top = fp_border(color = "black", width = 1.5),
           border.bottom = fp_border(color = "black", width = 1.5),
           part = "header"# partname of the table (all body header footer)
    ) %>% 
    align(align = "center", part = "header") %>% 
    align(align = "right", part = "body") %>% 
    border(.,
           
           border.right = fp_border(color = "black", width = .75),
           j =c(1, 4, 7)
           
    ) %>% 
    width(j = 2:10, width = x.width) %>% 
    set_caption(caption = caption) %>%
    add_footer_lines(values = tablenote) %>% 
    fix_border_issues()
  # hline_bottom(.,
  #              part="body",
  #              border = fp_border(color="black", width = 1.5) )
  # width(width = 1.2)
}



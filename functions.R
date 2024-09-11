library(tidyverse)
library(rvest)
library(plyr)

stat_summarize <- function(df,filter_col,stat_col,stat_func) {
  filter_vals <- unique(df[[{{filter_col}}]])
  result <- as.data.frame(matrix(ncol=2,nrow=length(filter_vals)))
  colnames(result) <- c(filter_col,stat_col)
  for (i in 1:length(filter_vals)) {
    filter_val <- filter_vals[i]
    target_rows <- which(df[[{{filter_col}}]] == filter_val)
    temp_df <- df[target_rows,]
    result[[{{filter_col}}]][i] <- filter_val
    result[[{{stat_col}}]][i] <- stat_func(temp_df[[{{stat_col}}]])
  }
  return(result)
}

add_na_rm <- function(func) {
  output_func <- function(x) {
    return(func(x,na.rm=TRUE))
  }
  return(output_func)
}

directory_bind <- function(dir) {
  files <- paste(dir,'/',list.files(dir),sep='')
  df <- read_csv(files[1])
  for (i in 2:length(files)) {
    temp_df <- read_csv(files[i],show_col_types=FALSE)
    df <- rbind.fill(df,temp_df)
  }
  return(df)
}

value_counts <- function(vec) {
  unique_vals <- unique(vec)
  
  result <- as.data.frame(matrix(ncol=3,nrow=length(unique_vals)))
  colnames(result) <- c("Val","N",'Norm')
  
  result$Val <- unique_vals
  
  result$N <- result$Val %>%
    lapply(function(x) {
      if (is.na(x)) {
        return(length(which(is.na(vec))))
      } else {
        return(length(which(vec == x)))
      }
    }) %>%
    unlist()
  
  result$Norm <- result$N / length(vec)
  
  return(result)
}

stat_by_cat_single <- function(cat_val,df,cat_col,func) {
  if (is.na(cat_val)) {
    temp_df <- df[which(is.na(df[[{{cat_col}}]])),]
  } else {
    temp_df <- df[which(df[[{{cat_col}}]] == cat_val),]
  }
  transformed_df <- temp_df %>% func()
  transformed_df[{{cat_col}}] <- cat_val
  return(transformed_df)
}

stat_by_cat <- function(df,cat_col,func) {
  cat_vals <- unique(df[[{{cat_col}}]])
  stat_results <- cat_vals %>% lapply(stat_by_cat_single,df,cat_col,func)
  result <- do.call(rbind,stat_results)
  return(result)
}

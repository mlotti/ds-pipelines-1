library(dplyr)
library(readr)
library(stringr)
library(sbtools)
library(whisker)

tabulate <- function(data) {

  output_dir <- "../out"

  readr::write_csv(data, path = file.path(output_dir, 'model_summary_results.csv'))

}

modify <- function(data) {

  output_dir <- "../out"

  n_profs <- c(2, 10, 50, 100, 500, 980)

  for (mod in c('pb','dl','pgdl')){
    mod_data <- filter(data, model_type == mod)
    mod_profiles <- unique(mod_data$n_prof)
    for (mod_profile in mod_profiles){
      d <- filter(mod_data, n_prof == mod_profile) %>% summarize(y0 = min(rmse), y1 = max(rmse), col = unique(col))
    }
    d <- group_by(mod_data, n_prof) %>% summarize(y = mean(rmse), col = unique(col), pch = unique(pch)) %>%
      rename(x = n_prof) %>% arrange(x)  
  }

  return(d)

}

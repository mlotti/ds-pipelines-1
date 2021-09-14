library(dplyr)
library(readr)
library(stringr)
library(sbtools)
library(whisker)

tabulate <- function(data) {

  output_dir <- "../out"

  readr::write_csv(data, path = file.path(output_dir, 'model_summary_results.csv'))

}

modify <- function(input) {

  output_dir <- "../out"

  data = readr::read_csv(input, col_types = 'iccd')%>%
  filter(str_detect(exper_id, 'similar_[0-9]+')) %>%
  mutate(col = case_when(
    model_type == 'pb' ~ '#1b9e77',
    model_type == 'dl' ~'#d95f02',
    model_type == 'pgdl' ~ '#7570b3'
  ), pch = case_when(
    model_type == 'pb' ~ 21,
    model_type == 'dl' ~ 22,
    model_type == 'pgdl' ~ 23
  ), n_prof = as.numeric(str_extract(exper_id, '[0-9]+')))

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

  return(data)

}

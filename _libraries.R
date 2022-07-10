# Assumes use of docker containers setup in sh/docker_init.sh

cran_list <- c("magrittr",
               "tibble",
               "here",
               "readr",
               "janitor",
               "dplyr",
               "ggplot2",
               "xml2",
               "rvest",
               "devtools",
               "config",
               "stringr",
               "purrr",
               "RSelenium",
               "tibble")

install_list <- cran_list[!(cran_list %in% installed.packages())]
if (length(install_list)) {
  install.packages(install_list)
}

# https://ffanalytics.fantasyfootballanalytics.net/
if (!("ffanalytics" %in% installed.packages())) {
  devtools::install_github("FantasyFootballAnalytics/ffanalytics")  
}

library_list <- c(cran_list, "ffanalytics")
lapply(library_list, require, character.only = TRUE)

# source each file in the R directory
sapply(list.files(here::here("R"), full.names = TRUE), source)
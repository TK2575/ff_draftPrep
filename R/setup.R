# https://fantasyfootballanalytics.net/2016/06/ffanalytics-r-package-fantasy-football-data-analysis.html
install.packages(c("devtools","rstudioapi"), dependencies=TRUE, repos=c("http://rstudio.org/_packages", "http://cran.rstudio.com"))

install.packages(c("tidyverse","rvest","httr","readxl","janitor","glue","Hmisc"), dependencies=TRUE, repos=c("http://rstudio.org/_packages", "http://cran.rstudio.com"))

devtools::install_github(repo = "FantasyFootballAnalytics/ffanalytics", build_vignettes = TRUE)
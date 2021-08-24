library("here")
library("dplyr")
library("xml2")
library("rvest")
library("janitor")
library("stringr")
source(here::here("R", "clean_yahoo_utils.R"))

predraft_rankings_html_to_table <- function(raw_html) {
  raw_html %>% 
    read_html() %>% 
    xml_find_first('//*[@id="ysf-preauctioncosts-dt"]') %>% 
    html_table %>% 
    tibble() %>% 
    janitor::clean_names() %>% 
    select(player, league_value:avg_salary) %>% 
    mutate(player = player %>% v_cln_plyr(),
           position = player %>% v_ext_pos(),
           player = player %>% v_trim_pos(),
           team = player %>% v_ext_team(),
           player = player %>% v_trim_team(),
           across(league_value:avg_salary, ~gsub("\\$", "", .) %>% as.numeric)) %>% 
    filter(!is.na(player))
}
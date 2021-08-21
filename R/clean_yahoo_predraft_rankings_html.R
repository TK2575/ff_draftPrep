library("here")
library("dplyr")
library("xml2")
library("rvest")
library("janitor")
library("stringr")

html_page_to_table <- function(raw_html) {
  extract_pos <- function(char) {
    j <- str_locate_all(pattern = '-', char) %>%
      unlist()
    
    char %>%
      substr(max(j)+1,nchar(char)) %>%
      str_trim()
  }
  
  v_ext_pos <- Vectorize(extract_pos)
  
  trim_pos <- function(char) {
    k <- str_locate_all(pattern = '-', char) %>%
      unlist()
    
    char %>%
      substr(0,max(k)-1) %>%
      str_trim()
  }
  
  v_trim_pos <- Vectorize(trim_pos)
  
  ext_team <- function(char) {
    l <- str_locate_all(pattern = " ", char) %>% 
      unlist()
    
    char %>% 
      substr(max(l)+1,nchar(char)) %>%
      str_trim() %>% 
      toupper()
      
  }
  
  v_ext_team <- Vectorize(ext_team)
  
  trim_team <- function(char) {
    m <- str_locate_all(pattern = " ", char) %>% 
      unlist()
    
    char %>% 
      substr(0, max(m)) %>% 
      str_trim()
  }
  
  v_trim_team <- Vectorize(trim_team)
  
  clean_plyr <- function(char) {
    i <- str_locate_all(pattern = '\\n', char) %>%
      unlist()
    
    char %>% 
      substr(i[1]+1,i[2]-1) %>%
      str_trim()
  }
  
  v_cln_plyr <- Vectorize(clean_plyr)
  
  
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
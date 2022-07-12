scrape_single_table <- function(url) {
  url %>% 
    read_html() %>% 
    html_nodes("table") %>% 
    html_table() %>% 
    .[[1]] 
}

team_defense <- function(year) {
  if (!is.numeric(year) || nchar(year) != 4) {
    stop("year must be a four digit number")
  }
  
  url <- paste0("https://widgets.sports-reference.com/wg.fcgi?css=1&site=pfr&url=%2Fyears%2F",
  year,
  "%2Fopp.htm&div=div_team_stats")
  
  url %>% 
    scrape_single_table() %>% 
    row_to_names(1) %>% 
    clean_names() %>% 
    as_tibble() 
  # TODO disambiguate column categories: total, passing, rushing, penalty
}
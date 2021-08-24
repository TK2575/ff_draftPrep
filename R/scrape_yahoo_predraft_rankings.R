library("here")
library("config")
library("xml2")
library("rvest")
library("purrr")
library("RSelenium")
library("dplyr")
source(here::here("R", "clean_yahoo_predraft_rankings_html.R"))

scrape_predraft_ranking_pages <- function() {
  url_base <- "https://football.fantasysports.yahoo.com/f1/41236/"
  
  open_session <- function() {
    ipaddress <- config::get(file = here::here("conf", "conf.yml"))$network$ipaddress
    rd <- remoteDriver(
      remoteServerAddr = ipaddress,
      port = 4445L,
      browserName = "firefox"
    )
    
    rd$open()
    rd
  }
  
  login <- function(rd) {
    credentials <- config::get(file = here::here("conf", "conf.yml"))$yahoo
    rd$navigate(url_base)
    webElem <- rd$findElement('xpath', '//*[@id="login-username"]')
    webElem$sendKeysToElement(list(credentials$username, key = "enter"))
    Sys.sleep(10)
    webElem2 <- rd$findElement('xpath', '//*[@id="login-passwd"]')
    webElem2$sendKeysToElement(list(credentials$password, key = 'enter'))
    Sys.sleep(10)
  }
  
  close_session <- function(rd) {
    rd$close()
  }
  
  scrape_predraft_ranking_page <- function(rd, page_number) {
    url <- paste0(url_base,
                  "3/prerank_auction_costs?filter=ALL&sort=TAC&count=", 
                  page_number*50)
    
    rd$navigate(url)
    rd$getPageSource()[[1]][1] %>% 
      predraft_rankings_html_to_table()
  }
  
  rd <- open_session()
  login(rd)
  
  df <- 
    map(as.list(0:23), .f = scrape_predraft_ranking_page, rd = rd)
  
  close_session(rd)
  
  df %>% 
    bind_rows()
}



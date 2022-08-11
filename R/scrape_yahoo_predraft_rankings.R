scrape_predraft_ranking_pages <- function() {
  conf <- config::get(file = here::here("conf", "conf.yaml"))
  
  url_base <- paste0("https://football.fantasysports.yahoo.com/f1/", conf$yahoo$league_id, "/")
  
  open_session <- function() {
    ipaddress <- conf$network$ipaddress
    rd <- remoteDriver(
      remoteServerAddr = ipaddress,
      port = 4445L,
      browserName = "firefox"
    )
    
    rd$open()
    rd
  }
  
  login <- function(rd) {
    rd$navigate(url_base)
    webElem <- rd$findElement('xpath', '//*[@id="login-username"]')
    webElem$sendKeysToElement(list(conf$yahoo$username, key = "enter"))
    Sys.sleep(10)
    webElem2 <- rd$findElement('xpath', '//*[@id="login-passwd"]')
    webElem2$sendKeysToElement(list(conf$yahoo$password, key = 'enter'))
    Sys.sleep(10)
  }
  
  close_session <- function(rd) {
    rd$close()
  }
  
  scrape_predraft_ranking_page <- function(rd, page_number) {
    url <- paste0(url_base, 
                  conf$yahoo$team_id,
                  "/prerank_auction_costs?filter=ALL&sort=TAC&count=", 
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



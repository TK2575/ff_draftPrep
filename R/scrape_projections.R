scrape_projections <- function(season, out_directory, latest_directory, force=FALSE) {
  scrape_file <- file.path(out, paste0("scrape-", Sys.Date(), ".Rds"))
  
  scrape <- NULL
  
  if (file.exists(scrape_file) & !force) {
    scrape <- readRDS(scrape_file)
  } else {
    scrape <- 
      scrape_data(src = c(
        "CBS", "ESPN", "Yahoo", "NFL", "FantasyPros", "NumberFire", 
        "FFToday", "FantasySharks", "FantasyFootballNerd", 
        "Walterfootball", "RTSports", "FantasyData", "Fleaflicker"), 
        pos = c("QB", "RB", "WR", "TE", "DST", "K"),
        season = season, 
        week = 0)
    
    write_rds(scrape, 
              scrape_file)
    
    write_rds(scrape, 
              file.path(latest_directory,"scrape-latest.Rds"))
  }
  
  scrape
}
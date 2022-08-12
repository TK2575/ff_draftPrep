scrape_yahoo_auction_values <- function(out_directory, latest_directory, force=FALSE) {
  yahoo_av_file <-
    file.path(out_directory, paste0("yahoo-av-", Sys.Date(), ".csv"))
  
  yahoo_av <- NULL
  
  if (file.exists(yahoo_av_file) & !force) {
    yahoo_av <- 
      read_csv(yahoo_av_file, show_col_types = FALSE)
  } else {
    yahoo_av <- 
      scrape_predraft_ranking_pages() %>% 
      mutate(position = if_else(position == "DEF","DST",position),
             team = case_when(
               team == "NO" ~ "NOS",
               team == "GB" ~ "GBP",
               team == "KC" ~ "KCC",
               team == "LV" ~ "LVR",
               team == "TB" ~ "TBB",
               team == "SF" ~ "SFO",
               team == "NE" ~ "NEP",
               team == "JAX" ~ "JAC",
               TRUE ~ team),
             player = gsub(" Jr.", "", player),
             # player = gsub(" III", "", player),
             # player = gsub(" II", "", player),
             player = gsub("Eli Mitchell", "Elijah Mitchell", player),
             player = gsub("DJ Moore", "D.J. Moore", player))
    
    write_csv(yahoo_av, yahoo_av_file)
    write_csv(yahoo_av, 
              file.path(latest_directory, "yahoo-av-latest.csv"))
  }
  yahoo_av
}
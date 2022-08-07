calculate_projections_table <- function(out_directory, latest_directory, force=FALSE) {
  projections_table_file <- 
    file.path(out_directory, 
              paste0("projections_table-", Sys.Date(), ".Rds"))
  
  projections_table <- NULL
  
  if (file.exists(projections_table_file) & !force) {
    projections_table <- readRDS(projections_table_file)
  } else {
    projections_table <- 
      projections_table(scrape,
                        scoring_rules = scoring_rules()) %>% 
      add_ecr() %>% 
      add_risk() %>%
      add_adp() %>% 
      add_aav_custom() %>% 
      add_player_info()
    
    write_rds(projections_table, 
              projections_table_file)
    
    write_rds(projections_table, 
              file.path(latest_directory, 
                        "projections_table-latest.Rds"))
  }
  projections_table
}
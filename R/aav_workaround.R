add_aav_custom <- function(projection_table) {
  
  season <- attr(projection_table, "season")
  week <- attr(projection_table, "week")
  lg_type <- attr(projection_table, "lg_type")
  
  if (week != 0) {
    warning("AAV data is not available for weekly data", 
            call. = FALSE)
    return(projection_table)
  }
  
  adp_tbl <- 
    get_adp(type="AAV") %>% 
    select(1, length(.)) %>% 
    rename_at(length(.), function(x) {
      return("aav")
    })
  
  projection_table %>% 
    left_join(adp_tbl, by="id") %>% 
    `attr<-`(which = "season", season) %>% 
    `attr<-`(which = "week", week) %>% 
    `attr<-`(which = "lg_type", lg_type)
}




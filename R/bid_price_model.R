bid_pricing <- function(projections, 
                        auction_values, 
                        latest_directory = NULL, 
                        dollar_vorp_target_override = NULL) {
  df <-
    projections %>% 
    select(id:sd_pts, 
           points_per_game, 
           risk:rank, 
           flex_rank:dollar_vorf) %>% 
    mutate(full_name = if_else(position == 'DST',
                               first_name,
                               paste0(first_name, " ", last_name))) %>% 
    left_join(yahoo_av, 
              by = c("full_name" = "player", "position" = "position", "team" = "team")) %>% 
    select(-full_name) %>% 
    mutate(max_yahoo_value = pmax(league_value, proj_salary, avg_salary, na.rm = TRUE),
           bid_diff = if_else(is.na(max_yahoo_value),0,max_bid - max_yahoo_value))
  
  dst_k <-
    df %>% 
    filter(position %in% c("DST","K")) %>% 
    mutate(max_bid = if_else(max_bid<5,1,if_else(max_bid<10,2,3)),
           max_yahoo_value = max_bid,
           bid_diff = 0)
  
  if (is.null(dollar_vorp_target_override)) {
    bid_adjustments <-
      df %>% 
      filter(!position %in% c("DST","K") & vorp >= 0) %>% 
      mutate(is_qb = position == 'QB') %>% 
      group_by(is_qb) %>% 
      summarize(bid_adjustment = -median(bid_diff, na.rm = TRUE)) %>% 
      ungroup()
    
    results <- 
      df %>% 
      filter(!position %in% c("DST","K")) %>% 
      mutate(is_qb = position == 'QB') %>% 
      left_join(bid_adjustments, by = "is_qb") %>% 
      mutate(max_bid = pmax(max_bid + bid_adjustment,1),
             dollar_vorp = if_else(vorp == 0, 0, (pmax(max_bid,max_yahoo_value, na.rm=TRUE) / vorp)) %>% round(2),
             bid_diff = max_bid - max_yahoo_value)
    
    if (!is.null(latest_directory)) {
      results %>% 
        filter(vorp >= 0) %>% 
        group_by(is_qb) %>% 
        summarize(dollar_vorp_median = median(dollar_vorp, na.rm = TRUE),
                  dollar_vorp_sd = sd(dollar_vorp, na.rm = TRUE)) %>% 
        left_join(bid_adjustments, by = "is_qb") %>% 
        relocate(is_qb, bid_adjustment) %>% 
        write_csv(file.path(latest, "bid_adjustments.csv"))
    }
    
    results %>% 
      bind_rows(dst_k) %>% 
      select(-(league_value:avg_salary), -bid_adjustment, -is_qb)
  } else {
    df %>% 
      filter(!position %in% c("DST","K")) %>% 
      bind_rows(dst_k) %>%
      select(-(league_value:avg_salary))
  }
  
}

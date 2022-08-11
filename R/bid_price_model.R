bid_pricing <- function(projections, auction_values, latest_directory = NULL) {
  df <-
    projections %>% 
    select(id:sd_pts, 
           points_per_game, 
           tier, 
           pos_tier, 
           pos_ecr:rank, 
           flex_rank:max_bid) %>% 
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
  
  bid_adjustments <-
    df %>% 
    filter(!position %in% c("DST","K") & vorp >= 0) %>% 
    group_by(position) %>% 
    summarize(bid_adjustment = -median(bid_diff, na.rm = TRUE)) %>% 
    ungroup()
  
  results <- 
    df %>% 
    filter(!position %in% c("DST","K")) %>% 
    left_join(bid_adjustments, by = "position") %>% 
    mutate(max_bid = pmax(max_bid + bid_adjustment,1),
           dollar_vorp = if_else(vorp == 0, 0, (max_bid / vorp)) %>% round(2),
           bid_diff = max_bid - max_yahoo_value)
  
  if (!is.null(latest_directory)) {
    results %>% 
      filter(vorp >= 0) %>% 
      group_by(position) %>% 
      summarize(dollar_vorp_mean = mean(dollar_vorp, na.rm = TRUE),
                dollar_vorp_median = median(dollar_vorp, na.rm = TRUE),
                dollar_vorp_sd = sd(dollar_vorp, na.rm = TRUE)) %>% 
      left_join(bid_adjustments, by = "position") %>% 
      relocate(position, bid_adjustment) %>% 
      write_csv(file.path(latest, "bid_adjustments.csv"))
  }
  
  results %>% 
    bind_rows(dst_k) %>% 
    select(-(league_value:avg_salary), -bid_adjustment)
}

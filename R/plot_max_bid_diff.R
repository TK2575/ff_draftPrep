plot_max_bid_diff_by_position <- function(projections, vorp_details) {
  vorp_target_display <- vorp_details$dollar_vorp_target_override
  if (is.null(vorp_target_display)) {
    vorp_target_display <- vorp_details$dollar_per_vorp_target
  }
  title <- 
    paste0("$",vorp_target_display, "/VORP")
  
  projections %>%
    filter(vorp >= 0) %>% 
    filter(!position %in% c("K","DST")) %>% 
    mutate(max_yahoo_value = pmax(league_value,proj_salary,avg_salary)) %>% 
    mutate(max_yahoo_value = if_else(is.na(max_yahoo_value),1,max_yahoo_value)) %>% 
    mutate(bid_diff = max_bid - max_yahoo_value) %>% 
    select(id, position, vorp, max_bid, bid_diff) %>%
    ggplot2::ggplot(aes(x=bid_diff, color=position)) +
    geom_freqpoly(binwidth = 10) +
    ggtitle("Max Bid Versus Projected Auction Price",
            subtitle = title)
}
library(here)
library(dplyr)
library(tibble)
source(here::here("R", "tiers.R"))

vorp_auction <- function(projections, 
                         target_points_week = 120,
                         budget = 200) {
  qb <-
    projections %>% 
    filter(position == 'QB', !is.na(points)) %>% 
    arrange(points_per_game %>% desc)
  
  rb <- 
    projections %>% 
    filter(position == 'RB', !is.na(points)) %>% 
    arrange(points_per_game %>% desc)
  
  wr <-
    projections %>% 
    filter(position == 'WR', !is.na(points)) %>% 
    arrange(points_per_game %>% desc)
  
  te <- 
    projections %>% 
    filter(position == 'TE', !is.na(points)) %>% 
    arrange(points_per_game %>% desc)
  
  flex <-
    projections %>% 
    filter(flex, !is.na(points)) %>% 
    arrange(points_per_game %>% desc)
  
  dst <-
    projections %>% 
    filter(position == 'DST', !is.na(points)) %>% 
    arrange(points_per_game %>% desc)
  
  k <-
    projections %>% 
    filter(position == 'K', !is.na(points)) %>% 
    arrange(points_per_game %>% desc)
  
  positions <- c('QB','RB1', 'RB2', 'WR1', 'WR2', 'TE', 'FLEX', 'DST', 'K')
  
  max_values <- c(
    qb$points_per_game %>% max(),
    rb$points_per_game %>% max(),
    rb$points_per_game %>% nth(2),
    wr$points_per_game %>% max(),
    wr$points_per_game %>% nth(2),
    te$points_per_game %>% max(),
    max(
      rb$points_per_game %>% nth(3),
      wr$points_per_game %>% nth(3),
      te$points_per_game %>% nth(2)
    ),
    dst$points_per_game %>% max(),
    k$points_per_game %>% max()
  )
  
  flex_replacement <-
    flex$points_per_game %>% nth(71)
  
  replacement_values <- c(
    qb$points_per_game %>% nth(15),
    flex_replacement,
    flex_replacement,
    flex_replacement,
    flex_replacement,
    te$points_per_game %>% nth(15),
    flex_replacement,
    dst$points_per_game %>% nth(15),
    k$points_per_game %>% nth(15)
  )
  
  roster_values <-
    tibble(
      pos = positions,
      best = max_values,
      replacement = replacement_values)
  
  replacement_team_points_weekly <-
    roster_values$replacement %>% sum()
  
  best_team_points_weekly <-
    roster_values$best %>% sum()
  
  target_team_vorp <- 
    target_points_week - replacement_team_points_weekly
  
  dollar_per_vorp_target <- 
    (budget / target_team_vorp) %>% 
    round()
  
  replacements <-
    roster_values %>% 
    select(-best) %>% 
    deframe() 
  
  qb <-
    qb %>% 
    mutate(points_per_dollar = (points_per_game / aav) %>% round(1),
           vorp = points_per_game - replacements['QB'],
           dollar_vorp = if_else(vorp == 0, 0, (aav / vorp) %>% round(1)),
           max_bid = ceiling(vorp * dollar_per_vorp_target),
           max_bid = if_else(max_bid<=1,1,max_bid)) %>% 
    add_column(pos_tier = tiers(qb$points,9)) 
  
  rb <-
    rb %>%
    mutate(points_per_dollar = (points_per_game / aav) %>% round(1),
           vorp = points_per_game - replacements['RB1'],
           dollar_vorp = if_else(vorp == 0, 0, (aav / vorp) %>% round(1)),
           max_bid = ceiling(vorp * dollar_per_vorp_target),
           max_bid = if_else(max_bid<=1,1,max_bid)) %>% 
    add_column(pos_tier = tiers(rb$points,20))
  
  wr <-
    wr %>% 
    mutate(points_per_dollar = (points_per_game / aav) %>% round(1),
           vorp = points_per_game - replacements['WR1'],
           dollar_vorp = if_else(vorp == 0, 0, (aav / vorp) %>% round(1)),
           max_bid = ceiling(vorp * dollar_per_vorp_target),
           max_bid = if_else(max_bid<=1,1,max_bid)) %>% 
    add_column(pos_tier = tiers(wr$points,20))
  
  te <-
    te %>% 
    mutate(points_per_dollar = (points_per_game / aav) %>% round(1),
           vorp = points_per_game - replacements['TE'],
           dollar_vorp = if_else(vorp == 0, 0, (aav / vorp) %>% round(1)),
           max_bid = ceiling(vorp * dollar_per_vorp_target),
           max_bid = if_else(max_bid<=1,1,max_bid)) %>% 
    add_column(pos_tier = tiers(te$points,8))
  
  dst <-
    dst %>% 
    mutate(points_per_dollar = (points_per_game / aav) %>% round(1),
           vorp = points_per_game - replacements['DST'],
           dollar_vorp = if_else(vorp == 0, 0, (aav / vorp) %>% round(1)),
           max_bid = ceiling(vorp * dollar_per_vorp_target),
           max_bid = if_else(max_bid<=1,1,max_bid)) %>% 
    add_column(pos_tier = tiers(dst$points,3))
  
  k <-
    k %>% 
    mutate(points_per_dollar = (points_per_game / aav) %>% round(1),
           vorp = (points_per_game - replacements['K']) %>% round(1),
           dollar_vorp = if_else(vorp == 0, 0, (aav / vorp) %>% round(1)),
           max_bid = ceiling(vorp * dollar_per_vorp_target),
           max_bid = if_else(max_bid<=1,1,max_bid)) %>% 
    add_column(pos_tier = tiers(k$points,6))
  
  bind_rows(qb, rb, wr, te, dst, k)
}
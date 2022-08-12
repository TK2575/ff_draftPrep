vorp_auction <- function(projections, 
                         target_points_week = 120,
                         budget = 200,
                         dollar_vorp_target_override = NULL,
                         latest_directory = NULL) {
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
    max(
      rb$points_per_game %>% nth(31),
      wr$points_per_game %>% nth(31),  
      te$points_per_game %>% nth(16)
    )
  
  replacement_values <- c(
    qb$points_per_game %>% nth(15),
    rb$points_per_game %>% nth(29),
    rb$points_per_game %>% nth(30),
    wr$points_per_game %>% nth(29),
    wr$points_per_game %>% nth(30),
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
    round(2)
  
  if (!is.null(latest_directory)) {
    vorp_details <- 
      list(roster_values,
           replacement_team_points_weekly,
           best_team_points_weekly,
           target_team_vorp,
           dollar_per_vorp_target,
           dollar_vorp_target_override)
    
    names(vorp_details) <- c("roster_values",
                             "replacement_team_points_weekly",
                             "best_team_points_weekly",
                             "target_team_vorp",
                             "dollar_per_vorp_target",
                             "dollar_vorp_target_override")
    
    write_rds(vorp_details,
              file.path(latest, "vorp_details.Rds"))
  }
  
  if (!is.null(dollar_vorp_target_override) & is.numeric(dollar_vorp_target_override)) {
    dollar_per_vorp_target <- dollar_vorp_target_override
  }
  
  replacements <-
    roster_values %>% 
    select(-best) %>% 
    deframe() 
  
  qb <-
    qb %>% 
    mutate(vorp = points_per_game - replacements['QB'],
           vorf = vorp)
  
  rb <-
    rb %>%
    mutate(vorp = points_per_game - replacements['RB1'],
           vorf = points_per_game - replacements['FLEX'])
  
  wr <-
    wr %>% 
    mutate(vorp = points_per_game - replacements['WR1'],
           vorf = points_per_game - replacements['FLEX'])
  
  te <-
    te %>% 
    mutate(vorp = points_per_game - replacements['TE'],
           vorf = points_per_game - replacements['FLEX'])
  
  dst <-
    dst %>% 
    mutate(vorp = points_per_game - replacements['DST'],
           vorf = vorp)
  
  k <-
    k %>% 
    mutate(vorp = (points_per_game - replacements['K']) %>% round(1),
           vorf = vorp)
  
  bind_rows(qb, rb, wr, te, dst, k) %>% 
    mutate(max_bid = pmax(1, ceiling(vorf * dollar_per_vorp_target)),
           dollar_vorp = if_else(vorp == 0, 0, (max_bid / vorp) %>% round(1)),
           dollar_vorf = if_else(vorf == 0, 0, (max_bid / vorf) %>% round(1)))
}
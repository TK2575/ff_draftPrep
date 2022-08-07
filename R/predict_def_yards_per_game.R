predict_def_yards_per_game <- function(season, out_directory, latest_directory, force=FALSE) {
  def_perf_file <-
    file.path(out_directory, paste0("def_perf_file-", season, ".csv"))
  
  def_perf <- NULL
  
  if (file.exists(def_perf_file) & !force) {
    def_perf <- 
      read_csv(def_perf_file, show_col_types = FALSE)
  } else {
    pbp <- nflfastR::load_pbp(c(season-6):(season-1))
    
    def_perf <-
      pbp %>% 
      filter(!is.na(yards_gained)) %>% 
      filter(!is.na(defteam)) %>% 
      group_by(defteam, game_id, week, game_date) %>% 
      summarize(yards_allowed = sum(yards_gained)) %>% 
      arrange(game_date, game_id)
    
    rm(pbp)
    nflreadr::.clear_cache()
    
    write_csv(def_perf, def_perf_file)
    write_csv(def_perf, 
              file.path(latest_directory, "def-perf-latest.csv"))
  }
  
  dst_yardage_points <-
    def_perf %>%
    rename(yards = yards_allowed,
           team = defteam) %>% 
    filter(!is.na(yards)) %>% 
    mutate(year = substr(game_date, 0, 4)) %>% 
    mutate(points = case_when(
      yards < 100 ~ 11.5,
      yards < 200 ~ 9.5,
      yards < 300 ~ 8.5,
      yards < 400 ~ 6,
      yards < 500 ~ 2.5,
      TRUE ~ 0)) %>% 
    group_by(year, team) %>% 
    summarize(weeks = n(),
              yards = sum(yards),
              points = sum(points),
              points_per_week = points / weeks,
              yards_per_week = yards / weeks)
  
  model <- lm(points_per_week ~ yards_per_week,
              data=dst_yardage_points)
  
  predicted_yards_per_game <- 
    scrape$DST %>% 
    mutate(id = if_else(
      is.na(id) & startsWith(player, "Las Vegas"),
      "0513", id)) %>% 
    filter(!is.na(dst_yds_allowed)) %>% 
    group_by(id) %>% 
    summarize(yards_per_week = mean(dst_yds_allowed) / regular_season_games)
  
  predicted_yards_per_game$points_per_week <- 
    predict(model, predicted_yards_per_game)
  
  predicted_yards_per_game %>% 
    mutate(additional_points = (points_per_week * regular_season_games) %>% round()) %>% 
    select(id, additional_points)
}
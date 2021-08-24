library("here")
library("janitor")
library("dplyr")
library("stringr")

identify_starters_by_ppg <- function(rosters) {
  single_slots <-
    rosters %>% 
    filter(team_pos_rank == 1 & position %in% c("QB","K","DST","TE")) %>% 
    mutate(slot = position)
  
  dual_slots <-
    rosters %>% 
    filter(team_pos_rank <= 2 & position %in% c("RB","WR")) %>% 
    mutate(slot = paste0(position, team_pos_rank))
  
  flex <-
    rosters %>% 
    filter(
      (team_pos_rank == 3 & position %in% c("RB","WR")) |
        (team_pos_rank == 2 & position == "TE")
    ) %>% 
    group_by(fantasy_team) %>% 
    top_n(1, points_per_game) %>% 
    mutate(slot = 'FLEX')
  
  bind_rows(single_slots, dual_slots, flex)
}




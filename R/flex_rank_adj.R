add_flex_rank_adj <- function(projections) {
  flex_rank_adj <- 
    projections %>% 
    filter((position %in% c('RB','WR') & pos_rank > 28) | (position == 'TE' & pos_rank > 14)) %>% 
    mutate(flex_rank_adj = rank(flex_rank, ties.method = "first")) %>% 
    select(id, flex_rank_adj)
  
  projections %>% 
    left_join(flex_rank_adj, by = "id")
}
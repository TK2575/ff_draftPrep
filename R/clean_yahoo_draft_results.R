draft_results_html_to_table <- function(raw_html) {
  raw_html %>% 
    read_html() %>% 
    html_table() %>% 
    .[[1]] %>% 
    janitor::clean_names() %>% 
    rename(fantasy_team = team) %>% 
    mutate(position = player %>% v_ext_pos() %>% gsub("\\)","",.),
           player = player %>% v_trim_pos(),
           team = player %>% v_ext_team %>% gsub("\\(","",.),
           player = player %>% v_trim_team(),
           salary = gsub("\\$","", salary) %>% as.numeric)
}

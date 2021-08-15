reorder_tiers <- function(unordered_tiers) {
  prior <- 0
  value <- 0
  tier <- 1
  len <- length(unordered_tiers)
  results <- vector(length=len)
  
  for (i in 1:len) {
    value <- unordered_tiers[i]
    
    if (prior == 0) {
      prior <- value
    }
    
    if (prior != value) {
      tier <- tier + 1
      prior <- value
    } 
    
    results[i] <- tier
  }
  results
}

tiers <- function(yearly_points_vector, tier_count) {
  yearly_points_vector %>% 
    kmeans(tier_count) %>% 
    .$cluster %>% 
    reorder_tiers()
}
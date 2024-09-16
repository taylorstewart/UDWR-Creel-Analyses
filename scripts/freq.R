freq <- function(data = dat_contact, grp_var = "gear") {
  # Create frequency distribution
  freq_dist <- data %>%
    group_by(across(grp_var)) %>% 
    summarize(count = n()) %>% 
    ungroup() %>% 
    mutate(percent = (count / sum(count)) * 100)
  freq_dist
}

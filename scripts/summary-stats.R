creel_summary_stats <- function(data = dat_contact, grp_var = "month", var = "age") {
  sum_stats <- data %>%
    group_by(across(grp_var)) %>% 
    summarize(n = n(),
              min = min(!! sym(var), na.rm = TRUE),
              q1 = quantile(!! sym(var), prob = 0.25, na.rm = TRUE),
              mean = mean(!! sym(var), na.rm = TRUE),
              median = median(!! sym(var), na.rm = TRUE),
              q3 = quantile(!! sym(var), prob = 0.75, na.rm = TRUE),
              max = max(!! sym(var), na.rm = TRUE),
              sd = sd(!! sym(var), na.rm = TRUE),
              se = sd/sqrt(n)) %>% 
    ungroup()
  sum_stats
}

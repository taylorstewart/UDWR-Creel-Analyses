check <- function(data1, data2) {
  calendar <- create_calendar(start_date, end_date) %>% 
    select(year, month, day, date_cal = date, wday_cal = wday, dow_cal = dow)
  
  d1 <- data1 %>%
    left_join(calendar) %>% 
    distinct(year, month, day, date_1 = date, date_cal, wday_cal, dow_1 = dow, dow_cal) %>% 
    mutate(dow_1_flag = ifelse(dow_cal != dow_1, 'Incorrect DOW in data 1', NA),
           date_1_flag = ifelse(date_cal != date_1, 'Incorrect date in data 1', NA)) %>% 
    arrange(year, month, day)
  
  d2 <- data2 %>%
    left_join(calendar) %>% 
    distinct(year, month, day, date_2 = date, date_cal, wday_cal, dow_2 = dow, dow_cal) %>% 
    mutate(dow_2_flag = ifelse(dow_cal != dow_2, 'Incorrect DOW in data 2', NA),
           date_2_flag = ifelse(date_cal != date_2, 'Incorrect date in data 2', NA)) %>% 
    arrange(year, month, day)
  
  d3 <- left_join(d1, d2) %>%
    mutate(date_3_flag = ifelse(date_1 != date_2, 'Inconsistency with dates between files', NA),
           dow_3_flag = ifelse(dow_1 != dow_2, 'Inconsistency in DOW between files', NA)) %>% 
    select(year, month, day, date_cal, wday_cal, dow_cal, date_1_flag, date_2_flag, dow_1_flag, dow_2_flag, date_3_flag, dow_3_flag)
  return(d3)
}

# Function to create a calendar data frame that can be merged with creel data files
create_calendar <- function(sdate = start_date, fdate = end_date) {
  # Convert start and end dates from character to Date type
  start_date <- ymd(sdate)
  end_date <- ymd(fdate)
  
  # Create a sequence of dates from start to end
  dates <- seq.Date(from = start_date, to = end_date, by = "day")
  
  # Create a data frame with the sequence of dates
  calendar_df <- data.frame(date = dates)
  
  # Extract year, month, and day from the dates
  calendar_df %<>% 
    mutate(year = year(date),
           month = month(date),
           day = day(date),
           wday = lubridate::wday(date, label = TRUE, abbr = FALSE),
           dow = ifelse(
             wday %in% c("Sunday", "Saturday") |
               (month == 1 & day == 1) |
               (month == 1 & day >= 15 & day <= 21 & wday == "Monday") |
               (month == 2 & day >= 15 & day <= 21 & wday == "Monday") |
               (month == 5 & day >= 25 & day <= 31 & wday == "Monday") |
               (month == 6 & day == 19) |
               (month == 7 & day == 4) |
               (month == 7 & day == 24) |
               (month == 9 & day >= 1 & day <= 7 & wday == "Monday") |
               (month == 10 & day >= 8 & day <= 14 & wday == "Monday") |
               (month == 11 & day == 11) |
               (month == 11 & day >= 22 & day <= 28 & wday == "Thursday") |
               (month == 11 & day >= 22 & day <= 28 & wday == "Friday") |
               (month == 12 & day == 24) |
               (month == 12 & day == 25) |
               (month == 12 & day == 31),
             'we', 'wd'
           ))
  
  return(calendar_df)
}

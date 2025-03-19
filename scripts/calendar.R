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

  # Assign season, but need to account for leap years
  ## Spring - March 20
  ## Summer - June 21
  ## Fall - September 22
  ## Winter - December 21
  if(unique(calendar_df$year) %in% seq(1992, 2092, 4)) {
    # leap year
    calendar_df %<>%
      mutate(yday = yday(date),
             season = case_when(
               yday >= 80 & yday < 173 ~ "Spring",
               yday >= 173 & yday < 266 ~ "Summer",
               yday >= 266 & yday < 356 ~ "Autumn",
               yday >= 365 & yday < 80 ~ "Winter"
      ))
  } else {
    # non leap year
    calendar_df %<>%
      mutate(yday = yday(date),
             season = case_when(
               yday >= 79 & yday < 172 ~ "Spring",
               yday >= 172 & yday < 265 ~ "Summer",
               yday >= 265 & yday < 355 ~ "Autumn",
               yday >= 365 & yday < 79 ~ "Winter"
             ))
  }

  return(calendar_df)
}

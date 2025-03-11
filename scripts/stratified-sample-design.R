library(suncalc)
library(dplyr)
library(tidyr)
library(lubridate)
library(magrittr)
library(ggplot2)

start_date <- "2026/03/01"
end_date <- "2026/10/31"
date_vec <- seq.Date(as.Date(start_date), as.Date(end_date), "days")

dat <- getSunlightTimes(date_vec, lat = 41.186126, lon = -111.381330, keep = c("sunrise", "sunset"), tz = "America/Denver") %>% 
  mutate(month = month(date))

dat_sum <- dat %>% group_by(month) %>% 
  summarize(max_sunrise = max(sunrise),
            min_sunset = min(sunset)) %>% 
  mutate(sunrise_hour = hour(max_sunrise),
         sunrise_minute = minute(max_sunrise),
         sunrise_time = as.POSIXct(paste0(Sys.Date(), " ", sunrise_hour, ":", sunrise_minute), format = "%Y-%m-%d %H:%M"),
         sunset_hour = hour(min_sunset),
         sunset_minute = minute(min_sunset),
         sunset_time = as.POSIXct(paste0(Sys.Date(), " ", sunset_hour, ":", sunset_minute), format = "%Y-%m-%d %H:%M")) %>% 
  mutate(lod = round(sunset_time-sunrise_time, 1))

ggplot(dat_sum, aes(x = month, y = lod)) +
  geom_bar(stat = "identity", fill = "gray70", color = "black") +
  scale_x_continuous(limits = c(2.5, 10.5), breaks = seq(1, 12, 1), expand = c(0, 0.1)) +
  scale_y_continuous(limits = c(0, 16), breaks = seq(0, 16, 2), expand = c(0, 0.02)) +
  labs(y = "Daylength (hours)", x = "Month") + 
  theme_bw() + 
  theme(axis.title.x = element_text(size = 18, margin = margin(10, 0, 0, 0)),
        axis.title.y = element_text(size = 18, margin = margin(0, 10, 0, 0)),
        axis.text.x = element_text(size = 15),
        axis.text.y = element_text(size = 15),
        panel.grid = element_blank(),
        plot.margin = margin(10, 10, 5, 5))

ggsave("figures/sample-design-tod-unstratified.png", width = 7, height = 7, dpi = 300)



dat_sum_strat <- dat_sum %>% 
  mutate(losp = lod/2) %>% 
  group_by_all() %>% 
  expand(shift = c("AM", "PM")) %>% 
  ungroup()



ggplot(dat_sum_strat, aes(x = month, y = losp, group = shift, fill = shift)) +
  geom_bar(stat = "identity", color = "black", alpha = 0.8) +
  scale_x_continuous(limits = c(2.5, 10.5), breaks = seq(1, 12, 1), expand = c(0, 0.1)) +
  scale_y_continuous(limits = c(0, 16), breaks = seq(0, 16, 2), expand = c(0, 0.02)) +
  scale_fill_manual(values = c("#1f78b4", "#33a02c")) +
  labs(y = "Length of Sampling Period (hours)", x = "Month") + 
  theme_bw() + 
  theme(axis.title.x = element_text(size = 18, margin = margin(10, 0, 0, 0)),
        axis.title.y = element_text(size = 18, margin = margin(0, 10, 0, 0)),
        axis.text.x = element_text(size = 15),
        axis.text.y = element_text(size = 15),
        legend.position = "inside",
        legend.position.inside = c(0.905, 0.92),
        legend.background = element_rect(fill = "transparent"),
        legend.title = element_blank(),
        legend.text = element_text(size = 15),
        panel.grid = element_blank(),
        plot.margin = margin(10, 10, 5, 5))

ggsave("figures/sample-design-tod-stratified.png", width = 7, height = 7, dpi = 300)

# Function to create a calendar data frame that can be merged with creel data files
## Build calendar from start and end dates to assign DOW and holidays
source("scripts/calendar.R")

cal <- create_calendar(start_date, end_date) %>% 
  mutate(week = isoweek(date)) %>% 
  filter(date >= start_date,
         date <= end_date)


# SURVEY DESIGN EXAMPLES -------------------------------------------------------------------------------------
## SURVEY DESIGN - NO STRATUM --------------------------------------------------------------------------------
## set seed to reproduce randomized results
set.seed(65775744)

## randomly select 16 days
cal_rand <- cal %>% 
  # number of days
  slice_sample(n = 16) %>% 
  ungroup() %>% 
  arrange(month, day)


## SURVEY DESIGN - TWO STRATUM --------------------------------------------------------------------------------
## set seed to reproduce randomized results
set.seed(254929024)

tod_levels <- c("AM", "PM")

## randomly select two weekdays per week and randomly assign a stratification (e.g., AM vs PM) with equal probability
cal_wd_one <- cal %>% filter(dow == "wd") %>% 
  # group by a variable to ensure equal distribution within the levels of selected variable
  group_by(week) %>% 
  # number of days
  slice_sample(n = 2) %>% 
  ungroup() %>% 
  # randomly assign strata
  mutate(shift = sample(rep(tod_levels, each = ceiling(n()/length(tod_levels))), size = n(), replace = FALSE))

## randomly select six weekend days per month and randomly assign a stratification (e.g., AM vs PM) with equal probability
cal_we_one <- cal %>% filter(dow == "we") %>% 
  # number of days
  slice_sample(n = 6) %>% 
  ungroup() %>% 
  # randomly assign strata
  mutate(shift = sample(rep(tod_levels, each = ceiling(n()/length(tod_levels))), size = n(), replace = FALSE))

## Combine randomized calendar
cal_rand_one <- bind_rows(cal_wd_one, cal_we_one) %>% 
  arrange(month, day)


## SURVEY DESIGN - MULTIPLE STRATA ----------------------------------------------------------------------------
## set seed to reproduce randomized results
set.seed(32104934)

## define strata
tod_levels <- c("AM", "PM")
site_levels <- c("A", "B", "C")
# create a dataframe with all possible combinations of strata
all_levels <- expand_grid(tod_levels, site_levels)

## define effort
n_per_week_wd <- 3
n_per_month_we <- 4

## define number of weeks and months in survey period
n_week <- cal %>% filter(dow == "wd") %>% distinct(week) %>% nrow()
n_month <- cal %>% filter(dow == "we") %>% distinct(month) %>% nrow()

## randomly select n weekdays per week and randomly assign both strata with equal probability
cal_wd_mult <- cal %>% filter(dow == "wd") %>% 
  # group by a variable to ensure equal distribution within the levels of selected variable
  group_by(week) %>% 
  # number of days
  slice_sample(n = n_per_week_wd) %>% 
  ungroup()

# randomly assign strata (e.g., time of day and access point)
rand_wd_mult <- bind_rows(replicate(n_week, all_levels %>% sample_n(n_per_week_wd, replace = FALSE), simplify = F)) %>% 
  rename(shift = 1, site = 2)

# combine random dates with randomized strata
cal_wd_mult %<>% bind_cols(rand_wd_mult)

## randomly select n weekend days per month and randomly assign both strata with equal probability
cal_we_mult <- cal %>% filter(dow == "we") %>% 
  # group by a variable to ensure equal distribution within the levels of selected variable
  group_by(month) %>% 
  # number of days
  slice_sample(n = n_per_month_we) %>% 
  ungroup()

# randomly assign strata (e.g., time of day and access point)
rand_we_mult <- bind_rows(replicate(n_month, all_levels %>% sample_n(n_per_month_we), simplify = F)) %>% 
  rename(shift = 1, site = 2)

# combine random dates with randomized strata
cal_we_mult %<>% bind_cols(rand_we_mult)


## Combine randomized calendar
cal_rand_mult <- bind_rows(cal_wd_mult, cal_we_mult) %>% 
  arrange(month, day)


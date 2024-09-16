# CLEAR ENVIRONMENT -----------------------------------------------------------------------------------------------
rm(list = ls(all.names=TRUE))


# LOAD REQUIRED PACKAGES ------------------------------------------------------------------------------------------
if(!require(data.table)) { install.packages("data.table"); library(data.table) }
if(!require(dplyr)) { install.packages("dplyr"); library(dplyr) }
if(!require(magrittr)) { install.packages("magrittr"); library(magrittr) }
if(!require(ggplot2)) { install.packages("ggplot2"); library(ggplot2) }
if(!require(scales)) { install.packages("scales"); library(scales) }
if(!require(ggpmisc)) { install.packages("ggpmisc"); library(ggpmisc) }


# DEFINE WHAT TABLE/DATA TO LOAD ----------------------------------------------------------------------------------
file_name <- "tables/catch_overall_mon.csv"
## load data
dat <- fread(file_name)
## replace spaces with _ in column headers
dat %<>% select_all(~gsub("\\s+|\\.", "_", .))


# DEFINE GROUPING VARIABLE (aka x-axis label for bar plots) -------------------------------------------------------
x_lab <- "Month"


# VISUALIZATIONS --------------------------------------------------------------------------------------------------
## mean effort bar plot
##--- create error bars that are not negative
dat_effort <- dat %>% mutate(Mean_Fishing_Effort_USE = Mean_Fishing_Effort+SE_for_Mean_Fishing_Effort,
                             Mean_Fishing_Effort_LSE = ifelse((Mean_Fishing_Effort-SE_for_Mean_Fishing_Effort) < 0, 0, Mean_Fishing_Effort-SE_for_Mean_Fishing_Effort))

##--- define y-axis limit and interval
y_max <- plyr::round_any(max(dat_effort$Mean_Fishing_Effort_USE, na.rm = TRUE), accuracy = 50, f = ceiling) # round max value by accuracy value
y_breaks <- seq(0, y_max, 50) # min, max, by interval

##--- build plot
ggplot(dat_effort, aes(x = factor(Month), y = Mean_Fishing_Effort)) +
  geom_bar(stat = "identity", color = "#000000", fill = "#BDBDBD") +
  geom_errorbar(aes(ymin = Mean_Fishing_Effort_LSE, 
                    ymax = Mean_Fishing_Effort_USE), 
                position = position_dodge(0.6),
                linewidth = 0.5, width = 0.5, show.legend = FALSE) +
  scale_y_continuous(limits = c(0, y_max), breaks = y_breaks, expand = c(0, 0.3)) +
  labs(x = x_lab, y = "Mean Fishing Effort (Hours ± SE)") +
  theme(axis.title.x = element_text(size = 14, margin = margin(10, 0, 0, 0)),
        axis.title.y = element_text(size = 14, margin = margin(0, 10, 0, 0)),
        axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 10),
        plot.margin = margin(5, 10, 5, 5),
        panel.border = element_rect(color = "black", fill = NA, linewidth = 1),
        panel.background = element_rect(fill = NA),
        panel.grid = element_line(color = "#DFDFDF50"))

##--- save plot (will save last plot called)
ggsave(paste0("figures/mean_effort_", tolower(x_lab), ".png"), dpi = 300, width = 7, height = 5)


## mean catch bar plot
##--- create error bars that are not negative
dat_catch <- dat %>% mutate(Mean_HarvestORCatch_USE = Mean_HarvestORCatch+SE_for_Mean_HarvestORCatch,
                            Mean_HarvestORCatch_LSE = ifelse((Mean_HarvestORCatch-SE_for_Mean_HarvestORCatch) < 0, 0, Mean_HarvestORCatch-SE_for_Mean_HarvestORCatch))

##--- define y-axis limit and interval
y_max <- plyr::round_any(max(dat_catch$Mean_HarvestORCatch_USE, na.rm = TRUE), accuracy = 20, f = ceiling) # round max value by accuracy value
y_breaks <- seq(0, y_max, 20) # min, max, by interval

##--- build plot
ggplot(dat_catch, aes(x = factor(Month), y = Mean_HarvestORCatch)) +
  geom_bar(stat = "identity", color = "#000000", fill = "#BDBDBD") +
  geom_errorbar(aes(ymin = Mean_HarvestORCatch_LSE, 
                    ymax = Mean_HarvestORCatch_USE), 
                position = position_dodge(0.6),
               linewidth = 0.5, width = 0.5, show.legend = FALSE) +
  scale_y_continuous(limits = c(0, y_max), breaks = y_breaks, expand = c(0, 0.1)) +
  labs(x = x_lab, y = "Mean Catch (# of Fish ± SE)") +
  theme(axis.title.x = element_text(size = 14, margin = margin(10, 0, 0, 0)),
        axis.title.y = element_text(size = 14, margin = margin(0, 10, 0, 0)),
        axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 10),
        plot.margin = margin(5, 10, 5, 5),
        panel.border = element_rect(color = "black", fill = NA, linewidth = 1),
        panel.background = element_rect(fill = NA),
        panel.grid = element_line(color = "#DFDFDF50"))

##--- save plot (will save last plot called)
ggsave(paste0("figures/mean_catch_", tolower(x_lab), ".png"), dpi = 300, width = 7, height = 5)


## mean catch per unit effort bar plot
##--- define y-axis limit and interval
y_max <- plyr::round_any(max(dat$Mean_HarvestORCatch_Per_Unit_Effort, na.rm = TRUE), accuracy = 1, f = ceiling) # round max value by accuracy value
y_breaks <- seq(0, y_max, 0.5) # min, max, by interval

##--- build plot
ggplot(dat, aes(x = factor(Month), y = Mean_HarvestORCatch_Per_Unit_Effort)) +
  geom_bar(stat = "identity", color = "#000000", fill = "#BDBDBD") +
  scale_y_continuous(limits = c(0, y_max), breaks = y_breaks, expand = c(0, 0.005)) +
  labs(x = x_lab, y = "Mean CPUE (# of Fish/Hour)") +
  theme(axis.title.x = element_text(size = 14, margin = margin(10, 0, 0, 0)),
        axis.title.y = element_text(size = 14, margin = margin(0, 10, 0, 0)),
        axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 10),
        plot.margin = margin(5, 10, 5, 5),
        panel.border = element_rect(color = "black", fill = NA, linewidth = 1),
        panel.background = element_rect(fill = NA),
        panel.grid = element_line(color = "#DFDFDF50"))
  
##--- save plot (will save last plot called)
ggsave(paste0("figures/mean_cpue_", tolower(x_lab), ".png"), dpi = 300, width = 7, height = 5)


## mean fishing trip bar plot
##--- create error bars that are not negative
dat_trip <- dat %>% mutate(Mean_Number_of_Angler_Trips_USE = Mean_Number_of_Angler_Trips+SE_for_Mean_Number_of_Angler_Trips,
                           Mean_Number_of_Angler_Trips_LSE = ifelse((Mean_Number_of_Angler_Trips-SE_for_Mean_Number_of_Angler_Trips) < 0, 0, Mean_Number_of_Angler_Trips-SE_for_Mean_Number_of_Angler_Trips))

##--- define y-axis limit and interval
y_max <- plyr::round_any(max(dat_trip$Mean_Number_of_Angler_Trips_USE, na.rm = TRUE), accuracy = 10, f = ceiling) # round max value by accuracy value
y_breaks <- seq(0, y_max, 10) # min, max, by interval

##--- build plot
ggplot(dat_trip, aes(x = factor(Month), y = Mean_Number_of_Angler_Trips)) +
  geom_bar(stat = "identity", color = "#000000", fill = "#BDBDBD") +
  geom_errorbar(aes(ymin = Mean_Number_of_Angler_Trips_LSE, 
                    ymax = Mean_Number_of_Angler_Trips_USE), 
                position = position_dodge(0.6),
                linewidth = 0.5, width = 0.5, show.legend = FALSE) +
  scale_y_continuous(limits = c(0, y_max), breaks = y_breaks, expand = c(0, 0.1)) +
  labs(x = x_lab, y = "Mean Number of Fishing Trips ± SE") +
  theme(axis.title.x = element_text(size = 14, margin = margin(10, 0, 0, 0)),
        axis.title.y = element_text(size = 14, margin = margin(0, 10, 0, 0)),
        axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 10),
        plot.margin = margin(5, 10, 5, 5),
        panel.border = element_rect(color = "black", fill = NA, linewidth = 1),
        panel.background = element_rect(fill = NA),
        panel.grid = element_line(color = "#DFDFDF50"))

##--- save plot (will save last plot called)
ggsave(paste0("figures/mean_trips_", tolower(x_lab), ".png"), dpi = 300, width = 7, height = 5)


## catch by trip regression scatterplot
##--- define x-axis limit and interval
x_max <- plyr::round_any(max(dat$Mean_Number_of_Angler_Trips, na.rm = TRUE), accuracy = 2, f = ceiling) # round max value by accuracy value
x_breaks <- seq(0, x_max, 5) # min, max, by interval

##--- define y-axis limit and interval
y_max <- plyr::round_any(max(dat$Mean_HarvestORCatch, na.rm = TRUE), accuracy = 5, f = ceiling) # round max value by accuracy value
y_breaks <- seq(0, y_max, 10) # min, max, by interval

##--- build plot
ggplot(dat, aes(x = Mean_Number_of_Angler_Trips, y = Mean_HarvestORCatch)) + 
  stat_poly_line(se = FALSE, color = "#838383", linetype = "dashed") +
  stat_poly_eq(use_label(c("eq", "R2"))) +
  geom_point(size = 2) + 
  scale_x_continuous(limits = c(-0.2, x_max), breaks = x_breaks, expand = c(0, 0.1)) +
  scale_y_continuous(limits = c(-0.5, y_max), breaks = y_breaks, expand = c(0, 0.1)) +
  labs(x = "Mean Number of Fishing Trips", y = "Mean Catch (# of Fish)") +
  theme(axis.title.x = element_text(size = 14, margin = margin(10, 0, 0, 0)),
        axis.title.y = element_text(size = 14, margin = margin(0, 10, 0, 0)),
        axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 10),
        plot.margin = margin(5, 10, 5, 5),
        panel.border = element_rect(color = "black", fill = NA, linewidth = 1),
        panel.background = element_rect(fill = NA),
        panel.grid = element_line(color = "#DFDFDF50"))

##--- save plot (will save last plot called)
ggsave(paste0("figures/catch_by_trip_regression_", tolower(x_lab), ".png"), dpi = 300, width = 7, height = 5)


## catch by trip regression scatterplot
##--- define x-axis limit and interval
x_max <- plyr::round_any(max(dat$Mean_Fishing_Effort, na.rm = TRUE), accuracy = 25, f = ceiling) # round max value by accuracy value
x_breaks <- seq(0, x_max, 25) # min, max, by interval

##--- define y-axis limit and interval
y_max <- plyr::round_any(max(dat$Mean_HarvestORCatch, na.rm = TRUE), accuracy = 5, f = ceiling) # round max value by accuracy value
y_breaks <- seq(0, y_max, 10) # min, max, by interval

##--- build plot
ggplot(dat, aes(x = Mean_Fishing_Effort, y = Mean_HarvestORCatch)) + 
  stat_poly_line(se = FALSE, color = "#838383", linetype = "dashed") +
  stat_poly_eq(use_label(c("eq", "R2"))) +
  geom_point(size = 2) + 
  scale_x_continuous(limits = c(-1, x_max), breaks = x_breaks, expand = c(0, 0.1)) +
  scale_y_continuous(limits = c(-0.4, y_max), breaks = y_breaks, expand = c(0, 0.1)) +
  labs(x = "Mean Fishing Effort (Hours)", y = "Mean Catch (# of Fish)") +
  theme(axis.title.x = element_text(size = 14, margin = margin(10, 0, 0, 0)),
        axis.title.y = element_text(size = 14, margin = margin(0, 10, 0, 0)),
        axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 10),
        plot.margin = margin(5, 10, 5, 5),
        panel.border = element_rect(color = "black", fill = NA, linewidth = 1),
        panel.background = element_rect(fill = NA),
        panel.grid = element_line(color = "#DFDFDF50"))

##--- save plot (will save last plot called)
ggsave(paste0("figures/catch_by_effort_regression_", tolower(x_lab), ".png"), dpi = 300, width = 7, height = 5)


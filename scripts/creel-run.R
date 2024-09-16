# CLEAR ENVIRONMENT -----------------------------------------------------------------------------------------------
rm(list = ls(all.names=TRUE))
#start.time <- Sys.time()


# LOAD REQUIRED PACKAGES; INSTALL IF MISSING ----------------------------------------------------------------------
if(!require(readxl)) { install.packages("readxl"); library(readxl) }
if(!require(dplyr)) { install.packages("dplyr"); library(dplyr) }
if(!require(tidyr)) { install.packages("tidyr"); library(tidyr) }
if(!require(magrittr)) { install.packages("magrittr"); library(magrittr) }
if(!require(lubridate)) { install.packages("lubridate"); library(lubridate) }


# IMPORT DATA -----------------------------------------------------------------------------------------------------
## Contact
dat_contact <- read_excel("data/test-moonlake-contact.xlsx", sheet = 1)
## Counts
dat_count <- read_excel("data/test-moonlake-count.xlsx", sheet = 1)
## Species Composition
dat_sppcomp <- read_excel("data/test-moonlake-sppcomp.xlsx", sheet = 1)


# DEFINE DATA OF INTEREST (i.e., data to evaluate) ----------------------------------------------------------------
start_date <- c("2018-07-03") # Format must be "yyyy-mm-dd"
end_date <- c("2019-06-29") # Format must be "yyyy-mm-dd"
##--- accurate dates are important to not over inflate the potential number of sampling dates
waterbody_interest <- c("Moon Lake") ## if multiple systems combine with c() e.g., c("East Canyon Res", "Lost Creek Res)

## Run data filtering by dates and waterbody name
source("scripts/data-filter.R")


# LOAD FUNCTIONS --------------------------------------------------------------------------------------------------
## Build calendar from start and end dates to assign DOW and holidays
source("scripts/calendar.R")
## QA/QC
source("scripts/date-check.R")  ## must run calendar.R first to load create_calendar()
## Roving Analyses
source("scripts/roving.R")
## Frequency
source("scripts/freq.R")
## Summary Statistics
source("scripts/summary-stats.R")


# QA/QC -----------------------------------------------------------------------------------------------------------
## Check for consistenecy between data files and if dates and DOW were assigned correctly
qaqc <- check(data1 = dat_contact, data2 = dat_count)
## Open data frame to view QA/QC results
View(qaqc)  ## NA in the "flag" columns indicates no error


# DEFINE STRATIFICATION OPTION ------------------------------------------------------------------------------------
## Stratification options to define stratum. This will determine how creel data are summarized. 
### option 1
### 1. by Month and DOW
### 2. by Month
### 3. Overall

### option 2
### 1. by Month, DOW, and TOD
### 2. by Month and DOW
### 3. by Month
### 4. Overall

### option 3
### 1. by Site, Month, and DOW
### 2. by Site and Month
### 3. by Site
### 4. Overall

### option 4
### 1. by Site, Month, DOW, and TOD
### 2. by Site, Month, and DOW
### 3. by Site and Month
### 4. by Site 
### 5. Overall

## Define stratification option
opt <- 2


# DEFINE GROUPING VARIABLES BASED ON STRATIFICATION ---------------------------------------------------------------
if(opt == 1 | opt == 2) {
  grp_var_vec <- c("month", "day", "dow", "tod")
  ## Remove 'day' by position in vector above. Adjust accordingly.
  grp_var_vec2 <- grp_var_vec[-2]
  
} else if(opt == 3 | opt == 4) {
  grp_var_vec <- c("month", "day", "dow", "tod", "site")
  ## Remove 'day' by position in vector above. Adjust accordingly.
  grp_var_vec2 <- grp_var_vec[-2]
}


# CALCULATE OVERALL CATCH CREEL STATISTICS ------------------------------------------------------------------------
## Statistics for contact data set
## Grouping variables should match the variables listed in choice one of the stratification option, plus 'day' variable.
roving1a(data = dat_contact, grp_var = grp_var_vec, effort = "effort", catch = "catch")
##--- IGNORE WARNING ABOUT USING EXTERNAL VECTORS
##--- output is saved as "contact_sum" in environment
##--- NOTE: if "tot_anglers" = 1, no variance components can be calculated and will be stated as NaN

## Statistics for count data set
## Grouping variables should match the variables listed in choice one of the stratification option, plus 'day' variable
roving1b(data = dat_count, grp_var = grp_var_vec, count = "totcnt", sample_time = "losp")
##--- output is saved as "count_sum" in environment
##--- NOTE: if "tot_anglers" = 1, variance components will be zero

## Statistics by stratum
## Grouping variables should match the variables listed in choice one of the stratification option.
roving1c(grp_var = grp_var_vec2)
##--- output is saved as "stratum_sum" in environment
##--- NOTE: if "n" = 1, variance components will be zero or NA

## Summarize by options defined above
roving1d(option = opt)
##--- outputs are saved as multiple objects in environment based on grouping option defined.
##--- each object will be prefixed with "stat".

## Export Overall CATCH
if(opt == 1 | opt == 3) {
  write.csv(statdow, "tables/catch_overall_dow.csv", row.names = FALSE)
  write.csv(statmon, "tables/catch_overall_mon.csv", row.names = FALSE)
  write.csv(statall, "tables/catch_overall_all.csv", row.names = FALSE)
} else if(opt == 2 | opt == 4) {
  write.csv(stattod, "tables/catch_overall_tod.csv", row.names = FALSE)
  write.csv(statdow, "tables/catch_overall_dow.csv", row.names = FALSE)
  write.csv(statmon, "tables/catch_overall_mon.csv", row.names = FALSE)
  write.csv(statall, "tables/catch_overall_all.csv", row.names = FALSE)
}

# CALCULATE OVERALL HARVEST CREEL STATISTICS -----------------------------------------------------------------
## Statistics for contact data set
## Grouping variables should match the variables listed in choice one of the stratification option, plus 'day' variable.
roving1a(data = dat_contact, grp_var = grp_var_vec, effort = "effort", catch = "harvest")
##--- output is saved as "contact_sum" in environment
##--- NOTE: if "tot_anglers" = 1, no variance components can be calculated and will be stated as NaN

## Statistics for count data set
## Grouping variables should match the variables listed in choice one of the stratification option, plus 'day' variable
roving1b(data = dat_count, grp_var = grp_var_vec, count = "totcnt", sample_time = "losp")
##--- output is saved as "count_sum" in environment
##--- NOTE: if "tot_anglers" = 1, variance components will be zero

## Statistics by stratum
## Grouping variables should match the variables listed in choice one of the stratification option.
roving1c(grp_var = grp_var_vec2)
##--- output is saved as "stratum_sum" in environment
##--- NOTE: if "n" = 1, variance components will be zero or NA

## Summarize by options defined above
roving1d(option = opt)
##--- outputs are saved as multiple objects in environment based on grouping option defined.
##--- each object will be prefixed with "stat".

## Export Overall HARVEST
if(opt == 1 | opt == 3) {
  write.csv(statdow, "tables/harvest_overall_dow.csv", row.names = FALSE)
  write.csv(statmon, "tables/harvest_overall_mon.csv", row.names = FALSE)
  write.csv(statall, "tables/harvest_overall_all.csv", row.names = FALSE)
} else if(opt == 2 | opt == 4) {
  write.csv(stattod, "tables/harvest_overall_tod.csv", row.names = FALSE)
  write.csv(statdow, "tables/harvest_overall_dow.csv", row.names = FALSE)
  write.csv(statmon, "tables/harvest_overall_mon.csv", row.names = FALSE)
  write.csv(statall, "tables/harvest_overall_all.csv", row.names = FALSE)
}


# CALCULATE SHORE CATCH STATISTIC ---------------------------------------------------------------------------------
## Create data frame with shore anglers
dat_shore <- dat_contact %>% filter(method == "S") ## case sensitive!!

## Statistics for contact data set
## Grouping variables should match the variables listed in choice one of the stratification option, plus 'day' variable.
roving1a(data = dat_shore, grp_var = grp_var_vec, effort = "effort", catch = "catch")

## Statistics for count data set
## Grouping variables should match the variables listed in choice one of the stratification option, plus 'day' variable
roving1b(data = dat_count, grp_var = grp_var_vec, count = "totcnt", sample_time = "losp")

## Statistics by stratum
## Grouping variables should match the variables listed in choice one of the stratification option.
roving1c(grp_var = grp_var_vec2)

## Summarize by options defined above
roving1d(option = opt)

## Export shore CATCH
if(opt == 1 | opt == 3) {
  write.csv(statdow, "tables/catch_shore_dow.csv", row.names = FALSE)
  write.csv(statmon, "tables/catch_shore_mon.csv", row.names = FALSE)
  write.csv(statall, "tables/catch_shore_all.csv", row.names = FALSE)
} else if(opt == 2 | opt == 4) {
  write.csv(stattod, "tables/catch_shore_tod.csv", row.names = FALSE)
  write.csv(statdow, "tables/catch_shore_dow.csv", row.names = FALSE)
  write.csv(statmon, "tables/catch_shore_mon.csv", row.names = FALSE)
  write.csv(statall, "tables/catch_shore_all.csv", row.names = FALSE)
}

# CALCULATE SHORE HARVEST STATISTIC -------------------------------------------------------------------------------
## Statistics for contact data set
## Grouping variables should match the variables listed in choice one of the stratification option, plus 'day' variable.
roving1a(data = dat_shore, grp_var = grp_var_vec, effort = "effort", catch = "harvest")

## Statistics for count data set
## Grouping variables should match the variables listed in choice one of the stratification option, plus 'day' variable
roving1b(data = dat_count, grp_var = grp_var_vec, count = "totcnt", sample_time = "losp")

## Statistics by stratum
## Grouping variables should match the variables listed in choice one of the stratification option.
roving1c(grp_var = grp_var_vec2)

## Summarize by options defined above
roving1d(option = opt)

## Export shore HARVEST
if(opt == 1 | opt == 3) {
  write.csv(statdow, "tables/harvest_shore_dow.csv", row.names = FALSE)
  write.csv(statmon, "tables/harvest_shore_mon.csv", row.names = FALSE)
  write.csv(statall, "tables/harvest_shore_all.csv", row.names = FALSE)
} else if(opt == 2 | opt == 4) {
  write.csv(stattod, "tables/harvest_shore_tod.csv", row.names = FALSE)
  write.csv(statdow, "tables/harvest_shore_dow.csv", row.names = FALSE)
  write.csv(statmon, "tables/harvest_shore_mon.csv", row.names = FALSE)
  write.csv(statall, "tables/harvest_shore_all.csv", row.names = FALSE)
}


# CALCULATE BOAT CATCH STATISTIC ----------------------------------------------------------------------------------
## Create data frame with boat anglers
dat_boat <- dat_contact %>% filter(method %in% c("B", "P", "T", "F", "K")) ## case sensitive!!

## Statistics for contact data set
## Grouping variables should match the variables listed in choice one of the stratification option, plus 'day' variable.
roving1a(data = dat_boat, grp_var = grp_var_vec, effort = "effort", catch = "catch")

## Statistics for count data set
## Grouping variables should match the variables listed in choice one of the stratification option, plus 'day' variable
roving1b(data = dat_count, grp_var = grp_var_vec, count = "totcnt", sample_time = "losp")

## Statistics by stratum
## Grouping variables should match the variables listed in choice one of the stratification option.
roving1c(grp_var = grp_var_vec2)

## Summarize by options defined above
roving1d(option = opt)

## Export boat CATCH
if(opt == 1 | opt == 3) {
  write.csv(statdow, "tables/catch_boat_dow.csv", row.names = FALSE)
  write.csv(statmon, "tables/catch_boat_mon.csv", row.names = FALSE)
  write.csv(statall, "tables/catch_boat_all.csv", row.names = FALSE)
} else if(opt == 2 | opt == 4) {
  write.csv(stattod, "tables/catch_boat_tod.csv", row.names = FALSE)
  write.csv(statdow, "tables/catch_boat_dow.csv", row.names = FALSE)
  write.csv(statmon, "tables/catch_boat_mon.csv", row.names = FALSE)
  write.csv(statall, "tables/catch_boat_all.csv", row.names = FALSE)
}

# CALCULATE BOAT HARVEST STATISTIC --------------------------------------------------------------------------------
## Statistics for contact data set
## Grouping variables should match the variables listed in choice one of the stratification option, plus 'day' variable.
roving1a(data = dat_boat, grp_var = grp_var_vec, effort = "effort", catch = "harvest")

## Statistics for count data set
## Grouping variables should match the variables listed in choice one of the stratification option, plus 'day' variable
roving1b(data = dat_count, grp_var = grp_var_vec, count = "totcnt", sample_time = "losp")

## Statistics by stratum
## Grouping variables should match the variables listed in choice one of the stratification option.
roving1c(grp_var = grp_var_vec2)

## Summarize by options defined above
roving1d(option = opt)

## Export boat HARVEST
if(opt == 1 | opt == 3) {
  write.csv(statdow, "tables/harvest_boat_dow.csv", row.names = FALSE)
  write.csv(statmon, "tables/harvest_boat_mon.csv", row.names = FALSE)
  write.csv(statall, "tables/harvest_boat_all.csv", row.names = FALSE)
} else if(opt == 2 | opt == 4) {
  write.csv(stattod, "tables/harvest_boat_tod.csv", row.names = FALSE)
  write.csv(statdow, "tables/harvest_boat_dow.csv", row.names = FALSE)
  write.csv(statmon, "tables/harvest_boat_mon.csv", row.names = FALSE)
  write.csv(statall, "tables/harvest_boat_all.csv", row.names = FALSE)
}


# GEAR TYPE FREQUENCY ----------------------------------------------------------------------------------------
## Gear frequency - Overall
gear_freq <- freq(data = dat_contact, grp_var = "gear")
write.csv(gear_freq, "tables/gear_overall.csv", row.names = FALSE)

## Gear frequency by month
gear_freq_mon <- freq(data = dat_contact, grp_var = c("month", "gear"))
write.csv(gear_freq_mon, "tables/gear_by_month.csv", row.names = FALSE)

## Gear frequency by method
gear_freq_method <- freq(data = dat_contact, grp_var = c("method", "gear"))
write.csv(gear_freq_method, "tables/gear_by_method.csv", row.names = FALSE)


# DEFINE LIST OF SPECIES TO ANALYZE INDEPENDENTLY -----------------------------------------------------------------
## use the species abbreviations in the headers (e.g., catch of rainbow trout is labelled as 'c_rbt' so define 'rbt' below)
spp_vec <- c("rbt", "tg", "splake", "bk", "crct", "kok")


# CALCULATE CATCH CREEL STATISTICS BY SPECIES ---------------------------------------------------------------------
## run loop over vector of species above
lapply(spp_vec, function(spp) {
  ## Statistics for contact data set
  ## Grouping variables should match the variables listed in choice one of the stratification option, plus 'day' variable.
  roving1a(data = dat_contact, grp_var = grp_var_vec, effort = "effort", catch = paste0("c_", spp))
  
  ## Statistics for count data set
  ## Grouping variables should match the variables listed in choice one of the stratification option, plus 'day' variable
  roving1b(data = dat_count, grp_var = grp_var_vec, count = "totcnt", sample_time = "losp")
  
  ## Statistics by stratum
  ## Grouping variables should match the variables listed in choice one of the stratification option.
  roving1c(grp_var = grp_var_vec2)
  
  ## Summarize by options defined above
  roving1d(option = opt)
  

  ## Export species CATCH by each stratification level
  if(opt == 1 | opt == 3) {
    if(exists("statdow_spp") == FALSE) {
      statdow_spp <<- statdow %<>% mutate(Species = spp) %>% 
        select(Waterbody, Species, everything())
    } else {
      statdow %<>% mutate(Species = spp)
      statdow_spp <<- rbind(statdow_spp, statdow)
    }
    if(exists("statmon_spp") == FALSE) {
      statmon_spp <<- statmon %<>% mutate(Species = spp) %>% 
        select(Waterbody, Species, everything())
    } else {
      statmon %<>% mutate(Species = spp)
      statmon_spp <<- rbind(statmon_spp, statmon)
    }
    if(exists("statall_spp") == FALSE) {
      statall_spp <<- statall %<>% mutate(Species = spp) %>% 
        select(Waterbody, Species, everything())
    } else {
      statall %<>% mutate(Species = spp)
      statall_spp <<- rbind(statall_spp, statall)
    }
  } else if(opt == 2 | opt == 4) {
    if(exists("stattod_spp") == FALSE) {
      stattod_spp <<- stattod %<>% mutate(Species = spp) %>% 
        select(Waterbody, Species, everything())
    } else {
      stattod %<>% mutate(Species = spp)
      stattod_spp <<- rbind(stattod_spp, stattod)
    }
    if(exists("statdow_spp") == FALSE) {
      statdow_spp <<- statdow %<>% mutate(Species = spp) %>% 
        select(Waterbody, Species, everything())
    } else {
      statdow %<>% mutate(Species = spp)
      statdow_spp <<- rbind(statdow_spp, statdow)
    }
    if(exists("statmon_spp") == FALSE) {
      statmon_spp <<- statmon %<>% mutate(Species = spp) %>% 
        select(Waterbody, Species, everything())
    } else {
      statmon %<>% mutate(Species = spp)
      statmon_spp <<- rbind(statmon_spp, statmon)
    }
    if(exists("statall_spp") == FALSE) {
      statall_spp <<- statall %<>% mutate(Species = spp) %>% 
        select(Waterbody, Species, everything())
    } else {
      statall %<>% mutate(Species = spp)
      statall_spp <<- rbind(statall_spp, statall)
    }
  }
})

##---CAUTION: Rerunning the loop above will only append new data to the previous data (no overwriting)
##---Run the line of code below to clear previous data
#rm(stattod_spp, statdow_spp, statmon_spp, statall_spp)

## Save catch by species files
if(opt == 1 | opt == 3) {
  write.csv(statdow_spp, paste0("tables/catch_by_species_dow.csv"), row.names = FALSE)
  write.csv(statmon_spp, paste0("tables/catch_by_species_mon.csv"), row.names = FALSE)
  write.csv(statall_spp, paste0("tables/catch_by_species_all.csv"), row.names = FALSE)
} else if(opt == 2 | opt == 4) {
  write.csv(stattod_spp, paste0("tables/catch_by_species_tod.csv"), row.names = FALSE)
  write.csv(statdow_spp, paste0("tables/catch_by_species_dow.csv"), row.names = FALSE)
  write.csv(statmon_spp, paste0("tables/catch_by_species_mon.csv"), row.names = FALSE)
  write.csv(statall_spp, paste0("tables/catch_by_species_all.csv"), row.names = FALSE)
}


# CALCULATE HARVEST CREEL STATISTICS BY SPECIES -------------------------------------------------------------------
## run loop over vector of species above
lapply(spp_vec, function(spp) {
  ## Statistics for contact data set
  ## Grouping variables should match the variables listed in choice one of the stratification option, plus 'day' variable.
  roving1a(data = dat_contact, grp_var = grp_var_vec, effort = "effort", catch = paste0("h_", spp))
  
  ## Statistics for count data set
  ## Grouping variables should match the variables listed in choice one of the stratification option, plus 'day' variable
  roving1b(data = dat_count, grp_var = grp_var_vec, count = "totcnt", sample_time = "losp")
  
  ## Statistics by stratum
  ## Grouping variables should match the variables listed in choice one of the stratification option.
  roving1c(grp_var = grp_var_vec2)
  
  ## Summarize by options defined above
  roving1d(option = opt)
  
  ## Export species HARVEST by each stratification level
  if(opt == 1 | opt == 3) {
    if(exists("statdow_spp") == FALSE) {
      statdow_spp <<- statdow %<>% mutate(Species = spp) %>% 
        select(Waterbody, Species, everything())
    } else {
      statdow %<>% mutate(Species = spp)
      statdow_spp <<- rbind(statdow_spp, statdow)
    }
    if(exists("statmon_spp") == FALSE) {
      statmon_spp <<- statmon %<>% mutate(Species = spp) %>% 
        select(Waterbody, Species, everything())
    } else {
      statmon %<>% mutate(Species = spp)
      statmon_spp <<- rbind(statmon_spp, statmon)
    }
    if(exists("statdow_spp") == FALSE) {
      statdow_spp <<- statdow %<>% mutate(Species = spp) %>% 
        select(Waterbody, Species, everything())
    } else {
      statdow %<>% mutate(Species = spp)
      statdow_spp <<- rbind(statdow_spp, statdow)
    }
  } else if(opt == 2 | opt == 4) {
    if(exists("stattod_spp") == FALSE) {
      stattod_spp <<- stattod %<>% mutate(Species = spp) %>% 
        select(Waterbody, Species, everything())
    } else {
      stattod %<>% mutate(Species = spp)
      stattod_spp <<- rbind(stattod_spp, stattod)
    }
    if(exists("statdow_spp") == FALSE) {
      statdow_spp <<- statdow %<>% mutate(Species = spp) %>% 
        select(Waterbody, Species, everything())
    } else {
      statdow %<>% mutate(Species = spp)
      statdow_spp <<- rbind(statdow_spp, statdow)
    }
    if(exists("statmon_spp") == FALSE) {
      statmon_spp <<- statmon %<>% mutate(Species = spp) %>% 
        select(Waterbody, Species, everything())
    } else {
      statmon %<>% mutate(Species = spp)
      statmon_spp <<- rbind(statmon_spp, statmon)
    }
    if(exists("statdow_spp") == FALSE) {
      statdow_spp <<- statdow %<>% mutate(Species = spp) %>% 
        select(Waterbody, Species, everything())
    } else {
      statdow %<>% mutate(Species = spp)
      statdow_spp <<- rbind(statdow_spp, statdow)
    }
  }
})

##---CAUTION: Rerunning the loop above will only append new data to the previous data (no overwriting)
##---Run the line of code below to clear previous data
#rm(stattod_spp, statdow_spp, statmon_spp, statall_spp)

## Save harvest by species files
if(opt == 1 | opt == 3) {
  write.csv(statdow_spp, paste0("tables/harvest_by_species_dow.csv"), row.names = FALSE)
  write.csv(statmon_spp, paste0("tables/harvest_by_species_mon.csv"), row.names = FALSE)
  write.csv(statall_spp, paste0("tables/harvest_by_species_all.csv"), row.names = FALSE)
} else if(opt == 2 | opt == 4) {
  write.csv(stattod_spp, paste0("tables/harvest_by_species_tod.csv"), row.names = FALSE)
  write.csv(statdow_spp, paste0("tables/harvest_by_species_dow.csv"), row.names = FALSE)
  write.csv(statmon_spp, paste0("tables/harvest_by_species_mon.csv"), row.names = FALSE)
  write.csv(statall_spp, paste0("tables/harvest_by_species_all.csv"), row.names = FALSE)
}


# SUMMARY STATISTICS -----------------------------------------------------------------------------------------
sum_stats <- creel_summary_stats(data = dat_contact, grp_var = c("month", "resid"), var = "num_ind")
sum_stats

#end.time <- Sys.time()
#end.time - start.time

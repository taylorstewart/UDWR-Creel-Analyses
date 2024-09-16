# CREATE NEW DFs TO TEMPORARILY WORK WITH -------------------------------------------------------------------------
dat_contact_tmp <- dat_contact %>% 
  mutate(date = as.Date(paste0(month, "-", day, "-", year), format = "%m-%d-%Y"))
dat_count_tmp <- dat_count %>% 
  mutate(date = as.Date(paste0(month, "-", day, "-", year), format = "%m-%d-%Y"))
dat_sppcomp_tmp <- dat_sppcomp %>% 
  mutate(date = as.Date(paste0(month, "-", day, "-", year), format = "%m-%d-%Y"))


# CHECK DATE FORMAT -----------------------------------------------------------------------------------------------
start_date_try <- try(
  as.Date(start_date, format = "%Y-%m-%d")
)
if(is.na(start_date_try) == TRUE) {print("Start date incorrect.")}
end_date_try <- try(
  as.Date(end_date, format = "%Y-%m-%d")
)
if(is.na(end_date_try) == TRUE) {print("End date incorrect.")}


if(is.na(start_date_try) == FALSE && is.na(end_date_try) == FALSE) {
  # FILTER CONTACT DATA ---------------------------------------------------------------------------------------------
  contact_try <- try(
    dat_contact_tmp %>% filter(date >= as.Date(start_date, format = "%Y-%m-%d"), 
                               date <= as.Date(end_date, format = "%Y-%m-%d"),
                               water_id %in% c(waterbody_interest))
  )
  
  if(nrow(contact_try) == 0) {print("Contact data filtering unsuccessful. Check filtering inputs and data format.")}
  if(nrow(contact_try) > 0) {print("Contact data filtering successful.")}
  
  
  # FILTER COUNT DATA -----------------------------------------------------------------------------------------------
  count_try <- try(
    dat_count_tmp %>% filter(date >= as.Date(start_date, format = "%Y-%m-%d"), 
                             date <= as.Date(end_date, format = "%Y-%m-%d"),
                             water_id %in% c(waterbody_interest))
  )
  
  if(nrow(count_try) == 0) {print("Count data filtering unsuccessful. Check filtering inputs and data format.")}
  if(nrow(count_try) > 0) {print("Count data filtering successful.")}
  
  
  # FILTER SPECIES COMPOSITION DATA ---------------------------------------------------------------------------------
  sppcomp_try <- try(
    dat_sppcomp_tmp %>% filter(date >= as.Date(start_date, format = "%Y-%m-%d"), 
                               date <= as.Date(end_date, format = "%Y-%m-%d"),
                               water_id %in% c(waterbody_interest))
  )
  
  if(nrow(sppcomp_try) == 0) {print("Species composition data filtering unsuccessful. Check filtering inputs and data format.")}
  if(nrow(sppcomp_try) > 0) {print("Species composition data filtering successful.")}
  
  
  # OVERWRITE PREVIOUS DATA WITH FILTERED DATA ----------------------------------------------------------------------
  dat_contact <- contact_try
  dat_count <- count_try
  dat_sppcomp <- sppcomp_try
}

# REMOVE TEMP FILES -----------------------------------------------------------------------------------------------
options(warn = -1)
rm(dat_contact_tmp, contact_try, dat_count_tmp, count_try, dat_sppcomp_tmp, sppcomp_try, start_date_try, end_date_try)
options(warn = 0)

# ------------------------------------------------------------------------------------------------------------
# Script to analyze roving surveys with Site/Zone, Monthly, DOW, and TOD stratification (i.e., multi-stage sampling)
# Written by: Taylor Stewart
# Created: 02/23/2024

# Original SAS code written by: Heather Thomas
# ------------------------------------------------------------------------------------------------------------


# This is the beginning of the first function
roving1a <- function(data = dat_contact, grp_var = c("year", "month", "day", "dow", "tod"), effort = "effort", catch = "catch") {

  ## Statistics needed to calculate mean harvest/catch rate for a sampled period. Output gives mean time
  ## anglers spent fishing (this is obtained from effort) and mean harvest/catch (catch) for a sampled period.
  ## The variance for the time anglers spent fishing is also calculated. Statistics needed to calculate 
  ## mean harvest/catch per unit effort for a sampled period.
  rate <- data %>% group_by(across(grp_var)) %>% 
    summarize(tot_anglers = n(),
              mn_hrs = mean(!! sym(effort), na.rm = TRUE),
              mn_fsh = mean(!! sym(catch), na.rm = TRUE),
              var_effort = var(!! sym(effort), na.rm = TRUE)) %>% 
    ungroup()
  
  ## This step calculates harvest/catch rate for a sampled period (rpue). Note that the mean harvest/catch
  ## must be in the numerator and mean time spent fishing in the denominator.
  rate %<>% mutate(rpue = mn_fsh/mn_hrs)
  
  ## Rate is merged into original data set to calculate the numerator of the variance of harvest/catch rate.
  ## (harvest/catch - time spent fishing*harvest/catch rate)^2  
  ## Summarize by grouping variables
  numerator <- left_join(data, rate)  %>% 
    arrange(across(grp_var)) %>% 
    mutate(num = (!! sym(catch) - (!! sym(effort) * rpue))^2) %>% 
    group_by(across(grp_var)) %>% 
    summarize(sumnum = sum(num, na.rm = TRUE)) %>% 
    ungroup()
  
  ## Merge together two above data sets to calculate harvest/catch rate variance (v_rpue):
  ## v_rpue = sumnum / (mean time spent fishing)^2 * count angler parties * (count of angler parties - 1).
  ## Variance for the mean length of an angler-trip is also calculated (v_ebar).
  contact_sum <- left_join(rate, numerator) %>% 
    mutate(var_rpue = sumnum / (mn_hrs^2 * tot_anglers * (tot_anglers - 1)),
           var_effort = var_effort / tot_anglers)
  
  contact_sum <<- contact_sum
}



# This begins the second function. This function calculates statistics for count data set.
roving1b <- function(data = dat_count, grp_var = c("year", "month", "day", "dow", "tod"), count = "totcnt", sample_time = "losp") {

  ## Computes statistics needed to calculate mean and variance of fishing effort for a sampled period. 
  ## Output gives mean and variance for number of anglers counted fishing. Computes statistics needed 
  ## to calculate mean and variance of fishing effort for a sampled period.
  avgcount <- data %>% group_by(across(grp_var)) %>% 
    summarize(n = n(),
              mean_angler_count = mean(!! sym(count), na.rm = TRUE),
              var_angler_count = var(!! sym(count), na.rm = TRUE)) %>% 
    ungroup()
  
  ## Extract sampling period from data
  grp_sample_period <- data %>% 
    select(any_of(grp_var), !! sym(sample_time)) %>% 
    distinct()
  
  ## This step calculates fishing effort for a sampled period (E): 
  ## E = mean number of angler parties counted (in a sampled period)*length of the sampling period. 
  ## The variance for fishing effort is also calculated (v_E) for each sampled period: 
  ## v_E = (length of the sampling period)^2 * variance of the number of counted angler parties in a sampled period.
  count1 <- left_join(grp_sample_period, avgcount) %>% 
    mutate(E = mean_angler_count * !! sym(sample_time),
           var_E = ((!! sym(sample_time))^2) * var_angler_count)
  
  ## This step calculates harvest/catch means and variances and the mean and variance for the number of angler-trips. 
  ## Mean and variance for fishing effort is also merged into this data set. This gives statistics for each sampled period.
  count_sum <- left_join(count1, contact_sum) %>% 
    mutate(d = n(),
           h_c1 = E * rpue,
           numtrip = E/mn_hrs,
           var_hc1 = (var_rpue * (E^2)) + (var_E * (rpue^2)) - (var_E * var_rpue),
           var_numtrip = numtrip^2 * ((var_effort / (mn_hrs^2)) + (var_E / E^2) - ((var_effort * var_E) / (mn_hrs^2 * E^2)))) %>% 
    replace(is.na(.), 0)

  count_sum <<- count_sum
}



# This begins the third function. This begins calculations for statistics for each stratum.
roving1c <- function(grp_var = c("year", "month", "dow", "tod")) {

  fish1 <- count_sum %>% 
    group_by(across(grp_var)) %>% 
    summarize(n = n(),
            ## mean
            E = mean(E, na.rm = TRUE),
            h_c1 = mean(h_c1, na.rm = TRUE),
            numtrip = mean(numtrip, na.rm = TRUE),
            rpue = mean(rpue, na.rm = TRUE),
            mn_hrs = mean(mn_hrs, na.rm = TRUE),
            mn_fsh = mean(mn_fsh, na.rm = TRUE)) %>% 
    ungroup()
  
  fish2 <- count_sum %>% 
    group_by(across(grp_var)) %>% 
    summarize(var_E = sum(var_E, na.rm = TRUE),
              var_hc1 = sum(var_hc1, na.rm = TRUE),
              var_numtrip = sum(var_numtrip, na.rm = TRUE)) %>% 
    ungroup()
  
  ## variance
  fish3 <- count_sum %>% 
    group_by(across(grp_var)) %>% 
    summarize(var_E1 = var(E, na.rm = TRUE),
              var_harcat1 = var(h_c1, na.rm = TRUE),
              var_numtrip1 = var(numtrip, na.rm = TRUE)) %>% 
    ungroup()
  
  fish4 <- left_join(fish1, fish2) %>% 
    left_join(fish3)
  
  stratum_sum <<- fish4
}



# This begins the fourth function.
roving1d <- function(option = 1) {
  
  # Determine the number of possible sampling periods within each stratum (D).
  n_periods <- create_calendar(start_date, end_date) %>% 
    group_by(month, dow) %>%
    count() %>% 
    ungroup() %>% 
    rename(n_periods = n) %>% 
    filter(month >= min(count_sum$month), month <= max(count_sum$month))


# Option 1 --------------------------------------------------------------------------------------------------------
  ## This block produces the following statistics:
  ## 1. by Month DOW
  ## 2. by Month
  ## 3. Overall
  if(option == 1) {
    statdow1 <- left_join(stratum_sum, n_periods) %>% 
      group_by(month, dow) %>% 
      reframe(
        dEm = E,
        dhc_m = h_c1,
        dnumtrpm = numtrip,
        drpuem = rpue,
        M = n_periods,
        d_fsh1 = mn_fsh,
        d_hrs1 = mn_hrs,
        # Calculate estimates of population totals
        dtot_E = M * E,
        dtot_hc = M * h_c1,
        dtot_trp = M * numtrip,
        # Calculate sampling fraction
        f_1h = n / M,
        fpc1 = 1 - f_1h,
        f1_inv = 1 / f_1h,
        # Calculate variance between sampling periods - means and totals
        s_E1m = fpc1 * (var_E1 / n),
        s_hc1m = fpc1 * (var_harcat1 / n),
        s_trip1m = fpc1 * (var_numtrip1 / n),
        # Calculate variance for totals
        s_E1t = M^2 * s_E1m,
        s_hc1t = M^2 * s_hc1m,
        s_trip1t = M^2 * s_trip1m,
        # Calculate variance within sampling periods - for means
        s_E2m = (1 / (n * M)) * var_E,
        s_hc2m = (1 / (n * M)) * var_hc1,
        s_trip2m = (1 / (n * M)) * var_numtrip,
        # Calculate variance within sampling periods - for totals
        s_E2 = f1_inv * var_E,
        s_hc2 = f1_inv * var_hc1,
        s_trip2 = f1_inv * var_numtrip,
        # Combine between and within variances to get two-stage variance estimates for each stratum - means and totals
        ds_Em = s_E1m + s_E2m,
        ds_hcm = s_hc1m + s_hc2m,
        ds_tripm = s_trip1m + s_trip2m,
        ds_Et = s_E1t + s_E2,
        ds_hct = s_hc1t + s_hc2,
        ds_tript = s_trip1t + s_trip2,
        # Calculate standard errors - means and totals
        dSE_Em = sqrt(ds_Em),
        dSE_hcm = sqrt(ds_hcm),
        dSE_trpm = sqrt(ds_tripm),
        dSE_Et = sqrt(ds_Et),
        dSE_hct = sqrt(ds_hct),
        dSE_trpt = sqrt(ds_tript)) %>% 
      ungroup()
  
  
    calm2a <- statdow1 %>% group_by(month) %>% 
      summarize(NM = sum(M)) %>% 
      ungroup()
    
    # Calculate sampling fraction
    fishery3 <- left_join(statdow1, calm2a) %>% 
      mutate(mW = M / NM,
             mW2 = mW^2,
             mn_fsh1 = d_fsh1 * mW,
             mn_hrs1 = d_hrs1 * mW,
             Et1 = dEm * mW,
             s_Et1m = ds_Em * mW2,
             h_c2 = dhc_m * mW,
             s_hct1m = ds_hcm * mW2,
             numtrpt1 = dnumtrpm * mW,
             s_trpt1m = ds_tripm * mW2)
    
    fishery4 <- fishery3 %>% 
      group_by(month) %>% 
      summarize(mEm = sum(Et1, na.rm = TRUE),
             mhc_m = sum(h_c2, na.rm = TRUE),
             mnumtrpm = sum(numtrpt1, na.rm = TRUE),
             mn_fsh1 = sum(d_fsh1, na.rm = TRUE),
             mn_hrs1 = sum(d_hrs1, na.rm = TRUE),
             ms_Em = sum(s_Et1m, na.rm = TRUE),
             ms_hcm = sum(s_hct1m, na.rm = TRUE),
             ms_tripm = sum(s_trpt1m, na.rm = TRUE)) %>% 
      ungroup()
    

    statmon1 <- left_join(calm2a, fishery4) %>% 
      mutate(mrpuem = mn_fsh1 / mn_hrs1,
             tot_NM = NM^2,
             # Calculate estimates of population totals
             mtot_E = NM * mEm,
             mtot_hc = NM * mhc_m,
             mtot_trp = NM * mnumtrpm,
             # Calculate variance for totals
             ms_Et = tot_NM * ms_Em,
             ms_hct = tot_NM * ms_hcm,
             ms_tript = tot_NM * ms_tripm,
             # Calculate standard errors - means and totals
             mSE_Em = sqrt(ms_Em),
             mSE_hcm = sqrt(ms_hcm),
             mSE_trpm = sqrt(ms_tripm),
             mSE_Et = sqrt(ms_Et),
             mSE_hct = sqrt(ms_hct),
             mSE_trpt = sqrt(ms_tript))
    
    
    ## Now, statistics for harvest/catch, effort, and number of angler trips adding all strata combined - Overall.
    calt <- statmon1 %>% 
      summarize(tot_N = sum(NM)) %>% 
      pull()
    
    # Merge calt2 and statmon1
    fishery5 <- statmon1 %>% 
      mutate(tot_N = calt,
             W = NM / tot_N,
             W2 = W^2,
             mn_fsh2 = mn_fsh1 * W,
             mn_hrs2 = mn_hrs1 * W,
             Et2 = mEm * W,
             s_Et2m = ms_Em * W2,
             h_c3 = mhc_m * W,
             s_hct2m = ms_hcm * W2,
             numtrpt2 = mnumtrpm * W,
             s_trpt2m = ms_tripm * W2)
    
    fishery6 <- fishery5 %>% 
      #group_by(year) %>% 
      summarize(Em = sum(Et2),
                s_Em = sum(s_Et2m),
                hc_m = sum(h_c3),
                s_hcm = sum(s_hct2m),
                numtrpm = sum(numtrpt2),
                s_tripm = sum(s_trpt2m),
                mn_fsh3 = sum(mn_fsh2),
                mn_hrs3 = sum(mn_hrs2))
    
    statall1 <- fishery6 %>% 
      mutate(tot_N = calt,
             rpuem = mn_fsh3/mn_hrs3,
             tot_N2 = tot_N^2,
             # Calculate estimates of population totals
             tot_E = tot_N * Em,
             tot_hc = tot_N * hc_m,
             tot_trp = tot_N * numtrpm,
             # Calculate variances for totals
             s_Et = tot_N^2 * s_Em,
             s_hct = tot_N^2 * s_hcm,
             s_tript = tot_N^2 * s_tripm,
             # Calculate standard errors
             SE_Em = sqrt(s_Em),
             SE_hcm = sqrt(s_hcm),
             SE_tripm = sqrt(s_tripm),
             SE_Et = sqrt(s_Et),
             SE_hct = sqrt(s_hct),
             SE_tript = sqrt(s_tript))

    ## Create summary tables
    statdow <- statdow1 %>% 
      mutate(system = waterbody_interest) %>% 
      select(system, month, dow, dEm, dSE_Em, dtot_E, dSE_Et, dhc_m, dSE_hcm, dtot_hc, dSE_hct, dnumtrpm, 
             dSE_trpm, dtot_trp, dSE_trpt, drpuem)
    # Set labels for variables
    colnames(statdow) <- c(
      system = "Waterbody",
      #year = "Year",
      month = "Month",
      dow = "Day of Week",
      dEm = "Mean Fishing Effort",
      dSE_Em = "SE for Mean Fishing Effort",
      dtot_E = "Total Fishing Effort",
      dSE_Et = "SE for Total Fishing Effort",
      dhc_m = "Mean HarvestORCatch",
      dSE_hcm = "SE for Mean HarvestORCatch",
      dtot_hc = "Total HarvestORCatch",
      dSE_hct = "SE for Total HarvestORCatch",
      dnumtrpm = "Mean Number of Angler Trips",
      dSE_tripm = "SE for Mean Number of Angler Trips",
      dtot_trp = "Total Number of Angler Trips",
      dSE_tript = "SE for Total Number of Angler Trips",
      drpuem = "Mean HarvestORCatch Per Unit Effort"
    )
    statdow <<- statdow
    
    statmon <- statmon1 %>% 
      mutate(system = waterbody_interest) %>% 
      select(system, month, mEm, mSE_Em, mtot_E, mSE_Et, mhc_m, mSE_hcm, mtot_hc, mSE_hct, mnumtrpm, mSE_trpm, mtot_trp, 
             mSE_trpt, mrpuem)
    # Set labels for variables
    colnames(statmon) <- c(
      system = "Waterbody",
      #year = "Year",
      month = "Month",
      mEm = "Mean Fishing Effort",
      mSE_Em = "SE for Mean Fishing Effort",
      mtot_E = "Total Fishing Effort",
      mSE_Et = "SE for Total Fishing Effort",
      mhc_m = "Mean HarvestORCatch",
      mSE_hcm = "SE for Mean HarvestORCatch",
      mtot_hc = "Total HarvestORCatch",
      mSE_hct = "SE for Total HarvestORCatch",
      mnumtrpm = "Mean Number of Angler Trips",
      mSE_tripm = "SE for Mean Number of Angler Trips",
      mtot_trp = "Total Number of Angler Trips",
      mSE_trpt = "SE for Total Number of Angler Trips",
      mrpuem = "Mean HarvestORCatch Per Unit Effort"
    )
    statmon <<- statmon
    
    statall <- statall1 %>% 
      mutate(system = waterbody_interest) %>% 
      select(system, Em, SE_Em, tot_E, SE_Et, hc_m, SE_hcm, tot_hc, SE_hct, numtrpm, SE_tripm, tot_trp,
             SE_tript, rpuem)
    # Set labels for variables
    colnames(statall) <- c(
      system = "Waterbody",
      #year = "Year",
      Em = "Mean Fishing Effort",
      SE_Em = "SE for Mean Fishing Effort",
      tot_E = "Total Fishing Effort",
      SE_Et = "SE for Total Fishing Effort",
      hc_m = "Mean HarvestORCatch",
      SE_hcm = "SE for Mean HarvestORCatch",
      tot_hc = "Total HarvestORCatch",
      SE_hct = "SE for Total HarvestORCatch",
      numtrpm = "Mean Number of Angler Trips",
      SE_tripm = "SE for Mean Number of Angler Trips",
      tot_trp = "Total Number of Angler Trips",
      SE_tript = "SE for Total Number of Angler Trips",
      rpuem = "Mean HarvestORCatch Per Unit Effort"
    )
    statall <<- statall
  }
  

# Option 2 --------------------------------------------------------------------------------------------------------
  if(option == 2) {
    ## This block produces the following statistics:
    ## 1. by Month DOW TOD
    ## 2. by Month DOW
    ## 3. by Month
    ## 4. Overall

    
    stattod1 <- left_join(stratum_sum, n_periods) %>% 
      group_by(month, dow) %>% 
      mutate(
        # Calculate estimates of population totals
        tot_Es = n_periods * E,
        tot_hcs = n_periods * h_c1,
        tot_trps = n_periods * numtrip,
        # Calculate sampling fraction
        f_1h = n / n_periods,
        fpc1 = 1 - f_1h,
        f1_inv = 1 / f_1h,
        # Calculate variance between sampling periods - means and totals. No total calculation for harvest/catch rates.
        # Variance for means.
        s_E1m = fpc1 * (var_E1 / n),
        s_hc1m = fpc1 * (var_harcat1 / n),
        s_trip1m = fpc1 * (var_numtrip1 / n),
        # Variance for totals
        s_E1t = n_periods^2 * s_E1m,
        s_hc1t = n_periods^2 * s_hc1m,
        s_trip1t = n_periods^2 * s_trip1m,
        # Calculate variance within sampling periods - for means
        s_E2m = (1 / (n * n_periods)) * var_E,
        s_hc2m = (1 / (n * n_periods)) * var_hc1,
        s_trip2m = (1 / (n * n_periods)) * var_numtrip,
        # Calculate variance within sampling periods - for totals
        s_E2 = f1_inv * var_E,
        s_hc2 = f1_inv * var_hc1,
        s_trip2 = f1_inv * var_numtrip,
        # Combine between and within variances to get two-stage variance estimates for each stratum - means and totals.
        # Variance for means
        s_Esm = s_E1m + s_E2m,
        s_hcsm = s_hc1m + s_hc2m,
        s_tripsm = s_trip1m + s_trip2m,
        # Variance for totals
        s_Est = s_E1t + s_E2,
        s_hcst = s_hc1t + s_hc2,
        s_tripst = s_trip1t + s_trip2,
        # Calculate standard errors - means and totals.
        # Standard Error for means
        SE_Esm = sqrt(s_Esm),
        SE_hcsm = sqrt(s_hcsm),
        SE_trpsm = sqrt(s_tripsm),
        # Standard Error for totals
        SE_Est = sqrt(s_Est),
        SE_hcst = sqrt(s_hcst),
        SE_trpst = sqrt(s_tripst)) %>% 
      ungroup()
    
    # Now, statistics for harvest/catch, effort, and number of angler trips by Month and DOW.
    cal2a <- stattod1 %>%
      group_by(month, dow) %>%
      summarise(M = sum(n_periods)) %>% 
      ungroup()
    
    fishery1 <- left_join(stattod1, cal2a) %>% 
      mutate(
        # Calculate sampling fraction
        mW = n_periods / M,
        mW2 = mW^2,
        mn_fsh1 = mn_fsh * mW,
        mn_hrs1 = mn_hrs * mW,
        Et1 = E * mW,
        s_Et1m = (s_Esm * mW2) / n,
        h_c2 = h_c1 * mW,
        s_hct1m = (s_hcsm * mW2) / n,
        numtrpt1 = numtrip * mW,
        s_trpt1m = (s_tripsm * mW2) / n)
    
    fishery2 <- fishery1 %>% 
      group_by(month, dow) %>% 
      reframe(dEm = sum(Et1),
              dhc_m = sum(h_c2),
              dnumtrpm = sum(numtrpt1),
              d_fsh1 = sum(mn_fsh1),
              d_hrs1 = sum(mn_hrs1),
              ds_Em = sum(s_Et1m),
              ds_hcm = sum(s_hct1m),
              ds_tripm = sum(s_trpt1m)) %>% 
      ungroup()
    
    statdow1 <- left_join(cal2a, fishery2) %>% 
      mutate(drpuem = d_fsh1 / d_hrs1,
             tot_N = M^2,
             # Calculate estimates of population totals
             dtot_E = M * dEm,
             dtot_hc = M * dhc_m,
             dtot_trp = M * dnumtrpm,
             # Variance for totals
             ds_Et = tot_N * ds_Em,
             ds_hct = tot_N * ds_hcm,
             ds_tript = tot_N * ds_tripm,
             # Calculate standard errors - means and totals
             # Standard Error for means
             dSE_Em = sqrt(ds_Em),
             dSE_hcm = sqrt(ds_hcm),
             dSE_trpm = sqrt(ds_tripm),
             # Standard Error for totals
             dSE_Et = sqrt(ds_Et),
             dSE_hct = sqrt(ds_hct),
             dSE_trpt = sqrt(ds_tript)) %>% 
      ungroup()
      
    
    
    # Now, statistics for harvest/catch, effort, and number of angler trips by Month.
    calm2a <- statdow1 %>%
      group_by(month) %>%
      summarise(NM = sum(M)) %>% 
      ungroup()
    
    fishery3 <- left_join(statdow1, calm2a) %>% 
      mutate(
        # Calculate sampling fraction
        mW = M / NM,
        mW2 = mW^2,
        mn_fsh1 = d_fsh1 * mW,
        mn_hrs1 = d_hrs1 * mW,
        Et1 = dEm * mW,
        s_Et1m = ds_Em * mW2,
        h_c2 = dhc_m * mW,
        s_hct1m = ds_hcm * mW2,
        numtrpt1 = dnumtrpm * mW,
        s_trpt1m = ds_tripm * mW2)
    
    fishery4 <- fishery3 %>% 
      group_by(month) %>% 
      summarize(mEm = sum(Et1, na.rm = TRUE),
             mhc_m = sum(h_c2, na.rm = TRUE),
             mnumtrpm = sum(numtrpt1, na.rm = TRUE),
             mn_fsh1 = sum(d_fsh1, na.rm = TRUE),
             mn_hrs1 = sum(d_hrs1, na.rm = TRUE),
             ms_Em = sum(s_Et1m, na.rm = TRUE),
             ms_hcm = sum(s_hct1m, na.rm = TRUE),
             ms_tripm = sum(s_trpt1m, na.rm = TRUE))
    
    statmon1 <- left_join(calm2a, fishery4) %>% 
      mutate(
        mrpuem = mn_fsh1 / mn_hrs1,
        tot_NM = NM^2,
        # Calculate estimates of population totals
        mtot_E = NM * mEm,
        mtot_hc = NM * mhc_m,
        mtot_trp = NM * mnumtrpm,
        # Variance for totals
        ms_Et = tot_NM * ms_Em,
        ms_hct = tot_NM * ms_hcm,
        ms_tript = tot_NM * ms_tripm,
        # Calculate standard errors - means and totals
        # Standard Error for means
        mSE_Em = sqrt(ms_Em),
        mSE_hcm = sqrt(ms_hcm),
        mSE_trpm = sqrt(ms_tripm),
        # Standard Error for totals
        mSE_Et = sqrt(ms_Et),
        mSE_hct = sqrt(ms_hct),
        mSE_trpt = sqrt(ms_tript)) %>% 
      ungroup()
    
    
    
    # Now, statistics for harvest/catch, effort, and number of angler trips adding all strata combined - Overall.
    calt <- statmon1 %>% 
      summarize(tot_N = sum(NM)) %>% 
      pull()
    
    # Merge calt2 and statmon1
    fishery5 <- statmon1 %>% 
      mutate(tot_N = calt,
             W = NM / tot_N,
             W2 = W^2,
             mn_fsh2 = mn_fsh1 * W,
             mn_hrs2 = mn_hrs1 * W,
             Et2 = mEm * W,
             s_Et2m = ms_Em * W2,
             h_c3 = mhc_m * W,
             s_hct2m = ms_hcm * W2,
             numtrpt2 = mnumtrpm * W,
             s_trpt2m = ms_tripm * W2)
    
    fishery6 <- fishery5 %>% 
      #group_by(year) %>% 
      summarize(Em = sum(Et2),
                s_Em = sum(s_Et2m),
                hc_m = sum(h_c3),
                s_hcm = sum(s_hct2m),
                numtrpm = sum(numtrpt2),
                s_tripm = sum(s_trpt2m),
                mn_fsh3 = sum(mn_fsh2),
                mn_hrs3 = sum(mn_hrs2))
    
    statall1 <- fishery6 %>% 
      mutate(tot_N = calt,
             rpuem = mn_fsh3/mn_hrs3,
             tot_N2 = tot_N^2,
             # Calculate estimates of population totals
             tot_E = tot_N * Em,
             tot_hc = tot_N * hc_m,
             tot_trp = tot_N * numtrpm,
             # Calculate variances for totals
             s_Et = tot_N^2 * s_Em,
             s_hct = tot_N^2 * s_hcm,
             s_tript = tot_N^2 * s_tripm,
             # Calculate standard errors
             SE_Em = sqrt(s_Em),
             SE_hcm = sqrt(s_hcm),
             SE_tripm = sqrt(s_tripm),
             SE_Et = sqrt(s_Et),
             SE_hct = sqrt(s_hct),
             SE_tript = sqrt(s_tript))
    
    
    stattod <- stattod1 %>% 
      mutate(system = waterbody_interest) %>% 
      select(system, month, dow, tod, E, SE_Esm, tot_Es, SE_Est, h_c1, SE_hcsm, tot_hcs, SE_hcst, numtrip, SE_trpsm,
             tot_trps, SE_trpst, rpue)
    # Set labels for variables
    colnames(stattod) <- c(
      system = "Waterbody",
      #year = "Year",
      month = "Month",
      dow = "Day of Week",
      tod = "Time of Day",
      E = "Mean Fishing Effort",
      SE_Em = "SE for Mean Fishing Effort",
      tot_E = "Total Fishing Effort",
      SE_Et = "SE for Total Fishing Effort",
      hc_m = "Mean HarvestORCatch",
      SE_hcm = "SE for Mean HarvestORCatch",
      tot_hc = "Total HarvestORCatch",
      SE_hct = "SE for Total HarvestORCatch",
      numtrpm = "Mean Number of Angler Trips",
      SE_tripm = "SE for Mean Number of Angler Trips",
      tot_trp = "Total Number of Angler Trips",
      SE_tript = "SE for Total Number of Angler Trips",
      rpuem = "Mean HarvestORCatch Per Unit Effort"
    )
    stattod <<- stattod
    
    ## Create summary tables
    statdow <- statdow1 %>% 
      mutate(system = waterbody_interest) %>% 
      select(system, month, dow, dEm, dSE_Em, dtot_E, dSE_Et, dhc_m, dSE_hcm, dtot_hc, dSE_hct, dnumtrpm,
             dSE_trpm, dtot_trp, dSE_trpt, drpuem)
    # Set labels for variables
    colnames(statdow) <- c(
      system = "Waterbody",
      #year = "Year",
      month = "Month",
      dow = "Day of Week",
      dEm = "Mean Fishing Effort",
      dSE_Em = "SE for Mean Fishing Effort",
      dtot_E = "Total Fishing Effort",
      dSE_Et = "SE for Total Fishing Effort",
      dhc_m = "Mean HarvestORCatch",
      dSE_hcm = "SE for Mean HarvestORCatch",
      dtot_hc = "Total HarvestORCatch",
      dSE_hct = "SE for Total HarvestORCatch",
      dnumtrpm = "Mean Number of Angler Trips",
      dSE_tripm = "SE for Mean Number of Angler Trips",
      dtot_trp = "Total Number of Angler Trips",
      dSE_tript = "SE for Total Number of Angler Trips",
      drpuem = "Mean HarvestORCatch Per Unit Effort"
    )
    statdow <<- statdow
    
    statmon <- statmon1 %>% 
      mutate(system = waterbody_interest) %>% 
      select(system, month, mEm, mSE_Em, mtot_E, mSE_Et, mhc_m, mSE_hcm, mtot_hc, mSE_hct, mnumtrpm, mSE_trpm, 
             mtot_trp, mSE_trpt, mrpuem)
    # Set labels for variables
    colnames(statmon) <- c(
      system = "Waterbody",
      #year = "Year",
      month = "Month",
      mEm = "Mean Fishing Effort",
      mSE_Em = "SE for Mean Fishing Effort",
      mtot_E = "Total Fishing Effort",
      mSE_Et = "SE for Total Fishing Effort",
      mhc_m = "Mean HarvestORCatch",
      mSE_hcm = "SE for Mean HarvestORCatch",
      mtot_hc = "Total HarvestORCatch",
      mSE_hct = "SE for Total HarvestORCatch",
      mnumtrpm = "Mean Number of Angler Trips",
      mSE_tripm = "SE for Mean Number of Angler Trips",
      mtot_trp = "Total Number of Angler Trips",
      mSE_trpt = "SE for Total Number of Angler Trips",
      mrpuem = "Mean HarvestORCatch Per Unit Effort"
    )
    statmon <<- statmon
    
    statall <- statall1 %>% 
      mutate(system = waterbody_interest) %>% 
      select(system, Em, SE_Em, tot_E, SE_Et, hc_m, SE_hcm, tot_hc, SE_hct, numtrpm, SE_tripm, tot_trp, 
             SE_tript, rpuem)
    # Set labels for variables
    colnames(statall) <- c(
      system = "Waterbody",
      #year = "Year",
      Em = "Mean Fishing Effort",
      SE_Em = "SE for Mean Fishing Effort",
      tot_E = "Total Fishing Effort",
      SE_Et = "SE for Total Fishing Effort",
      hc_m = "Mean HarvestORCatch",
      SE_hcm = "SE for Mean HarvestORCatch",
      tot_hc = "Total HarvestORCatch",
      SE_hct = "SE for Total HarvestORCatch",
      numtrpm = "Mean Number of Angler Trips",
      SE_tripm = "SE for Mean Number of Angler Trips",
      tot_trp = "Total Number of Angler Trips",
      SE_tript = "SE for Total Number of Angler Trips",
      rpuem = "Mean HarvestORCatch Per Unit Effort"
    )
    statall <<- statall
  }
  
# Option 3 --------------------------------------------------------------------------------------------------------
  if(option == 3) {
    ## This block produces the following statistics:
    ## 1. by Site Month DOW
    ## 2. by Site Month
    ## 3. by Site
    ## 4. Overall
    
    # Finally, statistics for harvest/catch, effort, and number of angler trips by Site, Month, and DOW
    statdow1 <- left_join(stratum_sum, n_periods) %>% 
      group_by(month, dow, site) %>% 
      reframe(
        dEm = E,
        dhc_m = h_c1,
        dnumtrpm = numtrip,
        drpuem = rpue,
        M = n_periods,
        d_fsh1 = mn_fsh,
        d_hrs1 = mn_hrs,
        # Calculate estimates of population totals
        dtot_E = M * E,
        dtot_hc = M * h_c1,
        dtot_trp = M * numtrip,
        # Calculate sampling fraction
        f_1h = n / M,
        fpc1 = 1 - f_1h,
        f1_inv = 1 / f_1h,
        # Calculate variance between sampling periods - means and totals
        s_E1m = fpc1 * (var_E1 / n),
        s_hc1m = fpc1 * (var_harcat1 / n),
        s_trip1m = fpc1 * (var_numtrip1 / n),
        # Calculate variance for totals
        s_E1t = M^2 * s_E1m,
        s_hc1t = M^2 * s_hc1m,
        s_trip1t = M^2 * s_trip1m,
        # Calculate variance within sampling periods - for means
        s_E2m = (1 / (n * M)) * var_E,
        s_hc2m = (1 / (n * M)) * var_hc1,
        s_trip2m = (1 / (n * M)) * var_numtrip,
        # Calculate variance within sampling periods - for totals
        s_E2 = f1_inv * var_E,
        s_hc2 = f1_inv * var_hc1,
        s_trip2 = f1_inv * var_numtrip,
        # Combine between and within variances to get two-stage variance estimates for each stratum - means and totals
        ds_Em = s_E1m + s_E2m,
        ds_hcm = s_hc1m + s_hc2m,
        ds_tripm = s_trip1m + s_trip2m,
        ds_Et = s_E1t + s_E2,
        ds_hct = s_hc1t + s_hc2,
        ds_tript = s_trip1t + s_trip2,
        # Calculate standard errors - means and totals
        dSE_Em = sqrt(ds_Em),
        dSE_hcm = sqrt(ds_hcm),
        dSE_trpm = sqrt(ds_tripm),
        dSE_Et = sqrt(ds_Et),
        dSE_hct = sqrt(ds_hct),
        dSE_trpt = sqrt(ds_tript)) %>% 
      ungroup()
    
    
    ## Now, statistics for harvest/catch, effort, and number of angler trips by Month and Site.
    calm2a <- statdow1 %>% group_by(month, site) %>% 
      summarize(NM = sum(M)) %>% 
      ungroup()
    
    # Calculate sampling fraction
    fishery3 <- left_join(statdow1, calm2a) %>% 
      mutate(mW = M / NM,
             mW2 = mW^2,
             mn_fsh1 = d_fsh1 * mW,
             mn_hrs1 = d_hrs1 * mW,
             Et1 = dEm * mW,
             s_Et1m = ds_Em * mW2,
             h_c2 = dhc_m * mW,
             s_hct1m = ds_hcm * mW2,
             numtrpt1 = dnumtrpm * mW,
             s_trpt1m = ds_tripm * mW2)
    
    fishery4 <- fishery3 %>% 
      group_by(month, site) %>% 
      summarize(mEm = sum(Et1, na.rm = TRUE),
                mhc_m = sum(h_c2, na.rm = TRUE),
                mnumtrpm = sum(numtrpt1, na.rm = TRUE),
                mn_fsh1 = sum(d_fsh1, na.rm = TRUE),
                mn_hrs1 = sum(d_hrs1, na.rm = TRUE),
                ms_Em = sum(s_Et1m, na.rm = TRUE),
                ms_hcm = sum(s_hct1m, na.rm = TRUE),
                ms_tripm = sum(s_trpt1m, na.rm = TRUE)) %>% 
      ungroup()
    
    statmon1 <- left_join(calm2a, fishery4) %>% 
      mutate(mrpuem = mn_fsh1 / mn_hrs1,
             tot_NM = NM^2,
             # Calculate estimates of population totals
             mtot_E = NM * mEm,
             mtot_hc = NM * mhc_m,
             mtot_trp = NM * mnumtrpm,
             # Calculate variance for totals
             ms_Et = tot_NM * ms_Em,
             ms_hct = tot_NM * ms_hcm,
             ms_tript = tot_NM * ms_tripm,
             # Calculate standard errors - means and totals
             mSE_Em = sqrt(ms_Em),
             mSE_hcm = sqrt(ms_hcm),
             mSE_trpm = sqrt(ms_tripm),
             mSE_Et = sqrt(ms_Et),
             mSE_hct = sqrt(ms_hct),
             mSE_trpt = sqrt(ms_tript))
  
    
    ## Now, statistics for harvest/catch, effort, and number of angler trips by Site.
    cals2a <- statmon1 %>% 
      group_by(site) %>% 
      summarize(SM = sum(NM))
    
    fishery5 <- left_join(cals2a, statmon1) %>% 
      mutate(
        # Calculate sampling fraction
        sW = NM / SM,
        sW2 = sW^2,
        mn_fsh2 = mn_fsh1 * sW,
        mn_hrs2 = mn_hrs1 * sW,
        Et2 = mEm * sW,
        s_Et2m = ms_Em * sW2,
        h_c3 = mhc_m * sW,
        s_hct2m = ms_hcm * sW2,
        numtrpt2 = mnumtrpm * sW,
        s_trpt2m = ms_tripm * sW2)
    
    fishery6 <- fishery5 %>% 
      group_by(site) %>% 
      summarize(sEm = sum(Et2, na.rm = TRUE),
                ss_Em = sum(s_Et2m, na.rm = TRUE),
                shc_m = sum(h_c3, na.rm = TRUE),
                ss_hcm = sum(s_hct2m, na.rm = TRUE),
                snumtrpm = sum(numtrpt2, na.rm = TRUE),
                ss_tripm = sum(s_trpt2m, na.rm = TRUE),
                mn_fsh3 = sum(mn_fsh2, na.rm = TRUE),
                mn_hrs3 = sum(mn_hrs2, na.rm = TRUE))
    
    statsit1 <- left_join(cals2a, fishery6) %>% 
      mutate(
        srpuem = mn_fsh3 / mn_hrs3,
        tot_SM = SM^2,
        # Calculate estimates of population totals
        stot_E = SM * sEm,
        stot_hc = SM * shc_m,
        stot_trp = SM * snumtrpm,
        # Variance for totals
        ss_Et = tot_SM * ss_Em,
        ss_hct = tot_SM * ss_hcm,
        ss_tript = tot_SM * ss_tripm,
        # Calculate standard errors - means and totals
        # Standard Error for means
        sSE_Em = sqrt(ss_Em),
        sSE_hcm = sqrt(ss_hcm),
        sSE_trpm = sqrt(ss_tripm),
        # Standard Error for totals
        sSE_Et = sqrt(ss_Et),
        sSE_hct = sqrt(ss_hct),
        sSE_trpt = sqrt(ss_tript)) %>% 
      ungroup()
    
    
    ## Now, statistics for harvest/catch, effort, and number of angler trips adding all strata combined - Overall.
    calt <- statmon1 %>% 
      #group_by(year) %>% 
      summarize(SM = sum(NM))
    
    # Merge calt2 and statmon1
    fishery5 <- left_join(calt, statmon1) %>% 
      mutate(W = NM / SM,
             W2 = W^2,
             mn_fsh2 = mn_fsh1 * W,
             mn_hrs2 = mn_hrs1 * W,
             Et2 = mEm * W,
             s_Et2m = ms_Em * W2,
             h_c3 = mhc_m * W,
             s_hct2m = ms_hcm * W2,
             numtrpt2 = mnumtrpm * W,
             s_trpt2m = ms_tripm * W2)
    
    fishery6 <- fishery5 %>% 
      group_by(site) %>% 
      summarize(sEm = sum(Et2),
                ss_Em = sum(s_Et2m),
                shc_m = sum(h_c3),
                ss_hcm = sum(s_hct2m),
                snumtrpm = sum(numtrpt2),
                ss_tripm = sum(s_trpt2m),
                mn_fsh3 = sum(mn_fsh2),
                mn_hrs3 = sum(mn_hrs2))
    
    statsit1 <- left_join(cals2a, fishery6) %>% 
      group_by(site) %>% 
      mutate(srpuem = mn_fsh3 / mn_hrs3,
                tot_SM = SM^2,
                # Calculate estimates of populaiton totals
                stot_E = SM * sEm,
                stot_hc = SM * shc_m,
                stot_trp = SM * snumtrpm,
                # Variance for totals
                ss_Et = tot_SM * ss_Em,
                ss_hct = tot_SM * ss_hcm,
                ss_tript = tot_SM * ss_tripm,
                # Calculate standard errors - means and totals
                # Standard error for means
                sSE_Em = sqrt(ss_Em),
                sSE_hcm = sqrt(ss_hcm),
                sSE_trpm = sqrt(ss_tripm),
                # Standard error for totals
                sSE_Et = sqrt(ss_Et),
                sSE_hct = sqrt(ss_hct),
                sSE_trpt = sqrt(ss_tript)) %>% 
      ungroup()
      
    
    # Now, statistics for harvest/catch, effort, and number of angler trips adding all strata combined - Overall.
    calt2 <- statsit1 %>% 
      summarize(tot_N = sum(SM)) %>% 
      pull()
    
    # Merge calt2 and statsit1
    fishery7 <- statsit1 %>% 
      mutate(tot_N = calt2,
             W = SM / tot_N,
             W2 = W^2,
             mn_fsh4 = mn_fsh3 * W,
             mn_hrs4 = mn_hrs3 * W,
             Et2 = sEm * W,
             s_Et2m = ss_Em * W2,
             h_c3 = shc_m * W,
             s_hct2m = ss_hcm * W2,
             numtrpt2 = snumtrpm * W,
             s_trpt2m = ss_tripm * W2)
    
    fishery8 <- fishery7 %>% 
      #group_by(year) %>% 
      summarize(Em = sum(Et2),
                s_Em = sum(s_Et2m),
                hc_m = sum(h_c3),
                s_hcm = sum(s_hct2m),
                numtrpm = sum(numtrpt2),
                s_tripm = sum(s_trpt2m),
                mn_fsh5 = sum(mn_fsh4),
                mn_hrs5 = sum(mn_hrs4))
    
    statall1 <- fishery8 %>% 
      mutate(tot_N = calt2,
             rpuem = mn_fsh5/mn_hrs5,
             tot_N2 = tot_N^2,
             # Calculate estimates of population totals
             tot_E = tot_N * Em,
             tot_hc = tot_N * hc_m,
             tot_trp = tot_N * numtrpm,
             # Calculate variances for totals
             s_Et = tot_N^2 * s_Em,
             s_hct = tot_N^2 * s_hcm,
             s_tript = tot_N^2 * s_tripm,
             # Calculate standard errors
             SE_Em = sqrt(s_Em),
             SE_hcm = sqrt(s_hcm),
             SE_tripm = sqrt(s_tripm),
             SE_Et = sqrt(s_Et),
             SE_hct = sqrt(s_hct),
             SE_tript = sqrt(s_tript))
    
    
    
    statdow <- statdow1 %>% 
      mutate(system = waterbody_interest) %>% 
      select(system, site, month, dow, dEm, dSE_Em, dtot_E, dSE_Et, dhc_m, dSE_hcm, dtot_hc, dSE_hct, dnumtrpm, 
             dSE_trpm, dtot_trp, dSE_trpt, drpuem)
    # Set labels for variables
    colnames(statdow) <- c(
      system = "Waterbody",
      #year = "Year",
      site = "Site",
      month = "Month",
      dow = "Day of Week",
      dEm = "Mean Fishing Effort",
      dSE_Em = "SE for Mean Fishing Effort",
      dtot_E = "Total Fishing Effort",
      dSE_Et = "SE for Total Fishing Effort",
      dhc_m = "Mean HarvestORCatch",
      dSE_hcm = "SE for Mean HarvestORCatch",
      dtot_hc = "Total HarvestORCatch",
      dSE_hct = "SE for Total HarvestORCatch",
      dnumtrpm = "Mean Number of Angler Trips",
      dSE_tripm = "SE for Mean Number of Angler Trips",
      dtot_trp = "Total Number of Angler Trips",
      dSE_tript = "SE for Total Number of Angler Trips",
      drpuem = "Mean HarvestORCatch Per Unit Effort"
    )
    statdow <<- statdow
    
    ## Create summary tables
    statmon <- statmon1 %>% 
      mutate(system = waterbody_interest) %>% 
      select(system, site, month, mEm, mSE_Em, mtot_E, mSE_Et, mhc_m, mSE_hcm, mtot_hc, mSE_hct, mnumtrpm,
             mSE_trpm, mtot_trp, mSE_trpt, mrpuem)
    # Set labels for variables
    colnames(statmon) <- c(
      system = "Waterbody",
      #year = "Year",
      site = "Site",
      month = "Month",
      mEm = "Mean Fishing Effort",
      mSE_Em = "SE for Mean Fishing Effort",
      mtot_E = "Total Fishing Effort",
      mSE_Et = "SE for Total Fishing Effort",
      mhc_m = "Mean HarvestORCatch",
      mSE_hcm = "SE for Mean HarvestORCatch",
      mtot_hc = "Total HarvestORCatch",
      mSE_hct = "SE for Total HarvestORCatch",
      mnumtrpm = "Mean Number of Angler Trips",
      mSE_tripm = "SE for Mean Number of Angler Trips",
      mtot_trp = "Total Number of Angler Trips",
      mSE_trpt = "SE for Total Number of Angler Trips",
      mrpuem = "Mean HarvestORCatch Per Unit Effort"
    )
    statmon <<- statmon
    
    statsite <- statsit1 %>% 
      mutate(system = waterbody_interest) %>% 
      select(system, site, sEm, sSE_Em, stot_E, sSE_Et, shc_m, sSE_hcm, stot_hc, sSE_hct, snumtrpm, sSE_trpm,
             stot_trp, sSE_trpt, srpuem)
    # Set labels for variables
    colnames(statsite) <- c(
      system = "Waterbody",
      #year = "Year",
      site = "Site",
      sEm = "Mean Fishing Effort",
      sSE_Em = "SE for Mean Fishing Effort",
      stot_E = "Total Fishing Effort",
      sSE_Et = "SE for Total Fishing Effort",
      shc_m = "Mean HarvestORCatch",
      sSE_hcm = "SE for Mean HarvestORCatch",
      stot_hc = "Total HarvestORCatch",
      sSE_hct = "SE for Total HarvestORCatch",
      snumtrpm = "Mean Number of Angler Trips",
      sSE_trpm = "SE for Mean Number of Angler Trips",
      stot_trp = "Total Number of Angler Trips",
      sSE_tript = "SE for Total Number of Angler Trips",
      srpuem = "Mean HarvestORCatch Per Unit Effort"
    )
    statsite <<- statsite
    
    statall <- statall1 %>%
      mutate(system = waterbody_interest) %>% 
      select(system, Em, SE_Em, tot_E, SE_Et, hc_m, SE_hcm, tot_hc, SE_hct, numtrpm, SE_tripm, tot_trp,
             SE_tript, rpuem)
    # Set labels for variables
    colnames(statall) <- c(
      system = "Waterbody",
      #year = "Year",
      Em = "Mean Fishing Effort",
      SE_Em = "SE for Mean Fishing Effort",
      tot_E = "Total Fishing Effort",
      SE_Et = "SE for Total Fishing Effort",
      hc_m = "Mean HarvestORCatch",
      SE_hcm = "SE for Mean HarvestORCatch",
      tot_hc = "Total HarvestORCatch",
      SE_hct = "SE for Total HarvestORCatch",
      numtrpm = "Mean Number of Angler Trips",
      SE_tripm = "SE for Mean Number of Angler Trips",
      tot_trp = "Total Number of Angler Trips",
      SE_tript = "SE for Total Number of Angler Trips",
      rpuem = "Mean HarvestORCatch Per Unit Effort"
    )
    statall <<- statall
  }

# Option 4 --------------------------------------------------------------------------------------------------------
  if(option == 4){
    ## This block produces the following statistics:
    ## 1. by Site Month DOW TOD
    ## 2. by Site Month DOW
    ## 3. by Site Month
    ## 5. Site
    ## 4. Overall
    
    # Finally, statistics for harvest/catch, effort, and number of angler trips by Site, Month, DOW, and TOD
    stattod1 <- left_join(stratum_sum, n_periods) %>% 
      group_by(month, dow, site) %>% 
      mutate(
        # Calculate estimates of population totals
        tot_Es = n_periods * E,
        tot_hcs = n_periods * h_c1,
        tot_trps = n_periods * numtrip,
        # Calculate sampling fraction
        f_1h = n / n_periods,
        fpc1 = 1 - f_1h,
        f1_inv = 1 / f_1h,
        # Calculate variance between sampling periods - means and totals. No total calculation for harvest/catch rates.
        # Variance for means.
        s_E1m = fpc1 * (var_E1 / n),
        s_hc1m = fpc1 * (var_harcat1 / n),
        s_trip1m = fpc1 * (var_numtrip1 / n),
        # Variance for totals
        s_E1t = n_periods^2*s_E1m,
        s_hc1t = n_periods^2*s_hc1m,
        s_trip1t = n_periods^2*s_trip1m,
        # Calculate variance within sampling periods - for means
        s_E2m = (1 / (n * n_periods)) * var_E,
        s_hc2m = (1 / (n * n_periods)) * var_hc1,
        s_trip2m = (1 / (n * n_periods)) * var_numtrip,
        # Calculate variance within sampling periods - for totals
        s_E2 = f1_inv * var_E,
        s_hc2 = f1_inv * var_hc1,
        s_trip2 = f1_inv * var_numtrip,
        # Combine between and within variances to get two-stage variance estimates for each stratum - means and totals.
        # Variance for means
        s_Esm = s_E1m + s_E2m,
        s_hcsm = s_hc1m + s_hc2m,
        s_tripsm = s_trip1m + s_trip2m,
        # Variance for totals
        s_Est = s_E1t + s_E2,
        s_hcst = s_hc1t + s_hc2,
        s_tripst = s_trip1t + s_trip2,
        # Calculate standard errors - means and totals.
        # Standard Error for means
        SE_Esm = sqrt(s_Esm),
        SE_hcsm = sqrt(s_hcsm),
        SE_trpsm = sqrt(s_tripsm),
        # Standard Error for totals
        SE_Est = sqrt(s_Est),
        SE_hcst = sqrt(s_hcst),
        SE_trpst = sqrt(s_tripst)) %>% 
      ungroup()
    
    # Now, statistics for harvest/catch, effort, and number of angler trips by Month, DOW, and Site.
    cal2a <- stattod1 %>%
      group_by(month, dow, site) %>%
      summarise(M = sum(n_periods)) %>% 
      ungroup()
    
    fishery1 <- left_join(stattod1, cal2a) %>% 
      mutate(
        # Calculate sampling fraction
        mW = n_periods / M,
        mW2 = mW^2,
        mn_fsh1 = mn_fsh * mW,
        mn_hrs1 = mn_hrs * mW,
        Et1 = E * mW,
        s_Et1m = (s_Esm * mW2) / n,
        h_c2 = h_c1 * mW,
        s_hct1m = (s_hcsm * mW2) / n,
        numtrpt1 = numtrip * mW,
        s_trpt1m = (s_tripsm * mW2) / n)
    
    fishery2 <- fishery1 %>% 
      group_by(month, dow, site) %>% 
      reframe(dEm = sum(Et1),
              dhc_m = sum(h_c2),
              dnumtrpm = sum(numtrpt1),
              d_fsh1 = sum(mn_fsh1),
              d_hrs1 = sum(mn_hrs1),
              ds_Em = sum(s_Et1m),
              ds_hcm = sum(s_hct1m),
              ds_tripm = sum(s_trpt1m)) %>% 
      ungroup()
    
    statdow1 <- left_join(cal2a, fishery2) %>% 
      mutate(drpuem = d_fsh1 / d_hrs1,
             tot_N = M^2,
             # Calculate estimates of population totals
             dtot_E = M * dEm,
             dtot_hc = M * dhc_m,
             dtot_trp = M * dnumtrpm,
             # Variance for totals
             ds_Et = tot_N * ds_Em,
             ds_hct = tot_N * ds_hcm,
             ds_tript = tot_N * ds_tripm,
             # Calculate standard errors - means and totals
             # Standard Error for means
             dSE_Em = sqrt(ds_Em),
             dSE_hcm = sqrt(ds_hcm),
             dSE_trpm = sqrt(ds_tripm),
             # Standard Error for totals
             dSE_Et = sqrt(ds_Et),
             dSE_hct = sqrt(ds_hct),
             dSE_trpt = sqrt(ds_tript)) %>% 
      ungroup()
    
    
    # Now, statistics for harvest/catch, effort, and number of angler trips by Month and Site.
    calm2a <- statdow1 %>%
      group_by(month, site) %>%
      summarise(NM = sum(M)) %>% 
      ungroup()
    
    fishery3 <- left_join(statdow1, calm2a) %>% 
      mutate(
        # Calculate sampling fraction
        mW = M / NM,
        mW2 = mW^2,
        mn_fsh1 = d_fsh1 * mW,
        mn_hrs1 = d_hrs1 * mW,
        Et1 = dEm * mW,
        s_Et1m = ds_Em * mW2,
        h_c2 = dhc_m * mW,
        s_hct1m = ds_hcm * mW2,
        numtrpt1 = dnumtrpm * mW,
        s_trpt1m = ds_tripm * mW2)
    
    fishery4 <- fishery3 %>% 
      group_by(month, site) %>% 
      summarize(mEm = sum(Et1, na.rm = TRUE),
                mhc_m = sum(h_c2, na.rm = TRUE),
                mnumtrpm = sum(numtrpt1, na.rm = TRUE),
                mn_fsh1 = sum(d_fsh1, na.rm = TRUE),
                mn_hrs1 = sum(d_hrs1, na.rm = TRUE),
                ms_Em = sum(s_Et1m, na.rm = TRUE),
                ms_hcm = sum(s_hct1m, na.rm = TRUE),
                ms_tripm = sum(s_trpt1m, na.rm = TRUE))
    
    statmon1 <- left_join(calm2a, fishery4) %>% 
      mutate(
        mrpuem = mn_fsh1 / mn_hrs1,
        tot_NM = NM^2,
        # Calculate estimates of population totals
        mtot_E = NM * mEm,
        mtot_hc = NM * mhc_m,
        mtot_trp = NM * mnumtrpm,
        # Variance for totals
        ms_Et = tot_NM * ms_Em,
        ms_hct = tot_NM * ms_hcm,
        ms_tript = tot_NM * ms_tripm,
        # Calculate standard errors - means and totals
        # Standard Error for means
        mSE_Em = sqrt(ms_Em),
        mSE_hcm = sqrt(ms_hcm),
        mSE_trpm = sqrt(ms_tripm),
        # Standard Error for totals
        mSE_Et = sqrt(ms_Et),
        mSE_hct = sqrt(ms_hct),
        mSE_trpt = sqrt(ms_tript)) %>% 
      ungroup()
    
    
    # Now, statistics for harvest/catch, effort, and number of angler trips by Site.
    cals2a <- statmon1 %>% 
      group_by(site) %>% 
      summarize(SM = sum(NM))
    
    fishery5 <- left_join(cals2a, statmon1) %>% 
      mutate(
        # Calculate sampling fraction
        sW = NM / SM,
        sW2 = sW^2,
        mn_fsh2 = mn_fsh1 * sW,
        mn_hrs2 = mn_hrs1 * sW,
        Et2 = mEm * sW,
        s_Et2m = ms_Em * sW2,
        h_c3 = mhc_m * sW,
        s_hct2m = ms_hcm * sW2,
        numtrpt2 = mnumtrpm * sW,
        s_trpt2m = ms_tripm * sW2)
      
    fishery6 <- fishery5 %>% 
      group_by(site) %>% 
      summarize(sEm = sum(Et2, na.rm = TRUE),
                ss_Em = sum(s_Et2m, na.rm = TRUE),
                shc_m = sum(h_c3, na.rm = TRUE),
                ss_hcm = sum(s_hct2m, na.rm = TRUE),
                snumtrpm = sum(numtrpt2, na.rm = TRUE),
                ss_tripm = sum(s_trpt2m, na.rm = TRUE),
                mn_fsh3 = sum(mn_fsh2, na.rm = TRUE),
                mn_hrs3 = sum(mn_hrs2, na.rm = TRUE))
    
    statsit1 <- left_join(cals2a, fishery6) %>% 
      mutate(
        srpuem = mn_fsh3 / mn_hrs3,
        tot_SM = SM^2,
        # Calculate estimates of population totals
        stot_E = SM * sEm,
        stot_hc = SM * shc_m,
        stot_trp = SM * snumtrpm,
        # Variance for totals
        ss_Et = tot_SM * ss_Em,
        ss_hct = tot_SM * ss_hcm,
        ss_tript = tot_SM * ss_tripm,
        # Calculate standard errors - means and totals
        # Standard Error for means
        sSE_Em = sqrt(ss_Em),
        sSE_hcm = sqrt(ss_hcm),
        sSE_trpm = sqrt(ss_tripm),
        # Standard Error for totals
        sSE_Et = sqrt(ss_Et),
        sSE_hct = sqrt(ss_hct),
        sSE_trpt = sqrt(ss_tript)) %>% 
      ungroup()
      

    # Now, statistics for harvest/catch, effort, and number of angler trips adding all strata combined - Overall.
    calt2 <- statsit1 %>% 
      summarize(tot_N = sum(SM)) %>% 
      pull()
    
    fishery7 <- statsit1 %>% 
      mutate(tot_N = calt2,
             W = SM / tot_N,
             W2 = W^2,
             mn_fsh4 = mn_fsh3 * W,
             mn_hrs4 = mn_hrs3 * W,
             Et2 = sEm * W,
             s_Et2m = ss_Em * W2,
             h_c3 = shc_m * W,
             s_hct2m = ss_hcm * W2,
             numtrpt2 = snumtrpm * W,
             s_trpt2m = ss_tripm * W2)
    
    fishery8 <- fishery7 %>% 
      #group_by(year) %>% 
      summarize(Em = sum(Et2),
                s_Em = sum(s_Et2m),
                hc_m = sum(h_c3),
                s_hcm = sum(s_hct2m),
                numtrpm = sum(numtrpt2),
                s_tripm = sum(s_trpt2m),
                mn_fsh5 = sum(mn_fsh4),
                mn_hrs5 = sum(mn_hrs4))
    
    statall1 <- fishery8 %>% 
      mutate(tot_N = calt2,
             rpuem = mn_fsh5/mn_hrs5,
             tot_N2 = tot_N^2,
             # Calculate estimates of population totals
             tot_E = tot_N * Em,
             tot_hc = tot_N * hc_m,
             tot_trp = tot_N * numtrpm,
             # Calculate variances for totals
             s_Et = tot_N^2 * s_Em,
             s_hct = tot_N^2 * s_hcm,
             s_tript = tot_N^2 * s_tripm,
             # Calculate standard errors
             SE_Em = sqrt(s_Em),
             SE_hcm = sqrt(s_hcm),
             SE_tripm = sqrt(s_tripm),
             SE_Et = sqrt(s_Et),
             SE_hct = sqrt(s_hct),
             SE_tript = sqrt(s_tript))
    
    ## Create summary tables
    stattod <- stattod1 %>% 
      mutate(system = waterbody_interest) %>% 
      select(system, site, month, dow, tod, E, SE_Esm, tot_Es, SE_Est, h_c1, SE_hcsm, tot_hcs, SE_hcst,
             numtrip, SE_trpsm, tot_trps, SE_trpst, rpue)
    # Set labels for variables
    colnames(stattod) <- c(
      system = "Waterbody",
      #year = "Year",
      site = "Site",
      month = "Month",
      dow = "Day of Week",
      tod = "Time of Day",
      E = "Mean Fishing Effort",
      SE_Esm = "SE for Mean Fishing Effort",
      tot_Es = "Total Fishing Effort",
      SE_Est = "SE for Total Fishing Effort",
      h_c1 = "Mean HarvestORCatch",
      SE_hcsm = "SE for Mean HarvestORCatch",
      tot_hcs = "Total HarvestORCatch",
      SE_hcst = "SE for Total HarvestORCatch",
      numtrip = "Mean Number of Angler Trips",
      SE_trpsm = "SE for Mean Number of Angler Trips",
      tot_trps = "Total Number of Angler Trips",
      SE_trpst = "SE for Total Number of Angler Trips",
      rpue = "Mean HarvestORCatch Per Unit Effort"
    )
    stattod <<- stattod

    statdow <- statdow1 %>% 
      mutate(system = waterbody_interest) %>% 
      select(system, site, month, dow, dEm, dSE_Em, dtot_E, dSE_Et, dhc_m, dSE_hcm, dtot_hc, dSE_hct, 
             dnumtrpm, dSE_trpm, dtot_trp, dSE_trpt, drpuem)
    # Set labels for variables
    colnames(statdow) <- c(
      system = "Waterbody",
      #year = "Year",
      site = "Site",
      month = "Month",
      dow = "Day of Week",
      dEm = "Mean Fishing Effort",
      dSE_Em = "SE for Mean Fishing Effort",
      dtot_E = "Total Fishing Effort",
      dSE_Et = "SE for Total Fishing Effort",
      dhc_m = "Mean HarvestORCatch",
      dSE_hcm = "SE for Mean HarvestORCatch",
      dtot_hc = "Total HarvestORCatch",
      dSE_hct = "SE for Total HarvestORCatch",
      dnumtrpm = "Mean Number of Angler Trips",
      dSE_tripm = "SE for Mean Number of Angler Trips",
      dtot_trp = "Total Number of Angler Trips",
      dSE_tript = "SE for Total Number of Angler Trips",
      drpuem = "Mean HarvestORCatch Per Unit Effort"
    )
    statdow <<- statdow

    statmon <- statmon1 %>% 
      mutate(system = waterbody_interest) %>% 
      select(system, site, month, mEm, mSE_Em, mtot_E, mSE_Et, mhc_m, mSE_hcm, mtot_hc, mSE_hct, 
             mnumtrpm, mSE_trpm, mtot_trp, mSE_trpt, mrpuem)
    # Set labels for variables
    colnames(statmon) <- c(
      system = "Waterbody",
      #year = "Year",
      site = "Site",
      month = "Month",
      mEm = "Mean Fishing Effort",
      mSE_Em = "SE for Mean Fishing Effort",
      mtot_E = "Total Fishing Effort",
      mSE_Et = "SE for Total Fishing Effort",
      mhc_m = "Mean HarvestORCatch",
      mSE_hcm = "SE for Mean HarvestORCatch",
      mtot_hc = "Total HarvestORCatch",
      mSE_hct = "SE for Total HarvestORCatch",
      mnumtrpm = "Mean Number of Angler Trips",
      mSE_tripm = "SE for Mean Number of Angler Trips",
      mtot_trp = "Total Number of Angler Trips",
      mSE_trpt = "SE for Total Number of Angler Trips",
      mrpuem = "Mean HarvestORCatch Per Unit Effort"
    )
    statmon <<- statmon

    statsite <- statsit1 %>% 
      mutate(system = waterbody_interest) %>% 
      select(system, site, sEm, sSE_Em, stot_E, sSE_Et, shc_m, sSE_hcm, stot_hc, sSE_hct, snumtrpm, 
             sSE_trpm, stot_trp, sSE_trpt, srpuem)
    # Set labels for variables
    colnames(statsite) <- c(
      system = "Waterbody",
      #year = "Year",
      site = "Site",
      sEm = "Mean Fishing Effort",
      sSE_Em = "SE for Mean Fishing Effort",
      stot_E = "Total Fishing Effort",
      sSE_Et = "SE for Total Fishing Effort",
      shc_m = "Mean HarvestORCatch",
      sSE_hcm = "SE for Mean HarvestORCatch",
      stot_hc = "Total HarvestORCatch",
      sSE_hct = "SE for Total HarvestORCatch",
      snumtrpm = "Mean Number of Angler Trips",
      sSE_trpm = "SE for Mean Number of Angler Trips",
      stot_trp = "Total Number of Angler Trips",
      sSE_tript = "SE for Total Number of Angler Trips",
      srpuem = "Mean HarvestORCatch Per Unit Effort"
    )
    statsite <<- statsite

    statall <- statall1 %>% 
      mutate(system = waterbody_interest) %>% 
      select(system, Em, SE_Em, tot_E, SE_Et, hc_m, SE_hcm, tot_hc, SE_hct, numtrpm, SE_tripm, 
             tot_trp, SE_tript, rpuem)
    # Set labels for variables
    colnames(statall) <- c(
      system = "Waterbody",
      #year = "Year",
      Em = "Mean Fishing Effort",
      SE_Em = "SE for Mean Fishing Effort",
      tot_E = "Total Fishing Effort",
      SE_Et = "SE for Total Fishing Effort",
      hc_m = "Mean HarvestORCatch",
      SE_hcm = "SE for Mean HarvestORCatch",
      tot_hc = "Total HarvestORCatch",
      SE_hct = "SE for Total HarvestORCatch",
      numtrpm = "Mean Number of Angler Trips",
      SE_tripm = "SE for Mean Number of Angler Trips",
      tot_trp = "Total Number of Angler Trips",
      SE_tript = "SE for Total Number of Angler Trips",
      rpuem = "Mean HarvestORCatch Per Unit Effort"
    )
    statall <<- statall
  }
}

# END -------------------------------------------------------------------------------------------------------------

/*roving.sas

     Created: 4/01

     Created By: Heather Thomas

     Notes: Roving Surveys with Monthly, DOW, and TOD (if needed) stratification - Two-stage sampling

     Modifications: 10/01 - Modified to correct variance calculations for stratified levels.
                            Removed variance from output. Only standard error (SE) of the mean
                            is reported.

					4/02  - Modified to impute zeros in estimating monthly catch and harvest 
                            when interview and count data show that there are no people on the
							lake/stream during both counts and interviews for the entire sampling
 							period.


*/

  options formdlim='+';


   /******************************************
     This is the beginning of the first macro
   ******************************************/

   %macro roving1a(data, byvar, vara, var2a);


    title1 "Descriptive Statistics for &var2a";

   /******************************************
    This procedure sorts the data by the
    different levels of strata
   ******************************************/

    proc sort data=&data;
      by &byvar;
    run;

    /*******************************************************
     Statistics needed to calculate mean harvest/catch
     rate for a sampled period. Output gives mean time
     anglers spent fishing (this is obtained from &vara)
     and mean harvest/catch (&var2a) for a sampled period.
     The variance for the time anglers spent fishing is also
     calculated.
    *******************************************************/

    proc univariate data=&data noprint;
      var &vara &var2a;
      by &byvar;
      output out=rate mean=mn_hrs mn_fsh var=v_hrs;
    run;

    /*******************************************************
     This step calculates harvest/catch rate for a
     sampled period (rpue). Note that the mean harvest/catch
     must be in the numerator and mean time spent fishing
     in the denominator.
    *******************************************************/

    data rate;
      set rate;
      rpue=mn_fsh/mn_hrs;
    run;
    
    proc sort data=rate; by &byvar; run;
    proc sort data=&data; by &byvar; run;

    /*********************************************************
     Rate is merged into original data set, in
     order to calculate the numerator of the
     variance of harvest/catch rate.
     (harvest/catch - time spent fishing*harvest/catch rate)^2
    *********************************************************/

    data numerat;
      merge rate &data;
      by &byvar;
      num=(&var2a-(&vara*rpue))**2;
    run;

    /******************************************
     This step sums the numerator values from
     previous calculation.
    ******************************************/

     proc univariate data=numerat noprint;
      var num;
      by &byvar;
      output out=numerat2 n=m sum=sumnum;
    run;

    proc sort data=rate; by &byvar; run;
    proc sort data=numerat2; by &byvar; run;

    /***********************************************
     Merge together two above data sets to calculate
     harvest/catch rate variance (v_rpue):
     v_rpue=sumnum/(mean time spent fishing)^2*count
            angler parties*(count of angler parties
            - 1).
     Variance for the mean length of an angler-trip
     is also calculated (v_ebar).
    ***********************************************/

    data int_vw1;
      merge rate numerat2;
      by &byvar;
      v_rpue=sumnum/(mn_hrs**2*m*(m-1));
      v_ebar=v_hrs/m;
      keep &byvar rpue v_rpue mn_hrs mn_fsh v_ebar;
    run;

    proc sort data=int_vw1; by &byvar; run;
    
   %mend roving1a;

    /**********************************************
     This begins the second macro. This macro
     calculates statistics for count data set.
    **********************************************/

   %macro roving1b(data2, byvar, varb, var2b);

    proc sort data=&data2;
      by &byvar;
    run;

    /**********************************************
     Computes statistics needed to calculate mean
     and variance of fishing effort for a sampled
     period. Output gives mean and variance for #
     of anglers counted fishing (&varb).
    **********************************************/

    proc univariate data=&data2 noprint;
      var &varb;
      by &byvar;
      output out=avgcount n=n mean=mean var=variance;
    run;

    proc sort data=avgcount; by &byvar; run;
    proc sort data=&data2; by &byvar; run;

    /****************************************************
     This step calculates fishing effort for a sampled
     period (E): E=mean # of angler parties counted
     (in a sampled period)*length of the sampling period.
     The variance for fishing effort is also calculated
     (v_E) for each sampled period: v_E=(length of the
     sampling period)^2*variance of the # of counted
     angler parties in a sampled period.
    ****************************************************/

    data count1;
      merge avgcount &data2;
      by &byvar;
      if first.tod;
      E=mean*&var2b;
      v_E=(&var2b**2)*variance;
      keep &byvar E v_E;
    run;

   proc sort data=count1; by &byvar; run;

    /**********************************************
     This step calculates harvest/catch means and
     variances (mean=&var2a.b; variance=v&var2a)
     and the mean and variance for the number of
     angler-trips. Mean and variance for fishing
     effort is also merged into this data set.
     This gives statistics for each sampled period.
    **********************************************/

   data all1;
     merge int_vw1 count1;
     by &byvar;
     h_c1=E*rpue;
	 if h_c1 = . then h_c1=0;
     num_trip=E/mn_hrs;
	 if num_trip = . then num_trip=0;
     v_hc1=(v_rpue*(E**2))+(v_E*(rpue**2))-(v_E*v_rpue);
	 if v_hc1 = . then v_hc1=0;
     vnumtrip=num_trip**2*((v_ebar/(mn_hrs**2))+(v_E/E**2)-((v_ebar*v_E)/
              (mn_hrs**2*E**2)));
	 if vnumtrip = . then vnumtrip=0;
   run;
   
  %mend roving1b;
    /**********************************************
     This begins the third macro. This begins
     calculations for statistics for each stratum.
    **********************************************/

   %macro roving1c(byvar);

   proc sort data=all1;
      by &byvar;
    run;

   proc univariate data=all1 noprint;
      var E h_c1 num_trip rpue mn_hrs mn_fsh;
      by &byvar;
      output out=fish1 n=d mean=E h_c1 num_trip rpue mn_hrs mn_fsh;
   run;

   proc sort data=fish1; by &byvar; run;

   proc univariate data=all1 noprint;
      var v_E v_hc1 vnumtrip;
      by &byvar;
      output out=fish2 sum=v_E v_hc1 vnumtrip;
   run;

   proc sort data=fish2; by &byvar; run;

   proc univariate data=all1 noprint;
      var E h_c1 num_trip;
      by &byvar;
      output out=fish3 var=v_E1 vharcat1 vtrip1;
   run;

   proc sort data=fish3; by &byvar; run;

   data fish4;
     merge fish1 fish2 fish3;
     by &byvar;
   run;

   proc sort data=fish4;
     by &byvar;
   run; 
   
  %mend roving1c;
    /**********************************************
     This begins the fourth macro.
    **********************************************/

   %macro roving1d(byvar, opt);

   /*********************************************
    All this macro does is determine the number
    of possible sampling periods within each
    stratum (D).
   *********************************************/

   data calendar; 
     set ds.calendar;
	 	x=1;
   run;

   proc sort data=calendar; 
    by &byvar;
   run;

   /**********************************
    This gives the number of possible
    sampling days in each stratum.
   **********************************/
   proc univariate data=calendar noprint;
      var x;
      by &byvar;
      output out=cal2 n=N; 
    run; 

   proc sort data=cal2; 
    by &byvar;
   run;

   proc sort data=fish4;
     by &byvar; 
   run;

    /**************************************************
     This block produces the following statistics:
     1. by Month DOW
     2. by Month
     3. Overall
   **************************************************/ 
 
   %let opt1=1;
   %if &opt1=&opt %then %do;

   /**************************************************
     Finally, statistics for harvest/catch, effort,
     and number of angler trips by Month and DOW
   **************************************************/
 
   data statdow1;
     merge cal2 fish4;
     by &byvar; 

	 dEm=E;
	 dhc_m=h_c1;
	 dnumtrpm=num_trip;
	 M=N;
	 d_fsh1=mn_fsh; 
     d_hrs1=mn_hrs;
    /**********************************
     Calculate estimates of population
     totals.
    **********************************/
     dtot_E=M*dEm;
     dtot_hc=M*dhc_m;
     dtot_trp=M*dnumtrpm;
	 drpuem=dtot_hc/dtot_E;
    /**********************************
     Calculate sampling fraction.
    **********************************/
     f_1h=d/M;
     fpc1=1-f_1h;
     f1_inv=1/f_1h;
    /**********************************
     Calculate variance between
     sampling periods - means and
     totals. No total calculation for
     harvest/catch rates.
    **********************************/
    /*********************
     Variance for means.
    *********************/
     s_E1m=fpc1*(v_E1/d);
     s_hc1m=fpc1*(vharcat1/d);
     s_trip1m=fpc1*(vtrip1/d);
     
    /*********************
     Variance for totals.
    *********************/
     s_E1t=M**2*s_E1m;
     s_hc1t=M**2*s_hc1m;
     s_trip1t=M**2*s_trip1m;
    /***********************************
     Calculate variance within
     sampling periods - for means.
    ***********************************/
     s_E2m=(1/(d*M))*v_E;
     s_hc2m=(1/(d*M))*v_hc1;
     s_trip2m=(1/(d*M))*vnumtrip;
     
    /***********************************
     Calculate variance within
     sampling periods - for totals.
    ***********************************/
     s_E2=f1_inv*v_E;
     s_hc2=f1_inv*v_hc1;
     s_trip2=f1_inv*vnumtrip;
    
    /************************************
     Combine between and within variances
     to get two-stage variance estimates
     for each stratum - means and totals.
    ************************************/
    /*********************
     Variance for means.
    *********************/
     ds_Em=s_E1m+s_E2m;
     ds_hcm=s_hc1m+s_hc2m;
     ds_tripm=s_trip1m+s_trip2m;
     
    /*********************
     Variance for totals.
    *********************/
     ds_Et=s_E1t+s_E2;
     ds_hct=s_hc1t+s_hc2;
     ds_tript=s_trip1t+s_trip2;
    /************************************
     Calculate standard errors - means
     and totals.
    ************************************/
    /*********************
     Standard Error for
     means.
    *********************/
     dSE_Em=ds_Em**0.5;
     dSE_hcm=ds_hcm**0.5;
     dSE_trpm=ds_tripm**0.5;
     
    /*********************
     Standard Error for
     totals.
    *********************/
     dSE_Et=ds_Et**0.5;
     dSE_hct=ds_hct**0.5;
     dSE_trpt=ds_tript**0.5;

     label
     dEm='Mean Fishing Effort'
     ds_Em='Variance for Mean Fishing Effort'
     dSE_Em='SE for Mean Fishing Effort'
     dtot_E='Total Fishing Effort'
     ds_Et='Variance for Total Fishing Effort'
     dSE_Et='SE for Total Fishing Effort'
     dhc_m='Mean Harvest/Catch'
     ds_hcm='Variance for Mean Harvest/Catch'
     dSE_hcm='SE for Mean Harvest/Catch'
     dtot_hc='Total Harvest/Catch'
     ds_hct='Variance for Total Harvest/Catch'
     dSE_hct='SE for Total Harvest/Catch'
     dnumtrpm='Mean Number of Angler Trips'
     ds_tripm='Variance for Mean Number of Angler Trips'
     dSE_trpm='SE for Mean Number of Angler Trips'
     dtot_trp='Total Number of Angler Trips'
     ds_tript='Variance for Total Number of Angler Trips'
     dSE_trpt='SE for Total Number of Angler Trips'
     drpuem='Mean Harvest/Catch Per Unit Effort';
   run;
 
   /**************************************************
     Now, statistics for harvest/catch, effort,
     and number of angler trips by Month.
   **************************************************/
    proc sort data=statdow1; 
      by month;
    run;

    proc univariate data=statdow1 noprint;
      by month;
      var M;
      output out=calm2a sum=NM;
    run; 

    proc sort data=statdow1;
      by month; 
    run;

    proc sort data=calm2a; 
      by month;
    run;

    data fishery3;
      merge statdow1 calm2a;
      by month;
      /**********************************
       Calculate sampling fraction.
      **********************************/
      mW=M/NM;
      mW2=mW**2;

	  mn_fsh1=d_fsh1*mW;
	  mn_hrs1=d_hrs1*mW;

      Et1=dEm*mW;
      s_Et1m=ds_Em*mW2;
      h_c2=dhc_m*mW;
      s_hct1m=ds_hcm*mW2;
      numtrpt1=dnumtrpm*mW;
      s_trpt1m=ds_tripm*mW2;
    run;

    proc sort data=fishery3;
      by month;
    run;

    proc univariate data=fishery3 noprint;
      by month;
      var Et1 h_c2 numtrpt1 d_fsh1 d_hrs1 s_Et1m s_hct1m s_trpt1m;
      output out=fishery4 sum=mEm mhc_m mnumtrpm mn_fsh1 mn_hrs1
                 ms_Em ms_hcm ms_tripm;
    run; 

    proc sort data=calm2a;
      by month; 
    run;

    proc sort data=fishery4;
      by month;
    run;

    data statmon1;
     merge calm2a fishery4;
     by month;
     z=1;

     tot_NM=NM**2;

    /**********************************
     Calculate estimates of population
     totals.
    **********************************/

     mtot_E=NM*mEm;
     mtot_hc=NM*mhc_m;
     mtot_trp=NM*mnumtrpm;
	 mrpuem=mtot_hc/mtot_E;

    /*********************
     Variance for totals.
    *********************/

     ms_Et=tot_NM*ms_Em;
     ms_hct=tot_NM*ms_hcm;
     ms_tript=tot_NM*ms_tripm;

    /************************************
     Calculate standard errors - means
     and totals.
    ************************************/
    /*********************
     Standard Error for
     means.
    *********************/

     mSE_Em=ms_Em**0.5;
     mSE_hcm=ms_hcm**0.5;
     mSE_trpm=ms_tripm**0.5;
     

    /*********************
     Standard Error for
     totals.
    *********************/

     mSE_Et=ms_Et**0.5;
     mSE_hct=ms_hct**0.5;
     mSE_trpt=ms_tript**0.5;


     label
     mEm='Mean Fishing Effort'
     ms_Em='Variance for Mean Fishing Effort'
     mSE_Em='SE for Mean Fishing Effort'
     mtot_E='Total Fishing Effort'
     ms_Et='Variance for Total Fishing Effort'
     mSE_Et='SE for Total Fishing Effort'
     mhc_m='Mean Harvest/Catch'
     ms_hcm='Variance for Mean Harvest/Catch'
     mSE_hcm='SE for Mean Harvest/Catch'
     mtot_hc='Total Harvest/Catch'
     ms_hct='Variance for Total Harvest/Catch'
     mSE_hct='SE for Total Harvest/Catch'
     mnumtrpm='Mean Number of Angler Trips'
     ms_tripm='Variance for Mean Number of Angler Trips'
     mSE_trpm='SE for Mean Number of Angler Trips'
     mtot_trp='Total Number of Angler Trips'
     ms_tript='Variance for Total Number of Angler Trips'
     mSE_trpt='SE for Total Number of Angler Trips'
     mrpuem='Mean Harvest/Catch Per Unit Effort';
   run;

   /**************************************************
     Now, statistics for harvest/catch, effort,
     and number of angler trips adding all strata
     combined - Overall.
   **************************************************/
 
    proc univariate data=statmon1 noprint;
      var NM;
      output out=calt sum=tot_N;
    run;

	data calt2;
	  set calt;
	  z=1;
	run;

	proc sort data=calt2; 
	  by z;
	run;

	proc sort data=statmon1; 
	  by z;
	run;

    data fishery5;
      merge statmon1 calt2;
	  by z;
      /**********************************
       Calculate sampling fraction.
      **********************************/
      W=NM/tot_N;
      W2=W**2;

	  mn_fsh2=mn_fsh1*W;
	  mn_hrs2=mn_hrs1*W;

      Et2=mEm*W;
      s_Et2m=ms_Em*W2;
      h_c3=mhc_m*W;
      s_hct2m=ms_hcm*W2;
      numtrpt2=mnumtrpm*W;
      s_trpt2m=ms_tripm*W2;
    run;

    proc univariate data=fishery5 noprint;
      var Et2 s_Et2m h_c3 s_hct2m numtrpt2 s_trpt2m
          mn_fsh2 mn_hrs2;
      output out=fishery6 sum=Em s_Em hc_m s_hcm numtrpm
                 s_tripm mn_fsh3 mn_hrs3;
    run; 

	data fishery6;
	  set fishery6;
	  z=1;
	run;

	proc sort data=fishery6;
	  by z;
	run;

	proc sort data=calt2;
	  by z;
	run;

    data statall1;
     merge calt2 fishery6;
	 by z;

     tot_N2=tot_N**2;
    /**********************************
     Calculate estimates of population
     totals.
    **********************************/
     tot_E=tot_N*Em;
     tot_hc=tot_N*hc_m;
     tot_trp=tot_N*numtrpm;
	 rpuem=tot_hc/tot_E;
    /*********************
     Variance for totals.
    *********************/
     s_Et=tot_N2*s_Em;
     s_hct=tot_N2*s_hcm;
     s_tript=tot_N2*s_tripm;
    /************************************
     Calculate standard errors - means
     and totals.
    ************************************/
    /*********************
     Standard Error for
     means.
    *********************/
     SE_Em=s_Em**0.5;
     SE_hcm=s_hcm**0.5;
     SE_tripm=s_tripm**0.5;
     
    /*********************
     Standard Error for
     totals.
    *********************/
     SE_Et=s_Et**0.5;
     SE_hct=s_hct**0.5;
     SE_tript=s_tript**0.5;

     label
     Em='Mean Fishing Effort'
     s_Em='Variance for Mean Fishing Effort'
     SE_Em='SE for Mean Fishing Effort'
     tot_E='Total Fishing Effort'
     s_Et='Variance for Total Fishing Effort'
     SE_Et='SE for Total Fishing Effort'
     hc_m='Mean Harvest/Catch'
     s_hcm='Variance for Mean Harvest/Catch'
     SE_hcm='SE for Mean Harvest/Catch'
     tot_hc='Total Harvest/Catch'
     s_hct='Variance for Total Harvest/Catch'
     SE_hct='SE for Total Harvest/Catch'
     numtrpm='Mean Number of Angler Trips'
     s_tripm='Variance for Mean Number of Angler Trips'
     SE_tripm='SE for Mean Number of Angler Trips'
     tot_trp='Total Number of Angler Trips'
     s_tript='Variance for Total Number of Angler Trips'
     SE_tript='SE for Total Number of Angler Trips'
     rpuem='Mean Harvest/Catch Per Unit Effort';
   run;
   %end;

   /**************************************************
     This block produces the following statistics:
     1. by Month DOW TOD
     2. by Month DOW
     3. by Month
     4. Overall
   **************************************************/ 
 
   %let opt2=2;
   %if &opt2=&opt %then %do;

   /**************************************************
     Finally, statistics for harvest/catch, effort,
     and number of angler trips by Month, DOW, TOD
   **************************************************/
   
   data stattod1;
     merge cal2 fish4; 
     by &byvar;
    /**********************************
     Calculate estimates of population
     totals.
    **********************************/
     tot_Es=N*E;
     tot_hcs=N*h_c1;
     tot_trps=N*num_trip;
	 rpue=tot_hcs/tot_Es;
    /**********************************
     Calculate sampling fraction.
    **********************************/
     f_1h=d/N;
     fpc1=1-f_1h;
     f1_inv=1/f_1h;
    /**********************************
     Calculate variance between
     sampling periods - means and
     totals. No total calculation for
     harvest/catch rates.
    **********************************/
    /*********************
     Variance for means.
    *********************/
     s_E1m=fpc1*(v_E1/d);
     s_hc1m=fpc1*(vharcat1/d);
     s_trip1m=fpc1*(vtrip1/d);
     
    /*********************
     Variance for totals.
    *********************/
     s_E1t=N**2*s_E1m;
     s_hc1t=N**2*s_hc1m;
     s_trip1t=N**2*s_trip1m;
    /***********************************
     Calculate variance within
     sampling periods - for means.
    ***********************************/
     s_E2m=(1/(d*N))*v_E;
     s_hc2m=(1/(d*N))*v_hc1;
     s_trip2m=(1/(d*N))*vnumtrip;
     
    /***********************************
     Calculate variance within
     sampling periods - for totals.
    ***********************************/
     s_E2=f1_inv*v_E;
     s_hc2=f1_inv*v_hc1;
     s_trip2=f1_inv*vnumtrip;
    
    /************************************
     Combine between and within variances
     to get two-stage variance estimates
     for each stratum - means and totals.
    ************************************/
    /*********************
     Variance for means.
    *********************/
     s_Esm=s_E1m+s_E2m;
     s_hcsm=s_hc1m+s_hc2m;
     s_tripsm=s_trip1m+s_trip2m;
     
    /*********************
     Variance for totals.
    *********************/
     s_Est=s_E1t+s_E2;
     s_hcst=s_hc1t+s_hc2;
     s_tripst=s_trip1t+s_trip2;
    /************************************
     Calculate standard errors - means
     and totals.
    ************************************/
    /*********************
     Standard Error for
     means.
    *********************/
     SE_Esm=s_Esm**0.5;
     SE_hcsm=s_hcsm**0.5;
     SE_trpsm=s_tripsm**0.5;
     
    /*********************
     Standard Error for
     totals.
    *********************/
     SE_Est=s_Est**0.5;
     SE_hcst=s_hcst**0.5;
     SE_trpst=s_tripst**0.5;


     label
     E='Mean Fishing Effort'
     s_Esm='Variance for Mean Fishing Effort'
     SE_Esm='SE for Mean Fishing Effort'
     tot_Es='Total Fishing Effort'
     s_Est='Variance for Total Fishing Effort'                                                                                        
     SE_Est='SE for Total Fishing Effort'
     h_c1='Mean Harvest/Catch'
     s_hcsm='Variance for Mean Harvest/Catch'
     SE_hcsm='SE for Mean Harvest/Catch'
     tot_hcs='Total Harvest/Catch'
     s_hcst='Variance for Total Harvest/Catch'
     SE_hcst='SE for Total Harvest/Catch'
     num_trip='Mean Number of Angler Trips'
     s_tripsm='Variance for Mean Number of Angler Trips'
     SE_trpsm='SE for Mean Number of Angler Trips'
     tot_trps='Total Number of Angler Trips'
     s_tripst='Variance for Total Number of Angler Trips'
     SE_trpst='SE for Total Number of Angler Trips'
     rpue='Mean Harvest/Catch Per Unit Effort';
   run;

   /**************************************************
     Now, statistics for harvest/catch, effort,
     and number of angler trips by Month and DOW.
   **************************************************/
    proc sort data=stattod1;
      by month dow; 
    run;

	proc univariate data=stattod1 noprint;
      by month dow;
      var N;
      output out=cal2a sum=M;
    run; 

    proc sort data=cal2a; 
      by month dow;
    run; 

    proc sort data=stattod1;
      by month dow; 
    run; 

    data fishery1;
      merge stattod1 cal2a;
      by month dow;
      /**********************************
       Calculate sampling fraction.
      **********************************/
      mW=N/M;
      mW2=mW**2;

	  mn_fsh1=mn_fsh*mW;
	  mn_hrs1=mn_hrs*mW;

      Et1=E*mW;
      s_Et1m=(s_Esm*mW2)/d;
      h_c2=h_c1*mW;
      s_hct1m=(s_hcsm*mW2)/d;
      numtrpt1=num_trip*mW;
      s_trpt1m=(s_tripsm*mW2)/d;
    run;

    proc sort data=fishery1;
      by month dow;
    run;

    proc univariate data=fishery1 noprint;
      by month dow;
      var Et1 h_c2 numtrpt1 mn_fsh1 mn_hrs1 s_Et1m s_hct1m s_trpt1m;
      output out=fishery2 sum=dEm dhc_m dnumtrpm d_fsh1 d_hrs1 
             ds_Em ds_hcm ds_tripm;
    run; 

	proc sort data=fishery2;
	  by month dow;
	run;

	proc sort data=cal2a;
	  by month dow;
	run; 

    data statdow1;
     merge cal2a fishery2;
     by month dow;

     tot_N=M**2;

    /**********************************
     Calculate estimates of population
     totals.
    **********************************/

     dtot_E=M*dEm;
     dtot_hc=M*dhc_m;
     dtot_trp=M*dnumtrpm;
	 drpuem=dtot_hc/dtot_E;

    /*********************
     Variance for totals.
    *********************/

     ds_Et=tot_N*ds_Em;
     ds_hct=tot_N*ds_hcm;
     ds_tript=tot_N*ds_tripm;

    /************************************
     Calculate standard errors - means
     and totals.
    ************************************/
    /*********************
     Standard Error for
     means.
    *********************/

     dSE_Em=ds_Em**0.5;
     dSE_hcm=ds_hcm**0.5;
     dSE_trpm=ds_tripm**0.5;
     

    /*********************
     Standard Error for
     totals.
    *********************/

     dSE_Et=ds_Et**0.5;
     dSE_hct=ds_hct**0.5;
     dSE_trpt=ds_tript**0.5;


     label
     dEm='Mean Fishing Effort'
     ds_Em='Variance for Mean Fishing Effort'
     dSE_Em='SE for Mean Fishing Effort'
     dtot_E='Total Fishing Effort'
     ds_Et='Variance for Total Fishing Effort'
     dSE_Et='SE for Total Fishing Effort'
     dhc_m='Mean Harvest/Catch'
     ds_hcm='Variance for Mean Harvest/Catch'
     dSE_hcm='SE for Mean Harvest/Catch'
     dtot_hc='Total Harvest/Catch'
     ds_hct='Variance for Total Harvest/Catch'
     dSE_hct='SE for Total Harvest/Catch'
     dnumtrpm='Mean Number of Angler Trips'
     ds_tripm='Variance for Mean Number of Angler Trips'
     dSE_trpm='SE for Mean Number of Angler Trips'
     dtot_trp='Total Number of Angler Trips'
     ds_tript='Variance for Total Number of Angler Trips'
     dSE_trpt='SE for Total Number of Angler Trips'
     drpuem='Mean Harvest/Catch Per Unit Effort';
   run;
    
   /**************************************************
     Now, statistics for harvest/catch, effort,
     and number of angler trips by Month.
   **************************************************/
    proc sort data=statdow1; 
      by month;
    run;

    proc univariate data=statdow1 noprint;
      by month;
      var M;
      output out=calm2a sum=NM;
    run; 

    proc sort data=statdow1;
      by month; 
    run;

    proc sort data=calm2a; 
      by month;
    run;

    data fishery3;
      merge statdow1 calm2a;
      by month;
      /**********************************
       Calculate sampling fraction.
      **********************************/
      mW=M/NM;
      mW2=mW**2;

	  mn_fsh1=d_fsh1*mW;
	  mn_hrs1=d_hrs1*mW;

      Et1=dEm*mW;
      s_Et1m=ds_Em*mW2;
      h_c2=dhc_m*mW;
      s_hct1m=ds_hcm*mW2;
      numtrpt1=dnumtrpm*mW;
      s_trpt1m=ds_tripm*mW2;
    run;

    proc sort data=fishery3;
      by month;
    run;

    proc univariate data=fishery3 noprint;
      by month;
      var Et1 h_c2 numtrpt1 d_fsh1 d_hrs1 s_Et1m s_hct1m s_trpt1m;
      output out=fishery4 sum=mEm mhc_m mnumtrpm mn_fsh1 mn_hrs1
                 ms_Em ms_hcm ms_tripm;
    run; 

    proc sort data=calm2a;
      by month; 
    run;

    proc sort data=fishery4;
      by month;
    run;

    data statmon1;
     merge calm2a fishery4;
     by month;
     z=1;

     tot_NM=NM**2;

    /**********************************
     Calculate estimates of population
     totals.
    **********************************/

     mtot_E=NM*mEm;
     mtot_hc=NM*mhc_m;
     mtot_trp=NM*mnumtrpm;
	 mrpuem=mtot_hc/mtot_E;

    /*********************
     Variance for totals.
    *********************/

     ms_Et=tot_NM*ms_Em;
     ms_hct=tot_NM*ms_hcm;
     ms_tript=tot_NM*ms_tripm;

    /************************************
     Calculate standard errors - means
     and totals.
    ************************************/
    /*********************
     Standard Error for
     means.
    *********************/

     mSE_Em=ms_Em**0.5;
     mSE_hcm=ms_hcm**0.5;
     mSE_trpm=ms_tripm**0.5;
     

    /*********************
     Standard Error for
     totals.
    *********************/

     mSE_Et=ms_Et**0.5;
     mSE_hct=ms_hct**0.5;
     mSE_trpt=ms_tript**0.5;


     label
     mEm='Mean Fishing Effort'
     ms_Em='Variance for Mean Fishing Effort'
     mSE_Em='SE for Mean Fishing Effort'
     mtot_E='Total Fishing Effort'
     ms_Et='Variance for Total Fishing Effort'
     mSE_Et='SE for Total Fishing Effort'
     mhc_m='Mean Harvest/Catch'
     ms_hcm='Variance for Mean Harvest/Catch'
     mSE_hcm='SE for Mean Harvest/Catch'
     mtot_hc='Total Harvest/Catch'
     ms_hct='Variance for Total Harvest/Catch'
     mSE_hct='SE for Total Harvest/Catch'
     mnumtrpm='Mean Number of Angler Trips'
     ms_tripm='Variance for Mean Number of Angler Trips'
     mSE_trpm='SE for Mean Number of Angler Trips'
     mtot_trp='Total Number of Angler Trips'
     ms_tript='Variance for Total Number of Angler Trips'
     mSE_trpt='SE for Total Number of Angler Trips'
     mrpuem='Mean Harvest/Catch Per Unit Effort';
   run;

   /**************************************************
     Now, statistics for harvest/catch, effort,
     and number of angler trips adding all strata
     combined - Overall.
   **************************************************/
 
    proc univariate data=statmon1 noprint;
      var NM;
      output out=calt sum=tot_N;
    run;

	data calt2;
	  set calt;
	  z=1;
	run;

	proc sort data=calt2; 
	  by z;
	run;

	proc sort data=statmon1;
	  by z;
	run;

    data fishery5;
      merge statmon1 calt2;
	  by z;
      /**********************************
       Calculate sampling fraction.
      **********************************/
      W=NM/tot_N;
      W2=W**2;

	  mn_fsh2=mn_fsh1*W;
	  mn_hrs2=mn_hrs1*W;

      Et2=mEm*W;
      s_Et2m=ms_Em*W2;
      h_c3=mhc_m*W;
      s_hct2m=ms_hcm*W2;
      numtrpt2=mnumtrpm*W;
      s_trpt2m=ms_tripm*W2;
    run;

    proc univariate data=fishery5 noprint;
      var Et2 s_Et2m h_c3 s_hct2m numtrpt2 s_trpt2m
          mn_fsh2 mn_hrs2;
      output out=fishery6 sum=Em s_Em hc_m s_hcm numtrpm
                 s_tripm mn_fsh3 mn_hrs3;
    run; 

	data fishery6;
	  set fishery6;
	  z=1;
	run;

	proc sort data=fishery6;
	  by z;
	run;

	proc sort data=calt2;
	  by z;
	run;

    data statall1;
     merge calt2 fishery6;
	 by z;

     tot_N2=tot_N**2;
    /**********************************
     Calculate estimates of population
     totals.
    **********************************/
     tot_E=tot_N*Em;
     tot_hc=tot_N*hc_m;
     tot_trp=tot_N*numtrpm;
	 rpuem=tot_hc/tot_E;
    /*********************
     Variance for totals.
    *********************/
     s_Et=tot_N2*s_Em;
     s_hct=tot_N2*s_hcm;
     s_tript=tot_N2*s_tripm;
    /************************************
     Calculate standard errors - means
     and totals.
    ************************************/
    /*********************
     Standard Error for
     means.
    *********************/
     SE_Em=s_Em**0.5;
     SE_hcm=s_hcm**0.5;
     SE_tripm=s_tripm**0.5;
     
    /*********************
     Standard Error for
     totals.
    *********************/
     SE_Et=s_Et**0.5;
     SE_hct=s_hct**0.5;
     SE_tript=s_tript**0.5;

     label
     Em='Mean Fishing Effort'
     s_Em='Variance for Mean Fishing Effort'
     SE_Em='SE for Mean Fishing Effort'
     tot_E='Total Fishing Effort'
     s_Et='Variance for Total Fishing Effort'
     SE_Et='SE for Total Fishing Effort'
     hc_m='Mean Harvest/Catch'
     s_hcm='Variance for Mean Harvest/Catch'
     SE_hcm='SE for Mean Harvest/Catch'
     tot_hc='Total Harvest/Catch'
     s_hct='Variance for Total Harvest/Catch'
     SE_hct='SE for Total Harvest/Catch'
     numtrpm='Mean Number of Angler Trips'
     s_tripm='Variance for Mean Number of Angler Trips'
     SE_tripm='SE for Mean Number of Angler Trips'
     tot_trp='Total Number of Angler Trips'
     s_tript='Variance for Total Number of Angler Trips'
     SE_tript='SE for Total Number of Angler Trips'
     rpuem='Mean Harvest/Catch Per Unit Effort';
   run;
   %end;

   /**************************************************
     This block produces the following statistics:
     1. by Site Month DOW
     2. by Site Month
     3. by Site
     4. Overall
   **************************************************/

   %let opt3=3;
   %if &opt3=&opt %then %do;

   /**************************************************
     Finally, statistics for harvest/catch, effort,
     and number of angler trips by Site, Month, 
     and DOW
   **************************************************/
   
   data statdow1;
     merge cal2 fish4;
     by &byvar;

	 dEm=E;
	 dhc_m=h_c1;
	 dnumtrpm=num_trip;
	 
	 M=N;
	 d_fsh1=mn_fsh; 
     d_hrs1=mn_hrs;
    /**********************************
     Calculate estimates of population
     totals.
    **********************************/
     dtot_E=M*dEm;
     dtot_hc=M*dhc_m;
     dtot_trp=M*dnumtrpm;
	 drpuem=dtot_hc/dtot_E;
    /**********************************
     Calculate sampling fraction.
    **********************************/
     f_1h=d/M;
     fpc1=1-f_1h;
     f1_inv=1/f_1h;
    /**********************************
     Calculate variance between
     sampling periods - means and
     totals. No total calculation for
     harvest/catch rates.
    **********************************/
    /*********************
     Variance for means.
    *********************/
     s_E1m=fpc1*(v_E1/d);
     s_hc1m=fpc1*(vharcat1/d);
     s_trip1m=fpc1*(vtrip1/d);
     
    /*********************
     Variance for totals.
    *********************/
     s_E1t=M**2*s_E1m;
     s_hc1t=M**2*s_hc1m;
     s_trip1t=M**2*s_trip1m;
    /***********************************
     Calculate variance within
     sampling periods - for means.
    ***********************************/
     s_E2m=(1/(d*M))*v_E;
     s_hc2m=(1/(d*M))*v_hc1;
     s_trip2m=(1/(d*M))*vnumtrip;
     
    /***********************************
     Calculate variance within
     sampling periods - for totals.
    ***********************************/
     s_E2=f1_inv*v_E;
     s_hc2=f1_inv*v_hc1;
     s_trip2=f1_inv*vnumtrip;
    
    /************************************
     Combine between and within variances
     to get two-stage variance estimates
     for each stratum - means and totals.
    ************************************/
    /*********************
     Variance for means.
    *********************/
     ds_Em=s_E1m+s_E2m;
     ds_hcm=s_hc1m+s_hc2m;
     ds_tripm=s_trip1m+s_trip2m;
     
    /*********************
     Variance for totals.
    *********************/
     ds_Et=s_E1t+s_E2;
     ds_hct=s_hc1t+s_hc2;
     ds_tript=s_trip1t+s_trip2;
    /************************************
     Calculate standard errors - means
     and totals.
    ************************************/
    /*********************
     Standard Error for
     means.
    *********************/
     dSE_Em=ds_Em**0.5;
     dSE_hcm=ds_hcm**0.5;
     dSE_trpm=ds_tripm**0.5;
     
    /*********************
     Standard Error for
     totals.
    *********************/
     dSE_Et=ds_Et**0.5;
     dSE_hct=ds_hct**0.5;
     dSE_trpt=ds_tript**0.5;

     label
     dEm='Mean Fishing Effort'
     ds_Em='Variance for Mean Fishing Effort'
     dSE_Em='SE for Mean Fishing Effort'
     dtot_E='Total Fishing Effort'
     ds_Et='Variance for Total Fishing Effort'
     dSE_Et='SE for Total Fishing Effort'
     dhc_m='Mean Harvest/Catch'
     ds_hcm='Variance for Mean Harvest/Catch'
     dSE_hcm='SE for Mean Harvest/Catch'
     dtot_hc='Total Harvest/Catch'
     ds_hct='Variance for Total Harvest/Catch'
     dSE_hct='SE for Total Harvest/Catch'
     dnumtrpm='Mean Number of Angler Trips'
     ds_tripm='Variance for Mean Number of Angler Trips'
     dSE_trpm='SE for Mean Number of Angler Trips'
     dtot_trp='Total Number of Angler Trips'
     ds_tript='Variance for Total Number of Angler Trips'
     dSE_trpt='SE for Total Number of Angler Trips'
     drpuem='Mean Harvest/Catch Per Unit Effort';
   run;
    
   /**************************************************
     Now, statistics for harvest/catch, effort,
     and number of angler trips by Site and Month.
   **************************************************/
    proc sort data=statdow1; 
      by site month;
    run;

    proc univariate data=statdow1 noprint;
      by site month;
      var M;
      output out=calm2a sum=NM;
    run; 

    proc sort data=statdow1;
      by site month; 
    run;

    proc sort data=calm2a; 
      by site month;
    run;

    data fishery3;
      merge statdow1 calm2a;
      by site month;
      /**********************************
       Calculate sampling fraction.
      **********************************/
      mW=M/NM;
      mW2=mW**2;

	  mn_fsh1=d_fsh1*mW;
	  mn_hrs1=d_hrs1*mW;

      Et1=dEm*mW;
      s_Et1m=ds_Em*mW2;
      h_c2=dhc_m*mW;
      s_hct1m=ds_hcm*mW2;
      numtrpt1=dnumtrpm*mW;
      s_trpt1m=ds_tripm*mW2;
    run;

    proc sort data=fishery3;
      by site month;
    run;

    proc univariate data=fishery3 noprint;
      by site month;
      var Et1 h_c2 numtrpt1 d_fsh1 d_hrs1 s_Et1m s_hct1m s_trpt1m;
      output out=fishery4 sum=mEm mhc_m mnumtrpm mn_fsh1 mn_hrs1
                 ms_Em ms_hcm ms_tripm;
    run; 

    proc sort data=calm2a;
      by site month; 
    run;

    proc sort data=fishery4;
      by site month;
    run;

    data statmon1;
     merge calm2a fishery4;
     by site month;
     z=1;
	
     tot_NM=NM**2;

    /**********************************
     Calculate estimates of population
     totals.
    **********************************/

     mtot_E=NM*mEm;
     mtot_hc=NM*mhc_m;
     mtot_trp=NM*mnumtrpm;
	 mrpuem=mtot_hc/mtot_E;

    /*********************
     Variance for totals.
    *********************/

     ms_Et=tot_NM*ms_Em;
     ms_hct=tot_NM*ms_hcm;
     ms_tript=tot_NM*ms_tripm;

    /************************************
     Calculate standard errors - means
     and totals.
    ************************************/
    /*********************
     Standard Error for
     means.
    *********************/

     mSE_Em=ms_Em**0.5;
     mSE_hcm=ms_hcm**0.5;
     mSE_trpm=ms_tripm**0.5;
     

    /*********************
     Standard Error for
     totals.
    *********************/

     mSE_Et=ms_Et**0.5;
     mSE_hct=ms_hct**0.5;
     mSE_trpt=ms_tript**0.5;


     label
     mEm='Mean Fishing Effort'
     ms_Em='Variance for Mean Fishing Effort'
     mSE_Em='SE for Mean Fishing Effort'
     mtot_E='Total Fishing Effort'
     ms_Et='Variance for Total Fishing Effort'
     mSE_Et='SE for Total Fishing Effort'
     mhc_m='Mean Harvest/Catch'
     ms_hcm='Variance for Mean Harvest/Catch'
     mSE_hcm='SE for Mean Harvest/Catch'
     mtot_hc='Total Harvest/Catch'
     ms_hct='Variance for Total Harvest/Catch'
     mSE_hct='SE for Total Harvest/Catch'
     mnumtrpm='Mean Number of Angler Trips'
     ms_tripm='Variance for Mean Number of Angler Trips'
     mSE_trpm='SE for Mean Number of Angler Trips'
     mtot_trp='Total Number of Angler Trips'
     ms_tript='Variance for Total Number of Angler Trips'
     mSE_trpt='SE for Total Number of Angler Trips'
     mrpuem='Mean Harvest/Catch Per Unit Effort';
   run;

   /**************************************************
     Now, statistics for harvest/catch, effort,
     and number of angler trips by Site.
   **************************************************/
    proc sort data=statmon1; 
      by site;
    run;

    proc univariate data=statmon1 noprint;
      by site;
      var NM;
      output out=cals2a sum=SM;
    run; 

    proc sort data=statmon1;
      by site; 
    run;

    proc sort data=cals2a; 
      by site;
    run;

    data fishery5;
      merge statmon1 cals2a;
      by site;
      /**********************************
       Calculate sampling fraction.
      **********************************/
      sW=NM/SM;
      sW2=sW**2;

	  mn_fsh2=mn_fsh1*sW;
	  mn_hrs2=mn_hrs1*sW;

      Et2=mEm*sW;
      s_Et2m=ms_Em*sW2;
      h_c3=mhc_m*sW;
      s_hct2m=ms_hcm*sW2;
      numtrpt2=mnumtrpm*sW;
      s_trpt2m=ms_tripm*sW2;
    run;

    proc sort data=fishery5;
      by site;
    run;

    proc univariate data=fishery5 noprint;
      by site;
      var Et2 s_Et2m h_c3 s_hct2m numtrpt2 s_trpt2m
          mn_fsh2 mn_hrs2;
      output out=fishery6 sum=sEm ss_Em shc_m ss_hcm snumtrpm
                 ss_tripm mn_fsh3 mn_hrs3;
    run; 

    proc sort data=cals2a;
      by site; 
    run;

    proc sort data=fishery6;
      by site;
    run;

    data statsit1; 
     merge cals2a fishery6;
     by site;
     z=1;
	 
     tot_SM=SM**2;

    /**********************************
     Calculate estimates of population
     totals.
    **********************************/

     stot_E=SM*sEm;
     stot_hc=SM*shc_m;
     stot_trp=SM*snumtrpm;
	 srpuem=stot_hc/stot_E;

    /*********************
     Variance for totals.
    *********************/

     ss_Et=tot_SM*ss_Em;
     ss_hct=tot_SM*ss_hcm;
     ss_tript=tot_SM*ss_tripm;

    /************************************
     Calculate standard errors - means
     and totals.
    ************************************/
    /*********************
     Standard Error for
     means.
    *********************/

     sSE_Em=ss_Em**0.5;
     sSE_hcm=ss_hcm**0.5;
     sSE_trpm=ss_tripm**0.5;
     
    /*********************
     Standard Error for
     totals.
    *********************/

     sSE_Et=ss_Et**0.5;
     sSE_hct=ss_hct**0.5;
     sSE_trpt=ss_tript**0.5;


     label
     sEm='Mean Fishing Effort'
     ss_Em='Variance for Mean Fishing Effort'
     sSE_Em='SE for Mean Fishing Effort'
     stot_E='Total Fishing Effort'
     ss_Et='Variance for Total Fishing Effort'
     sSE_Et='SE for Total Fishing Effort'
     shc_m='Mean Harvest/Catch'
     ss_hcm='Variance for Mean Harvest/Catch'
     sSE_hcm='SE for Mean Harvest/Catch'
     stot_hc='Total Harvest/Catch'
     ss_hct='Variance for Total Harvest/Catch'
     sSE_hct='SE for Total Harvest/Catch'
     snumtrpm='Mean Number of Angler Trips'
     ss_tripm='Variance for Mean Number of Angler Trips'
     sSE_trpm='SE for Mean Number of Angler Trips'
     stot_trp='Total Number of Angler Trips'
     ss_tript='Variance for Total Number of Angler Trips'
     sSE_trpt='SE for Total Number of Angler Trips'
     srpuem='Mean Harvest/Catch Per Unit Effort';
   run;


   /**************************************************
     Now, statistics for harvest/catch, effort,
     and number of angler trips adding all strata
     combined - Overall.
   **************************************************/
 
    proc univariate data=statsit1 noprint;
      var SM;
      output out=calt sum=tot_N;
    run;

	data calt2;
	  set calt;
	  z=1;
	run;

	proc sort data=calt2; 
	  by z;
	run;

	proc sort data=statsit1;
	  by z;
	run;

    data fishery7;
      merge statsit1 calt2;
	  by z;
      /**********************************
       Calculate sampling fraction.
      **********************************/
      W=sM/tot_N;
      W2=W**2;

	  mn_fsh4=mn_fsh3*W;
	  mn_hrs4=mn_hrs3*W;

      Et2=sEm*W;
      s_Et2m=ss_Em*W2;
      h_c3=shc_m*W;
      s_hct2m=ss_hcm*W2;
      numtrpt2=snumtrpm*W;
      s_trpt2m=ss_tripm*W2;
    run;

    proc univariate data=fishery7 noprint;
      var Et2 s_Et2m h_c3 s_hct2m numtrpt2 s_trpt2m
          mn_fsh4 mn_hrs4;
      output out=fishery8 sum=Em s_Em hc_m s_hcm numtrpm
                 s_tripm mn_fsh5 mn_hrs5;
    run; 

	data fishery8;
	  set fishery8;
	  z=1;
	run;

	proc sort data=fishery8;
	  by z;
	run;

	proc sort data=calt2;
	  by z;
	run;

    data statall1;
     merge calt2 fishery8;
	 by z;

     tot_N2=tot_N**2;
    /**********************************
     Calculate estimates of population
     totals.
    **********************************/
     tot_E=tot_N*Em;
     tot_hc=tot_N*hc_m;
     tot_trp=tot_N*numtrpm;
	 rpuem=tot_hc/tot_E;
    /*********************
     Variance for totals.
    *********************/
     s_Et=tot_N2*s_Em;
     s_hct=tot_N2*s_hcm;
     s_tript=tot_N2*s_tripm;
    /************************************
     Calculate standard errors - means
     and totals.
    ************************************/
    /*********************
     Standard Error for
     means.
    *********************/
     SE_Em=s_Em**0.5;
     SE_hcm=s_hcm**0.5;
     SE_tripm=s_tripm**0.5;
     
    /*********************
     Standard Error for
     totals.
    *********************/
     SE_Et=s_Et**0.5;
     SE_hct=s_hct**0.5;
     SE_tript=s_tript**0.5;

     label
     Em='Mean Fishing Effort'
     s_Em='Variance for Mean Fishing Effort'
     SE_Em='SE for Mean Fishing Effort'
     tot_E='Total Fishing Effort'
     s_Et='Variance for Total Fishing Effort'
     SE_Et='SE for Total Fishing Effort'
     hc_m='Mean Harvest/Catch'
     s_hcm='Variance for Mean Harvest/Catch'
     SE_hcm='SE for Mean Harvest/Catch'
     tot_hc='Total Harvest/Catch'
     s_hct='Variance for Total Harvest/Catch'
     SE_hct='SE for Total Harvest/Catch'
     numtrpm='Mean Number of Angler Trips'
     s_tripm='Variance for Mean Number of Angler Trips'
     SE_tripm='SE for Mean Number of Angler Trips'
     tot_trp='Total Number of Angler Trips'
     s_tript='Variance for Total Number of Angler Trips'
     SE_tript='SE for Total Number of Angler Trips'
     rpuem='Mean Harvest/Catch Per Unit Effort';
   run;
   %end;

   %let opt4=4;
   %if &opt4=&opt %then %do;

   /**************************************************
     Finally, statistics for harvest/catch, effort,
     and number of angler trips by Site, Month, DOW,
     and TOD
   **************************************************/
   
   data stattod1; 
     merge cal2 fish4; 
     by &byvar;
    /**********************************
     Calculate estimates of population
     totals.
    **********************************/
     tot_Es=N*E;
     tot_hcs=N*h_c1;
     tot_trps=N*num_trip;
	 rpue=tot_hcs/tot_Es;
    /**********************************
     Calculate sampling fraction.
    **********************************/
     f_1h=d/N;
     fpc1=1-f_1h;
     f1_inv=1/f_1h;
    /**********************************
     Calculate variance between
     sampling periods - means and
     totals. No total calculation for
     harvest/catch rates.
    **********************************/
    /*********************
     Variance for means.
    *********************/
     s_E1m=fpc1*(v_E1/d);
     s_hc1m=fpc1*(vharcat1/d);
     s_trip1m=fpc1*(vtrip1/d);
     
    /*********************
     Variance for totals.
    *********************/
     s_E1t=N**2*s_E1m;
     s_hc1t=N**2*s_hc1m;
     s_trip1t=N**2*s_trip1m;

    /***********************************
     Calculate variance within
     sampling periods - for means.
    ***********************************/
     s_E2m=(1/(d*N))*v_E;
     s_hc2m=(1/(d*N))*v_hc1;
     s_trip2m=(1/(d*N))*vnumtrip;
     
    /***********************************
     Calculate variance within
     sampling periods - for totals.
    ***********************************/
     s_E2=f1_inv*v_E;
     s_hc2=f1_inv*v_hc1;
     s_trip2=f1_inv*vnumtrip;
    
    /************************************
     Combine between and within variances
     to get two-stage variance estimates
     for each stratum - means and totals.
    ************************************/
    /*********************
     Variance for means.
    *********************/
     s_Esm=s_E1m+s_E2m;
     s_hcsm=s_hc1m+s_hc2m;
     s_tripsm=s_trip1m+s_trip2m;
     
    /*********************
     Variance for totals.
    *********************/
     s_Est=s_E1t+s_E2;
     s_hcst=s_hc1t+s_hc2;
     s_tripst=s_trip1t+s_trip2;
    /************************************
     Calculate standard errors - means
     and totals.
    ************************************/
    /*********************
     Standard Error for
     means.
    *********************/
     SE_Esm=s_Esm**0.5;
     SE_hcsm=s_hcsm**0.5;
     SE_trpsm=s_tripsm**0.5;
     
    /*********************
     Standard Error for
     totals.
    *********************/
     SE_Est=s_Est**0.5;
     SE_hcst=s_hcst**0.5;
     SE_trpst=s_tripst**0.5;


     label
     E='Mean Fishing Effort'
     s_Esm='Variance for Mean Fishing Effort'
     SE_Esm='SE for Mean Fishing Effort'
     tot_Es='Total Fishing Effort'
     s_Est='Variance for Total Fishing Effort'                                                                                        
     SE_Est='SE for Total Fishing Effort'
     h_c1='Mean Harvest/Catch'
     s_hcsm='Variance for Mean Harvest/Catch'
     SE_hcsm='SE for Mean Harvest/Catch'
     tot_hcs='Total Harvest/Catch'
     s_hcst='Variance for Total Harvest/Catch'
     SE_hcst='SE for Total Harvest/Catch'
     num_trip='Mean Number of Angler Trips'
     s_tripsm='Variance for Mean Number of Angler Trips'
     SE_trpsm='SE for Mean Number of Angler Trips'
     tot_trps='Total Number of Angler Trips'
     s_tripst='Variance for Total Number of Angler Trips'
     SE_trpst='SE for Total Number of Angler Trips'
     rpue='Mean Harvest/Catch Per Unit Effort';
   run;

   /**************************************************
     Now, statistics for harvest/catch, effort,
     and number of angler trips by Site, Month, and 
     DOW.
   **************************************************/
    proc sort data=stattod1;
      by site month dow; 
    run;

	proc univariate data=stattod1 noprint;
      by site month dow;
      var N;
      output out=cal2a sum=M;
    run; 

    proc sort data=cal2a;
      by site month dow;
    run; 

    proc sort data=stattod1;
      by site month dow; 
    run; 

    data fishery1;
      merge stattod1 cal2a;
      by site month dow;
      /**********************************
       Calculate sampling fraction.
      **********************************/
      mW=N/M;
      mW2=mW**2;

	  mn_fsh1=mn_fsh*mW;
	  mn_hrs1=mn_hrs*mW;

      Et1=E*mW;
      s_Et1m=(s_Esm*mW2)/d;
      h_c2=h_c1*mW;
      s_hct1m=(s_hcsm*mW2)/d;
      numtrpt1=num_trip*mW;
      s_trpt1m=(s_tripsm*mW2)/d;
    run;

    proc sort data=fishery1;
      by site month dow;
    run;

    proc univariate data=fishery1 noprint;
      by site month dow;
      var Et1 h_c2 numtrpt1 mn_fsh1 mn_hrs1 s_Et1m s_hct1m s_trpt1m;
      output out=fishery2 sum=dEm dhc_m dnumtrpm d_fsh1 d_hrs1 
             ds_Em ds_hcm ds_tripm;
    run; 

	proc sort data=fishery2;
	  by site month dow;
	run;

	proc sort data=cal2a;
	  by site month dow;
	run; 

    data statdow1;
     merge cal2a fishery2;
     by site month dow;

     tot_N=M**2;

    /**********************************
     Calculate estimates of population
     totals.
    **********************************/

     dtot_E=M*dEm;
     dtot_hc=M*dhc_m;
     dtot_trp=M*dnumtrpm;
	 drpuem=dtot_hc/dtot_E;

    /*********************
     Variance for totals.
    *********************/

     ds_Et=tot_N*ds_Em;
     ds_hct=tot_N*ds_hcm;
     ds_tript=tot_N*ds_tripm;

    /************************************
     Calculate standard errors - means
     and totals.
    ************************************/
    /*********************
     Standard Error for
     means.
    *********************/

     dSE_Em=ds_Em**0.5;
     dSE_hcm=ds_hcm**0.5;
     dSE_trpm=ds_tripm**0.5;
     

    /*********************
     Standard Error for
     totals.
    *********************/

     dSE_Et=ds_Et**0.5;
     dSE_hct=ds_hct**0.5;
     dSE_trpt=ds_tript**0.5;


     label
     dEm='Mean Fishing Effort'
     ds_Em='Variance for Mean Fishing Effort'
     dSE_Em='SE for Mean Fishing Effort'
     dtot_E='Total Fishing Effort'
     ds_Et='Variance for Total Fishing Effort'
     dSE_Et='SE for Total Fishing Effort'
     dhc_m='Mean Harvest/Catch'
     ds_hcm='Variance for Mean Harvest/Catch'
     dSE_hcm='SE for Mean Harvest/Catch'
     dtot_hc='Total Harvest/Catch'
     ds_hct='Variance for Total Harvest/Catch'
     dSE_hct='SE for Total Harvest/Catch'
     dnumtrpm='Mean Number of Angler Trips'
     ds_tripm='Variance for Mean Number of Angler Trips'
     dSE_trpm='SE for Mean Number of Angler Trips'
     dtot_trp='Total Number of Angler Trips'
     ds_tript='Variance for Total Number of Angler Trips'
     dSE_trpt='SE for Total Number of Angler Trips'
     drpuem='Mean Harvest/Catch Per Unit Effort';
   run;
    
   /**************************************************
     Now, statistics for harvest/catch, effort,
     and number of angler trips by Site and Month.
   **************************************************/
    proc sort data=statdow1; 
      by site month;
    run;

    proc univariate data=statdow1 noprint;
      by site month;
      var M;
      output out=calm2a sum=NM;
    run; 

    proc sort data=statdow1;
      by site month; 
    run;

    proc sort data=calm2a; 
      by site month;
    run;

    data fishery3;
      merge statdow1 calm2a;
      by site month;
      /**********************************
       Calculate sampling fraction.
      **********************************/
      mW=M/NM;
      mW2=mW**2;

	  mn_fsh1=d_fsh1*mW;
	  mn_hrs1=d_hrs1*mW;

      Et1=dEm*mW;
      s_Et1m=ds_Em*mW2;
      h_c2=dhc_m*mW;
      s_hct1m=ds_hcm*mW2;
      numtrpt1=dnumtrpm*mW;
      s_trpt1m=ds_tripm*mW2;
    run;

    proc sort data=fishery3;
      by site month;
    run;

    proc univariate data=fishery3 noprint;
      by site month;
      var Et1 h_c2 numtrpt1 d_fsh1 d_hrs1 s_Et1m s_hct1m s_trpt1m;
      output out=fishery4 sum=mEm mhc_m mnumtrpm mn_fsh1 mn_hrs1
                 ms_Em ms_hcm ms_tripm;
    run; 

    proc sort data=calm2a;
      by site month; 
    run;

    proc sort data=fishery4;
      by site month;
    run;

    data statmon1;
     merge calm2a fishery4;
     by site month;
     z=1;
	 
     tot_NM=NM**2;

    /**********************************
     Calculate estimates of population
     totals.
    **********************************/

     mtot_E=NM*mEm;
     mtot_hc=NM*mhc_m;
     mtot_trp=NM*mnumtrpm;
	 mrpuem=mtot_hc/mtot_E;

    /*********************
     Variance for totals.
    *********************/

     ms_Et=tot_NM*ms_Em;
     ms_hct=tot_NM*ms_hcm;
     ms_tript=tot_NM*ms_tripm;

    /************************************
     Calculate standard errors - means
     and totals.
    ************************************/
    /*********************
     Standard Error for
     means.
    *********************/

     mSE_Em=ms_Em**0.5;
     mSE_hcm=ms_hcm**0.5;
     mSE_trpm=ms_tripm**0.5;
     

    /*********************
     Standard Error for
     totals.
    *********************/

     mSE_Et=ms_Et**0.5;
     mSE_hct=ms_hct**0.5;
     mSE_trpt=ms_tript**0.5;


     label
     mEm='Mean Fishing Effort'
     ms_Em='Variance for Mean Fishing Effort'
     mSE_Em='SE for Mean Fishing Effort'
     mtot_E='Total Fishing Effort'
     ms_Et='Variance for Total Fishing Effort'
     mSE_Et='SE for Total Fishing Effort'
     mhc_m='Mean Harvest/Catch'
     ms_hcm='Variance for Mean Harvest/Catch'
     mSE_hcm='SE for Mean Harvest/Catch'
     mtot_hc='Total Harvest/Catch'
     ms_hct='Variance for Total Harvest/Catch'
     mSE_hct='SE for Total Harvest/Catch'
     mnumtrpm='Mean Number of Angler Trips'
     ms_tripm='Variance for Mean Number of Angler Trips'
     mSE_trpm='SE for Mean Number of Angler Trips'
     mtot_trp='Total Number of Angler Trips'
     ms_tript='Variance for Total Number of Angler Trips'
     mSE_trpt='SE for Total Number of Angler Trips'
     mrpuem='Mean Harvest/Catch Per Unit Effort';
   run;

   /**************************************************
     Now, statistics for harvest/catch, effort,
     and number of angler trips by Site.
   **************************************************/
    proc sort data=statmon1; 
      by site;
    run;

    proc univariate data=statmon1 noprint;
      by site;
      var NM;
      output out=cals2a sum=SM;
    run; 

    proc sort data=statmon1;
      by site; 
    run;

    proc sort data=cals2a; 
      by site;
    run;

    data fishery5;
      merge statmon1 cals2a;
      by site;
      /**********************************
       Calculate sampling fraction.
      **********************************/
      sW=NM/SM;
      sW2=sW**2;

	  mn_fsh2=mn_fsh1*sW;
	  mn_hrs2=mn_hrs1*sW;

      Et2=mEm*sW;
      s_Et2m=ms_Em*sW2;
      h_c3=mhc_m*sW;
      s_hct2m=ms_hcm*sW2;
      numtrpt2=mnumtrpm*sW;
      s_trpt2m=ms_tripm*sW2;
    run;

    proc sort data=fishery5;
      by site;
    run;

    proc univariate data=fishery5 noprint;
      by site;
      var Et2 s_Et2m h_c3 s_hct2m numtrpt2 s_trpt2m
          mn_fsh2 mn_hrs2;
      output out=fishery6 sum=sEm ss_Em shc_m ss_hcm snumtrpm
                 ss_tripm mn_fsh3 mn_hrs3;
    run; 

    proc sort data=cals2a;
      by site; 
    run;

    proc sort data=fishery6;
      by site;
    run;

    data statsit1; 
     merge cals2a fishery6;
     by site;
     z=1;

     tot_SM=SM**2;

    /**********************************
     Calculate estimates of population
     totals.
    **********************************/

     stot_E=SM*sEm;
     stot_hc=SM*shc_m;
     stot_trp=SM*snumtrpm;
	 srpuem=stot_hc/stot_E;

    /*********************
     Variance for totals.
    *********************/

     ss_Et=tot_SM*ss_Em;
     ss_hct=tot_SM*ss_hcm;
     ss_tript=tot_SM*ss_tripm;

    /************************************
     Calculate standard errors - means
     and totals.
    ************************************/
    /*********************
     Standard Error for
     means.
    *********************/

     sSE_Em=ss_Em**0.5;
     sSE_hcm=ss_hcm**0.5;
     sSE_trpm=ss_tripm**0.5;
     
    /*********************
     Standard Error for
     totals.
    *********************/

     sSE_Et=ss_Et**0.5;
     sSE_hct=ss_hct**0.5;
     sSE_trpt=ss_tript**0.5;


     label
     sEm='Mean Fishing Effort'
     ss_Em='Variance for Mean Fishing Effort'
     sSE_Em='SE for Mean Fishing Effort'
     stot_E='Total Fishing Effort'
     ss_Et='Variance for Total Fishing Effort'
     sSE_Et='SE for Total Fishing Effort'
     shc_m='Mean Harvest/Catch'
     ss_hcm='Variance for Mean Harvest/Catch'
     sSE_hcm='SE for Mean Harvest/Catch'
     stot_hc='Total Harvest/Catch'
     ss_hct='Variance for Total Harvest/Catch'
     sSE_hct='SE for Total Harvest/Catch'
     snumtrpm='Mean Number of Angler Trips'
     ss_tripm='Variance for Mean Number of Angler Trips'
     sSE_trpm='SE for Mean Number of Angler Trips'
     stot_trp='Total Number of Angler Trips'
     ss_tript='Variance for Total Number of Angler Trips'
     sSE_trpt='SE for Total Number of Angler Trips'
     srpuem='Mean Harvest/Catch Per Unit Effort';
   run;


   /**************************************************
     Now, statistics for harvest/catch, effort,
     and number of angler trips adding all strata
     combined - Overall.
   **************************************************/
 
    proc univariate data=statsit1 noprint;
      var SM;
      output out=calt sum=tot_N;
    run;

	data calt2;
	  set calt;
	  z=1;
	run;

	proc sort data=calt2; 
	  by z;
	run;

	proc sort data=statsit1;
	  by z;
	run;

    data fishery7;
      merge statsit1 calt2;
	  by z;
      /**********************************
       Calculate sampling fraction.
      **********************************/
      W=sM/tot_N;
      W2=W**2;

	  mn_fsh4=mn_fsh3*W;
	  mn_hrs4=mn_hrs3*W;

      Et2=sEm*W;
      s_Et2m=ss_Em*W2;
      h_c3=shc_m*W;
      s_hct2m=ss_hcm*W2;
      numtrpt2=snumtrpm*W;
      s_trpt2m=ss_tripm*W2;
    run;

    proc univariate data=fishery7 noprint;
      var Et2 s_Et2m h_c3 s_hct2m numtrpt2 s_trpt2m
          mn_fsh4 mn_hrs4;
      output out=fishery8 sum=Em s_Em hc_m s_hcm numtrpm
                 s_tripm mn_fsh5 mn_hrs5;
    run; 

	data fishery8;
	  set fishery8;
	  z=1;
	run;

	proc sort data=fishery8;
	  by z;
	run;

	proc sort data=calt2;
	  by z;
	run;

    data statall1;
     merge calt2 fishery8;
	 by z;

     tot_N2=tot_N**2;
    /**********************************
     Calculate estimates of population
     totals.
    **********************************/
     tot_E=tot_N*Em;
     tot_hc=tot_N*hc_m;
     tot_trp=tot_N*numtrpm;
	 rpuem=tot_hc/tot_E;
    /*********************
     Variance for totals.
    *********************/
     s_Et=tot_N2*s_Em;
     s_hct=tot_N2*s_hcm;
     s_tript=tot_N2*s_tripm;
    /************************************
     Calculate standard errors - means
     and totals.
    ************************************/
    /*********************
     Standard Error for
     means.
    *********************/
     SE_Em=s_Em**0.5;
     SE_hcm=s_hcm**0.5;
     SE_tripm=s_tripm**0.5;
     
    /*********************
     Standard Error for
     totals.
    *********************/
     SE_Et=s_Et**0.5;
     SE_hct=s_hct**0.5;
     SE_tript=s_tript**0.5;

     label
     Em='Mean Fishing Effort'
     s_Em='Variance for Mean Fishing Effort'
     SE_Em='SE for Mean Fishing Effort'
     tot_E='Total Fishing Effort'
     s_Et='Variance for Total Fishing Effort'
     SE_Et='SE for Total Fishing Effort'
     hc_m='Mean Harvest/Catch'
     s_hcm='Variance for Mean Harvest/Catch'
     SE_hcm='SE for Mean Harvest/Catch'
     tot_hc='Total Harvest/Catch'
     s_hct='Variance for Total Harvest/Catch'
     SE_hct='SE for Total Harvest/Catch'
     numtrpm='Mean Number of Angler Trips'
     s_tripm='Variance for Mean Number of Angler Trips'
     SE_tripm='SE for Mean Number of Angler Trips'
     tot_trp='Total Number of Angler Trips'
     s_tript='Variance for Total Number of Angler Trips'
     SE_tript='SE for Total Number of Angler Trips'
     rpuem='Mean Harvest/Catch Per Unit Effort';
   run;
   %end;

  %mend roving1d;

   /******************************************
     This is the beginning of the fifth macro
   ******************************************/
   %macro roving1e(opt);

    proc format;
     value mon 1='JANUARY' 2='FEBRUARY' 3='MARCH' 4='APRIL'
                 5='MAY' 6='JUNE' 7='JULY' 8='AUGUST' 9='SEPTEMBER'
                 10='OCTOBER' 11='NOVEMBER' 12='DECEMBER';

   %let opt1=1;
   %if &opt1=&opt %then %do;

   data statdow;
     set statdow1;
     keep month dow dEm dSE_Em dtot_E dSE_Et dhc_m
          dSE_hcm dtot_hc dSE_hct dnumtrpm
          dSE_trpm dtot_trp dSE_trpt drpuem;
   run;

   proc print data=statdow L;
     title2 'Descriptive Statistics for Roving Surveys';
     title3 'By Month and DOW';
     var month dow dEm dSE_Em dtot_E dSE_Et dhc_m
          dSE_hcm dtot_hc dSE_hct dnumtrpm
          dSE_trpm dtot_trp dSE_trpt drpuem;
     format month mon.;
   run;

   data statmon;
     set statmon1;
     keep month mEm mSE_Em mtot_E mSE_Et mhc_m
          mSE_hcm mtot_hc mSE_hct mnumtrpm mSE_trpm
          mtot_trp mSE_trpt mrpuem;
   run;

   proc print data=statmon L;
     title2 'Descriptive Statistics for Roving Surveys';
     title3 'By Month';
     var month mEm mSE_Em mtot_E mSE_Et mhc_m
          mSE_hcm mtot_hc mSE_hct mnumtrpm mSE_trpm
          mtot_trp mSE_trpt mrpuem;
     format month mon.;
   run;

   data statall;
     set statall1;
     keep Em SE_Em tot_E SE_Et hc_m
          SE_hcm tot_hc SE_hct numtrpm SE_tripm
          tot_trp SE_tript rpuem;
   run;

   proc print data=statall L;
     title2 'Descriptive Statistics for Roving Surveys';
     title3 'All Strata Levels Combined';
     var Em SE_Em tot_E SE_Et hc_m
          SE_hcm tot_hc SE_hct numtrpm SE_tripm
          tot_trp SE_tript rpuem;
   run;
   %end;

   %let opt2=2;
   %if &opt2=&opt %then %do;

   data stattod;
     set stattod1;
     keep month dow tod E SE_Esm tot_Es SE_Est h_c1
          SE_hcsm tot_hcs SE_hcst num_trip
          SE_trpsm tot_trps SE_trpst rpue;
   run;

   proc print data=stattod L;
     title2 'Descriptive Statistics for Roving Surveys';
     title3 'by Month, DOW, and TOD';
     var month dow tod E SE_Esm tot_Es SE_Est h_c1
          SE_hcsm tot_hcs SE_hcst num_trip
          SE_trpsm tot_trps SE_trpst rpue;
     format month mon.;
   run;

   data statdow;
     set statdow1;
     keep month dow dEm dSE_Em dtot_E dSE_Et dhc_m
          dSE_hcm dtot_hc dSE_hct dnumtrpm
          dSE_trpm dtot_trp dSE_trpt drpuem;
   run;

   proc print data=statdow L;
     title2 'Descriptive Statistics for Roving Surveys';
     title3 'By Month and DOW';
     var month dow dEm dSE_Em dtot_E dSE_Et dhc_m
          dSE_hcm dtot_hc dSE_hct dnumtrpm
          dSE_trpm dtot_trp dSE_trpt drpuem;
     format month mon.;
   run;

   data statmon;
     set statmon1;
     keep month mEm mSE_Em mtot_E mSE_Et mhc_m
          mSE_hcm mtot_hc mSE_hct mnumtrpm mSE_trpm
          mtot_trp mSE_trpt mrpuem;
   run;

   proc print data=statmon L;
     title2 'Descriptive Statistics for Roving Surveys';
     title3 'By Month';
     var month mEm mSE_Em mtot_E mSE_Et mhc_m
          mSE_hcm mtot_hc mSE_hct mnumtrpm mSE_trpm
          mtot_trp mSE_trpt mrpuem;
     format month mon.;
   run;

   data statall;
     set statall1;
     keep Em SE_Em tot_E SE_Et hc_m
          SE_hcm tot_hc SE_hct numtrpm SE_tripm
          tot_trp SE_tript rpuem;
   run;

   proc print data=statall L;
     title2 'Descriptive Statistics for Roving Surveys';
     title3 'All Strata Levels Combined';
     var Em SE_Em tot_E SE_Et hc_m
          SE_hcm tot_hc SE_hct numtrpm SE_tripm
          tot_trp SE_tript rpuem;
   run;
   %end;

   %let opt3=3;
   %if &opt3=&opt %then %do;

   data statdow;
     set statdow1;
     keep site month dow dEm dSE_Em dtot_E dSE_Et dhc_m
          dSE_hcm dtot_hc dSE_hct dnumtrpm
          dSE_trpm dtot_trp dSE_trpt drpuem;
   run;

   proc print data=statdow L;
     title2 'Descriptive Statistics for Roving Surveys';
     title3 'By Site, Month, and DOW';
     var site month dow dEm dSE_Em dtot_E dSE_Et dhc_m
          dSE_hcm dtot_hc dSE_hct dnumtrpm
          dSE_trpm dtot_trp dSE_trpt drpuem;
     format month mon.;
   run;

   data statmon;
     set statmon1;
     keep site month mEm mSE_Em mtot_E mSE_Et mhc_m
          mSE_hcm mtot_hc mSE_hct mnumtrpm mSE_trpm
          mtot_trp mSE_trpt mrpuem;
   run;

   proc print data=statmon L;
     title2 'Descriptive Statistics for Roving Surveys';
     title3 'By Site and Month';
     var site month mEm mSE_Em mtot_E mSE_Et mhc_m
          mSE_hcm mtot_hc mSE_hct mnumtrpm mSE_trpm
          mtot_trp mSE_trpt mrpuem;
     format month mon.;
   run;

   data statsite;
     set statsit1;
     keep site sEm sSE_Em stot_E sSE_Et shc_m
          sSE_hcm stot_hc sSE_hct snumtrpm sSE_trpm
          stot_trp sSE_trpt srpuem;
   run;

   proc print data=statsite L;
     title2 'Descriptive Statistics for Roving Surveys';
     title3 'By Site';
     var site sEm sSE_Em stot_E sSE_Et shc_m
          sSE_hcm stot_hc sSE_hct snumtrpm sSE_trpm
          stot_trp sSE_trpt srpuem;
   run;

   data statall;
     set statall1;
     keep Em SE_Em tot_E SE_Et hc_m
          SE_hcm tot_hc SE_hct numtrpm SE_tripm
          tot_trp SE_tript rpuem;
   run;

   proc print data=statall L;
     title2 'Descriptive Statistics for Roving Surveys';
     title3 'All Strata Levels Combined';
     var Em SE_Em tot_E SE_Et hc_m
          SE_hcm tot_hc SE_hct numtrpm SE_tripm
          tot_trp SE_tript rpuem;
   run;
   %end;

   %let opt4=4;
   %if &opt4=&opt %then %do;

   data stattod;
     set stattod1;
     keep site month dow tod E SE_Esm tot_Es SE_Est h_c1
          SE_hcsm tot_hcs SE_hcst num_trip
          SE_trpsm tot_trps SE_trpst rpue;
   run;

   proc print data=stattod L;
     title2 'Descriptive Statistics for Roving Surveys';
     title3 'by Site, Month, DOW, and TOD';
     var site month dow tod E SE_Esm tot_Es SE_Est h_c1
          SE_hcsm tot_hcs SE_hcst num_trip
          SE_trpsm tot_trps SE_trpst rpue;
     format month mon.;
   run;

   data statdow;
     set statdow1;
     keep site month dow dEm dSE_Em dtot_E dSE_Et dhc_m
          dSE_hcm dtot_hc dSE_hct dnumtrpm
          dSE_trpm dtot_trp dSE_trpt drpuem;
   run;

   proc print data=statdow L;
     title2 'Descriptive Statistics for Roving Surveys';
     title3 'By Site, Month, and DOW';
     var site month dow dEm dSE_Em dtot_E dSE_Et dhc_m
          dSE_hcm dtot_hc dSE_hct dnumtrpm
          dSE_trpm dtot_trp dSE_trpt drpuem;
     format month mon.;
   run;

   data statmon;
     set statmon1;
     keep site month mEm mSE_Em mtot_E mSE_Et mhc_m
          mSE_hcm mtot_hc mSE_hct mnumtrpm mSE_trpm
          mtot_trp mSE_trpt mrpuem;
   run;

   proc print data=statmon L;
     title2 'Descriptive Statistics for Roving Surveys';
     title3 'By Site and Month';
     var site month mEm mSE_Em mtot_E mSE_Et mhc_m
          mSE_hcm mtot_hc mSE_hct mnumtrpm mSE_trpm
          mtot_trp mSE_trpt mrpuem;
     format month mon.;
   run;

   data statsite;
     set statsit1;
     keep site sEm sSE_Em stot_E sSE_Et shc_m
          sSE_hcm stot_hc sSE_hct snumtrpm sSE_trpm
          stot_trp sSE_trpt srpuem;
   run;

   proc print data=statsite L;
     title2 'Descriptive Statistics for Roving Surveys';
     title3 'By Site';
     var site sEm sSE_Em stot_E sSE_Et shc_m
          sSE_hcm stot_hc sSE_hct snumtrpm sSE_trpm
          stot_trp sSE_trpt srpuem;
   run;

   data statall;
     set statall1;
     keep Em SE_Em tot_E SE_Et hc_m
          SE_hcm tot_hc SE_hct numtrpm SE_tripm
          tot_trp SE_tript rpuem;
   run;

   proc print data=statall L;
     title2 'Descriptive Statistics for Roving Surveys';
     title3 'All Strata Levels Combined';
     var Em SE_Em tot_E SE_Et hc_m
          SE_hcm tot_hc SE_hct numtrpm SE_tripm
          tot_trp SE_tript rpuem;
   run;
   %end;

   %mend roving1e;

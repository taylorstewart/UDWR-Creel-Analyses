/*calendar.sas

  Created:  3/99
  Created By:  Heather Thomas

  Purpose:  Calendar program to be merge in with creel survey programs

  Details:  sdate=start date of creel analysis
            fdate=end date of creel analysis


*/

   /********************************************************
    Macro program that creates a calendar sas data file that
    can be merged in with creel data files.
   ********************************************************/


   %macro date(sdate, fdate);

   %let sdate=&sdate;
   %let fdate=&fdate;

   /*********************************************************
    This formats month as character rather than numeric when
    printed
   *********************************************************/

   proc format;
     value mon 1='JANUARY' 2='FEBRUARY' 3='MARCH' 4='APRIL'
                 5='MAY' 6='JUNE' 7='JULY' 8='AUGUST' 9='SEPTEMBER'
                 10='OCTOBER' 11='NOVEMBER' 12='DECEMBER';


   data ds.calendar;
     date1=input(symget('sdate'),mmddyy10.);
     date2=input(symget('fdate'),mmddyy10.);

   /*********************************************************
    Set weekends and holidays to dow=weekend and all other
    days to weekday.  Used wd=weekday and we=weekend/holiday.
   *********************************************************/

     do date=date1 to date2;
        month=month(date);
        day=day(date);
        year=year(date);
        wday=weekday(date);
        if wday=1 or wday=7 or
           (month=1 and day=1) or
           (month=1 and 15<=day<=21 and wday=2) or
           (month=2 and 15<=day<=21 and wday=2) or
           (month=5 and 25<=day<=31 and wday=2) or
           (month=7 and day=4) or
           (month=7 and day=24) or
           (month=9 and 1<=day<=7 and wday=2) or
           (month=11 and 22<=day<=28 and wday=5) or
           (month=12 and day=24) or
           (month=12 and day=25) or
           (month=12 and day=31) then dow='we';
        else dow='wd';
        keep date day month year dow wday;
        output;
      end;

     format date mmddyy10.;

     proc print data=ds.calendar;
       format month mon.;
     run;

     %mend date;

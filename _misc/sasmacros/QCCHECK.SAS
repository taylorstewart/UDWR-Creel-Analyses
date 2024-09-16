/*qccheck.sas

  Created:  4/02
  Created By:  Heather Thomas

  Purpose:  Quality Control Checks for the following checks:
			1. Check that month, day, year are consistent between datasets.

			2. Compare that assignment of WD and WE in count and 
			   and contact datasets are consistent with calendar.

			3. Check that month, day, year for the datasets 
			   are consistent with month, day, year for the 
			   calendar.

*/

   /********************************************************
    Macro program performs date checks between data files
    and calendar.
   ********************************************************/

   %macro check(data1, data2);
  		data d1;
		  set &data1;
		  dow1=dow;

		  if month=1 then mon='01';
		  if month=2 then mon='02';
		  if month=3 then mon='03';
		  if month=4 then mon='04';
		  if month=5 then mon='05';
		  if month=6 then mon='06';
		  if month=7 then mon='07';
		  if month=8 then mon='08';
		  if month=9 then mon='09';
		  if month=10 then mon='10';
		  if month=11 then mon='11';
		  if month=12 then mon='12';

		  if day=1 then dy='01';
		  if day=2 then dy='02';
		  if day=3 then dy='03';
		  if day=4 then dy='04';
		  if day=5 then dy='05';
		  if day=6 then dy='06';
		  if day=7 then dy='07';
		  if day=8 then dy='08';
		  if day=9 then dy='09';
		  if day=10 then dy='10';
		  if day=11 then dy='11';
		  if day=12 then dy='12';
		  if day=13 then dy='13';
		  if day=14 then dy='14';
		  if day=15 then dy='15';
		  if day=16 then dy='16';
		  if day=17 then dy='17';
		  if day=18 then dy='18';
		  if day=19 then dy='19';
		  if day=20 then dy='20';
		  if day=21 then dy='21';
		  if day=22 then dy='22';
		  if day=23 then dy='23';
		  if day=24 then dy='24';
		  if day=25 then dy='25';
		  if day=26 then dy='26';
		  if day=27 then dy='27';
		  if day=28 then dy='28';
		  if day=29 then dy='29';
		  if day=30 then dy='30';
		  if day=31 then dy='31';

		  dt1=compress(mon||dy||year);
		  date1=input(dt1, mmddyy10.);
		  keep month day year dow1 date1;
		  format date1 mmddyy10.;
		run; 

		proc sort data=d1;
		  by month day year;
		run;

		data d2;
		  set &data2;
		  dow2=dow;
		  if month=1 then mon='01';
		  if month=2 then mon='02';
		  if month=3 then mon='03';
		  if month=4 then mon='04';
		  if month=5 then mon='05';
		  if month=6 then mon='06';
		  if month=7 then mon='07';
		  if month=8 then mon='08';
		  if month=9 then mon='09';
		  if month=10 then mon='10';
		  if month=11 then mon='11';
		  if month=12 then mon='12';

		  if day=1 then dy='01';
		  if day=2 then dy='02';
		  if day=3 then dy='03';
		  if day=4 then dy='04';
		  if day=5 then dy='05';
		  if day=6 then dy='06';
		  if day=7 then dy='07';
		  if day=8 then dy='08';
		  if day=9 then dy='09';
		  if day=10 then dy='10';
		  if day=11 then dy='11';
		  if day=12 then dy='12';
		  if day=13 then dy='13';
		  if day=14 then dy='14';
		  if day=15 then dy='15';
		  if day=16 then dy='16';
		  if day=17 then dy='17';
		  if day=18 then dy='18';
		  if day=19 then dy='19';
		  if day=20 then dy='20';
		  if day=21 then dy='21';
		  if day=22 then dy='22';
		  if day=23 then dy='23';
		  if day=24 then dy='24';
		  if day=25 then dy='25';
		  if day=26 then dy='26';
		  if day=27 then dy='27';
		  if day=28 then dy='28';
		  if day=29 then dy='29';
		  if day=30 then dy='30';
		  if day=31 then dy='31';

		  dt2=compress(mon||dy||year);
		  date2=input(dt2, mmddyy10.);
		  keep month day year dow2 date2;
		  format date2 mmddyy10.;
		run;
		
		proc sort data=d2;
		  by month day year;
		run;

		proc sort data=ds.calendar;
		  by month day year;
		run;

		data d3;
		  merge d1 d2;
		  by month day year;
		  if date1 ne date2 then flag='Inconsistency with Month, Day, Year in Files';
		run;

		proc print data=d3;
		  title 'Check that Month, Day, and Year are Consistent between Files';
		  where flag ne ' ';
		  var year month day dow1 dow2 flag;
		run;

		data d4;
		  merge d1 ds.calendar;
		  by month day year;
		  if (dow ne dow1 and dow1 ne ' ') then flag='DOW assigned incorrectly in Data1';
		run;

		proc print data=d4;
		  title 'Check that DOW is Assigned Correctly in Data1';
          where flag ne ' '; 
		  var year month day dow dow1 flag;
		run;

		data d4a;
		  merge d1(in=x) ds.calendar;
		  by month day year;
		  if x=1;
		  if date ne date1 then flag='Inconsistency with Month, Day, Year in Data1 and Calendar';
		run;

		proc print data=d4a;
		  title 'Check that Month, Day, and Year in Calendar and Data1 are Consistent';
          where flag ne ' '; 
		  var year month day flag;
		run;

		data d5;
		  merge d2 ds.calendar;
		  by month day year;
		  if (dow ne dow2 and dow2 ne ' ') then flag='DOW assigned incorrectly in Data2';
		run;

		proc print data=d5;
		  title 'Check that DOW is Assigned Correctly in Data2';
          where flag ne ' '; 
		  var year month day dow dow2 flag;
		run; 

		data d5a;
		  merge d2 (in=x) ds.calendar;
		  by month day year;
		  if x=1;
		  if date ne date2 then flag='Inconsistency with Month, Day, Year in Data2 and Calendar';
		run;

		proc print data=d5a;
		  title 'Check that Month, Day, and Year in Calendar and Data1 are Consistent';
          where flag ne ' '; 
		  var year month day flag;
		run;

   %mend check;
    
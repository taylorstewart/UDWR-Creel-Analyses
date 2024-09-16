/* exportxls.sas

 	Created: 4/2002
 	Created by: Heather Thomas

 	Purpose: To output sas dataset to an excel file of current version.
			 Automatically replaces old excel file if the excel file already exist.

 	Details: data     = sas data set name
       		 path     = directory for excel file
       		 filename = name of excel file

			 Can be used with SAS v8 and higher.
*/

%macro export(data, path, filename);
	proc export data=&data
		outfile="&path&filename"
		dbms=xlsx replace;
	run;
%mend export;

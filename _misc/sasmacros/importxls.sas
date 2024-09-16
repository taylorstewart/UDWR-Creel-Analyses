/*importxls.sas

        Created: 4/2002
        Created By: Heather Thomas

        Purpose: To read xls tables in as SAS datasets using SAS v8 or higher.

		Details: Path     = excel file path
			     filename = name of the file to be read into sas dataset
			     data     = sas dataset name 

			     Later versions of Excel can be used with this macro as long as SAS
			     v8 or higher is used.
		
*/

	%macro import(path, filename, data);
        proc import
          datafile="&path&filename"
          out=&data
          dbms=xlsx
          replace;
        run;
	%mend import;

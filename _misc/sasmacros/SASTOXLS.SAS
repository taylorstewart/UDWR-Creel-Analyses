/*sastoxls.sas

  Created:  1/99
  Created By:  Heather Thomas

  Purpose:  To write a temporary sas data set to an excel file

  Details:  ds=sas data set name
            path_xls = directory for excel file
            file = name of excel file


*/

  %macro sastoxls(ds, path_xls, file);

     proc dbload dbms=xls data=&ds;
       path="&path_xls.&file";
         putnames=yes;
         load;
     run;

  %mend sastoxls;

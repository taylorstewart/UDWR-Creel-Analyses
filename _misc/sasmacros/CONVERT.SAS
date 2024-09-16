/*convert.sas

  Created:  1/99
  Created By:  Heather Thomas

  Purpose:  Macro to convert from excel files to sas data sets

  Details:  Pathname is specified in &path
            Excel filename is specified in &file
            Scantype denotes whether to scan column in data set


*/

  %macro convert(path, file, scantype, mixed, label);
     libname dlib "&path";
     libname avlib "&path.temp";

     proc access dbms=xls;
       create avlib.&file..access;
       path="&path.&file";
       scantype=&scantype;
       getnames=yes;
       assign=yes;
       mixed=&mixed;
       list all;
       create avlib.&file..view;
       select all;
     run;

     proc access
       vd=avlib.&file
       out=&file;
     run;

     data dlib.&file (label="&label");
       set &file;
     run;

     proc contents data=dlib.&file;
     run;

   %mend convert;

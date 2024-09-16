       %macro freq(data, var);

        proc freq data=&data noprint;
         title3 "Frequency Distributions";
          tables &var/out=&var;
         run;
         data &var;
          set &var;
          variable='          ';
          variable="&var";
         run;

         proc print data=&var;
          title 'Frequency Distribution';
          title2 "For &var";
         run;
        %mend freq;

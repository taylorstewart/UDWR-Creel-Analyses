%macro meansby(data, byvar, var);
  proc sort data=&data;
    by &byvar;
  run;

  proc univariate data=&data noprint;
    by &byvar;
    var &var;
    output out=&var n=n mean=mean std=sd
                    var=variance stdmean=sem min=minimum
                    median=median max=maximum;
  run;

  proc print data=&var;
    title "Descriptive Statistics by &byvar";
    title2 "For &var";
  run;

%mend meansby;

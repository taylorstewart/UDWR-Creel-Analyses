%macro means(data, var);

  proc univariate data=&data noprint;
    var &var;
    output out=&var n=n mean=mean std=sd
                    var=variance stdmean=sem min=minimum
                    median=median max=maximum;
  run;

   proc print data=&var;
    title 'Descriptive Statistics';
    title2 "For &var";
  run;

%mend means;

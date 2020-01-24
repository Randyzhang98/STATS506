/* -------------------------------------------------------------------------- *
 *
 * SAS code for Problem set 6, Question 1.
 * 
 * Author: Huayu Li
 * Updated: Dec 4, 2019
 * -------------------------------------------------------------------------- *
*/
/* 80: ---------------------------------------------------------------------- */

/* To begin with, set the working directory first. */
libname dir '~/STATS506/PS6/';
%let path=~/STATS506/PS6/;

/* To begin with, import the data ps2_q2c.csv */
proc import datafile="&path.ps2_q2c.csv" out=dir.data replace;
run;

data recs;
set dir.data;
log_Dist=log(abs(Dist));
log_Maxdev=log(abs(Max_dev));
log_Meandev=log(abs(Mean_dev));
log_auc=log(abs(AUC));
run;

/* Then using the linear mixed model to fit the data */
/* First Dist */
Ods output estimates = est_logdist;
proc mixed data=recs;
class subject_nr Condition exemplar;
model log_Dist = Condition/ ddfm=kr s;
random intercept / subject=subject_nr;
random intercept / subject=exemplar;
estimate 'log_Dist' Condition 1 -1/ cl;
run;

/* Then Max_dev */
Ods output estimates = est_maxdev;
proc mixed data=recs;
class subject_nr Condition exemplar;
model log_Maxdev = Condition/ ddfm=kr s;
random intercept / subject=subject_nr;
random intercept / subject=exemplar;
estimate 'log_Maxdev' Condition 1 -1/ cl;
run;

/* Then Mean_dev */
Ods output estimates = est_meandev;
proc mixed data=recs;
class subject_nr Condition exemplar;
model log_Meandev = Condition/ ddfm=kr s;
random intercept / subject=subject_nr;
random intercept / subject=exemplar;
estimate 'log_Meandev' Condition 1 -1/ cl;
run;

/* Finally AUC */
Ods output estimates = est_logauc;
proc mixed data=recs;
class subject_nr Condition exemplar;
model log_AUC = Condition/ ddfm=kr s;
random intercept / subject=subject_nr;
random intercept / subject=exemplar;
estimate 'log_AUC' Condition 1 -1/ cl;
run;

/* Here we produce the result data for output; one for the 
 * absolute value of coefficient of condition, the other for
 * the confidence interval. */

data Cond_result;
set est_logdist est_maxdev est_meandev est_logauc;
run;

data Cond_result;
set Cond_result; 
keep Label Estimate Lower Upper;
run;

data Cond_result;
set Cond_result;
Condition=Estimate;
Lower_CI=Lower;
Upper_CI=Upper;
drop Estimate Lower Upper;
run;

/* Saving result into file PS6_q1.csv, and create the .lst file */
proc export data=cond_result
outfile='/folders/myfolders/STATS506/PS6/PS6_q1.csv' replace;
run;

ods listing;
proc printto print='/folders/myfolders/STATS506/PS6/ps6_q1.lst'
new;
run;

proc print data=Cond_result;
title 'Final result of Question 1';
run;

proc printto;
run;
ods listing close;

















/* -------------------------------------------------------------------------- *
 * Stats 506 Problem Set 6 Question 1.
 *
 * This file imports RECS data from:
 *   ./mousetrap_data.csv
 *   Which is the output from PS 2 Q2 d
 *
 * Author: Sijun Zhang 89934761
 * Updated: Dec 8, 2019
 * -------------------------------------------------------------------------- *
*/
/* 80: ---------------------------------------------------------------------- */
/* data library for reading/writing data: ----------------------------------- */
libname mylib '/folders/myfolders/Problem Set 6/';

/* import delimited data with proc import: ---------------------------------- */
proc import
 datafile='/folders/myfolders/Problem Set 6/mousetrap_data.csv'
 out=mylib.ps2q2d
 replace;
run;

/* transform the data into log form and set auc < 1 as 1 */
data mylib.ps6_q1;
 set mylib.ps2q2d;
 AUC=max(AUC, 1);
 tot_dist = log(tot_dist);
 max_abs_dev = log(max_abs_dev);
 avg_abs_dev = log(avg_abs_dev);
 AUC = log(AUC);
 by subject_nr count_trial;
run;

proc format library=mylib.ps6_q1;
 value Condition
  1="Typical"
  2="Atypical";
run;

/* Use Linear Mixed model fit tot_dist */
ods output
 SolutionF = tot_dist_re;

proc mixed data=mylib.ps6_q1;
 class Condition Exemplar;
 model tot_dist = Condition / cl;
 random intercept / subject=Exemplar;
 random intercept / subject=subject_nr;
run;

/* Use Linear Mixed model fit max_abs_dev */
ods output
 SolutionF = max_abs_dev_re;

proc mixed data=mylib.ps6_q1;
 class Condition Exemplar;
 model max_abs_dev = Condition / cl;
 random intercept / subject=Exemplar;
 random intercept / subject=subject_nr;
run;

/* Use Linear Mixed model fit avg_abs_dev */
ods output
 SolutionF = avg_abs_dev_re;

proc mixed data=mylib.ps6_q1;
 class Condition Exemplar;
 model avg_abs_dev = Condition / cl;
 random intercept / subject=Exemplar;
 random intercept / subject=subject_nr;
run;

/* Use Linear Mixed model fit AUC */
ods output
 SolutionF = AUC_re;

proc mixed data=mylib.ps6_q1;
 class Condition Exemplar;
 model AUC = Condition / cl;
 random intercept / subject=Exemplar;
 random intercept / subject=subject_nr;
run;

/* add label name */
data name;
 input label$;
 cards;
  tot_list
  max_dev
  avg_dev
  AUC;
run;

/* collect the result for output */
data est_result;
 set tot_dist_re max_abs_dev_re avg_abs_dev_re AUC_re;
 if (_n_=2)|(_n_=5)|(_n_=8)|(_n_=11);
 keep Estimate Lower Upper;
run;

data final_result;
 merge name est_result;
 Relative_effect_est = exp(Estimate);
 Relative_effect_lwr = exp(Lower);
 Relative_effect_upr = exp(Upper);
run;

/* Saving result into file ps6_q1.csv, and create the .lst file */
proc export data=final_result
 outfile='/folders/myfolders/Problem Set 6/ps6_q1.csv' replace;
run;

ods listing;
proc printto print='/folders/myfolders/Problem Set 6/ps6_q1.lst'
new;
run;

proc print data=final_result;
 title 'Final result of Question 1';
run;

proc printto;
run;
ods listing close;



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

data mylib.testps6_q1;
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

/* refit the model above using maximum likelihood */
ods output
 estimates = tot_dist_re;


/* subject level slope and intercept are uncorrelated */
proc mixed data=mylib.testps6_q1;
 class Condition Exemplar subject_nr;
 model tot_dist = Condition / ddfm=kenwardroger;
 random intercept / subject=Exemplar;
 random intercept / subject=subject_nr;
 estimate 'tot_dist' Condition 1 - 1/ cl;
run;




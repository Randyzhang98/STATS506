/* -------------------------------------------------------------------------- *
 * Stats 506 Problem Set 6 Question 2.
 *
 * This file imports RECS data from:
 *   ./recs2015_public_v4.csv
 *   Which is continue from PS2 Q1
 *
 * Author: Sijun Zhang 89934761
 * Updated: Dec 8, 2019
 * -------------------------------------------------------------------------- *
*/
/* 80: ---------------------------------------------------------------------- */

libname mylib '/folders/myfolders/Problem Set 6/';

/* import delimited data with proc import: ---------------------------------- */
proc import
 datafile='/folders/myfolders/Problem Set 6/recs2015_public_v4.csv'
 out=mylib.recs
 replace;
run;

/* a. First, use proc transpose to reshape the repliacte weights to a longer format.  */
/* Save these to disk as brrwt_long.sas7bdat using a two part filename. */
proc transpose data=mylib.recs
			   out=mylib.brrwt_long;
			   var BRRWT1-BRRWT96 ;
			   by DOEID;
run;

data mylib.brrwt_long;
 set mylib.brrwt_long;
 repl = _NAME_;
 wt = COL1;
 keep DOEID repl wt;
run;

/* b. Estiamte the national average home temperature at night, among homes */
/* that use space heating. */
/* need to eliminate the NA data repl, so to rerun the transpose */
data temp;
 set mylib.recs;
 if (TEMPNITE ~= -2)&&(HEATHOME = 1);
 keep DOEID TEMPNITE NWEIGHT;
run;

/* need to eliminate the NA data repl */
data brrwt_long_b;
 set mylib.recs;
 if (TEMPNITE ~= -2)&&(HEATHOME = 1);
 keep DOEID BRRWT1-BRRWT96;
run;

proc transpose data=brrwt_long_b
			   out=brrwt_long_b;
			   var BRRWT1-BRRWT96 ;
			   by DOEID;
run;

data brrwt_long_b;
 set brrwt_long_b;
 repl = _NAME_;
 wt = COL1;
 keep DOEID repl wt;
run;

/* Join home type to weights: -------------------------------------------------- */
data temp;
 merge temp brrwt_long_b;
 by DOEID;
 temp_est=TEMPNITE*NWEIGHT;
 temp_repl=TEMPNITE*wt;
run;

/* use the summary to calc the sum of variable, need to drop ovrall summary*/
proc summary data=temp;
 class repl;
 output out=temp_sum
 sum(temp_est) = sum_temp_est
 sum(wt) = sum_wt_repl
 sum(temp_repl) = sum_temp_repl
 sum(NWEIGHT) = sum_nwt;
run;

/* calc estimation and sse */
data temp;
 set temp_sum;
 if (_TYPE_ ~= 0);
 temp_est = sum_temp_est/sum_nwt;
 temp_repl = sum_temp_repl/sum_wt_repl;
 std_err = (4*(temp_est-temp_repl)**2);
run;

/* average the error */
proc summary data=temp;
 output out=temp
 mean(temp_est) = temp_est
 mean(std_err) = std_err;
run;

/* calc CI */
data temp;
 set temp;
 CI_lwr = temp_est + quantile('NORMAL',0.025)*std_err**0.5;
 CI_upr = temp_est + quantile('NORMAL',0.975)*std_err**0.5;
 if (_TYPE_ ~= .);
 drop _TYPE_ _FREQ_ std_err;
run;

/* output the result */
proc print data=temp;
 title 'national average home temperature at night';
run;

proc export data=temp
 outfile='/folders/myfolders/Problem Set 6/ps6_q2b.csv' replace;
run;

/* c. Next, by census division, estimate the average winter home
temperatures at night, during the day with someone home, and during
the day with no one home (when applicable) */
/* need to eliminate the NA data repl, so to repeat the transpose */
data temp_c;
 set mylib.recs;
 if (TEMPNITE ~= -2)&&(TEMPHOME ~= -2)&&(TEMPGONE ~= -2);
 keep DOEID DIVISION TEMPNITE TEMPHOME TEMPGONE NWEIGHT;
run;

/* need to eliminate the NA data repl */
data brrwt_long_c;
 set mylib.recs;
 if (TEMPNITE ~= -2)&&(TEMPHOME ~= -2)&&(TEMPGONE ~= -2);
 keep DOEID DIVISION BRRWT1-BRRWT96;
run;

proc transpose data=brrwt_long_c
			   out=brrwt_long_c;
			   var BRRWT1-BRRWT96 ;
			   by DOEID DIVISION;
run;

data brrwt_long_c;
 set brrwt_long_c;
 repl = _NAME_;
 wt = COL1;
 keep DOEID DIVISION repl wt;
run;

/* Join home type to weights: -------------------------------------------------- */
data temp_c;
 merge temp_c brrwt_long_c;
 by DOEID;
 night_temp_est=TEMPNITE*NWEIGHT;
 night_temp_repl=TEMPNITE*wt;
 home_temp_est=TEMPHOME*NWEIGHT;
 home_temp_repl=TEMPHOME*wt;
 gone_temp_est=TEMPGONE*NWEIGHT;
 gone_temp_repl=TEMPGONE*wt;
run;

/* use the summary to calc the sum of variable, need to drop ovrall summary*/
proc summary data=temp_c;
 class DIVISION repl;
 output out=temp_sum_c
 sum(night_temp_est) = sum_night_temp_est
 sum(wt) = sum_wt_repl
 sum(night_temp_repl) = sum_night_temp_repl
 sum(NWEIGHT) = sum_nwt
 sum(home_temp_est) = sum_home_temp_est
 sum(home_temp_repl) = sum_home_temp_repl
 sum(gone_temp_est) = sum_gone_temp_est
 sum(gone_temp_repl) = sum_gone_temp_repl;
run;

/* calc estimation and sse */
data temp_c;
 set temp_sum_c;
 if (_TYPE_ = 3);
 night_temp_est=sum_night_temp_est/sum_nwt;
 night_temp_repl=sum_night_temp_repl/sum_wt_repl;
 home_temp_est=sum_home_temp_est/sum_nwt;
 home_temp_repl=sum_home_temp_repl/sum_wt_repl;
 gone_temp_est=sum_gone_temp_est/sum_nwt;
 gone_temp_repl=sum_gone_temp_repl/sum_wt_repl;
 std_err_night = (4*(night_temp_est-night_temp_repl)**2);
 std_err_home = (4*(home_temp_est-home_temp_repl)**2);
 std_err_gone = (4*(gone_temp_est-gone_temp_repl)**2);
run;

/* average the error */
proc summary data=temp_c;
 class DIVISION;
 output out=temp_c
 mean(night_temp_est) = night_temp_est
 mean(std_err_night) = std_err_night
 mean(home_temp_est) = home_temp_est
 mean(std_err_home) = std_err_home
 mean(gone_temp_est) = gone_temp_est
 mean(std_err_gone) = std_err_gone;
run;

/* calc CI */
data temp_c;
 set temp_c;
 night_lwr = night_temp_est + quantile('NORMAL',0.025)*std_err_night**0.5;
 night_upr = night_temp_est + quantile('NORMAL',0.975)*std_err_night**0.5;
 home_lwr = home_temp_est + quantile('NORMAL',0.025)*std_err_home**0.5;
 home_upr = home_temp_est + quantile('NORMAL',0.975)*std_err_home**0.5;
 gone_lwr = gone_temp_est + quantile('NORMAL',0.025)*std_err_gone**0.5;
 gone_upr = gone_temp_est + quantile('NORMAL',0.975)*std_err_gone**0.5;
 if (_TYPE_ ~= 0);
 drop _TYPE_ _FREQ_;
run;

/* decoding the division code */
data names;
 input DIVISION names$;
 cards;
  1 New_England
  2 Middle_Atlantic
  3 East_North_Central
  4 West_North_Central
  5 South_Atlantic
  6 East_South_Central
  7 West_South_Central
  8 Mountain_North
  9 Mountain_South
  10 Pacific
;

data temp_c;
 merge names temp_c;
 by DIVISION;
 drop DIVISION std_err_home std_err_night std_err_gone;
 rename names=DIVISION;
run;

/* output the result */
proc print data=temp_c;
 title 'national average home temperature by census division';
run;

proc export data=temp_c
 outfile='/folders/myfolders/Problem Set 6/ps6_q2c.csv' replace;
run;





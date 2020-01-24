/* -------------------------------------------------------------------------- *
 * Stats 506 Problem Set 6 Question 3.
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

/* b. Estiamte the national average home temperature at night, among homes */
/* that use space heating. */

proc sql;
 create table temp as select DOEID,TEMPNITE,HEATHOME,NWEIGHT
 from mylib.recs;
 create table bt as select *
 from mylib.brrwt_long;
 /*  diff from in q2, we can first merge the whole set and then drop the uncessary point */
 create table temp_b as
 select temp.DOEID,temp.TEMPNITE,temp.HEATHOME,temp.NWEIGHT,bt.repl,bt.wt from bt left join
  (select * from temp)
  on temp.DOEID = bt.DOEID
 where HEATHOME = 1 and not TEMPNITE = -2;
/*   calc the est and stderr */
  create table temp_b_re as
  select mean(temp_est) as temp_est,
  		 (mean(4*(temp_est - temp_repl)**2))**0.5 as std_err
  from (select sum(TEMPNITE*NWEIGHT)/sum(NWEIGHT) as temp_est,
               sum(TEMPNITE*wt)/sum(wt) as temp_repl
        from temp_b
        group by repl
  );
/*   calc CI */
  create table temp_b_re as
	select temp_est, temp_est+quantile('normal',0.025)*std_err as CI_lower,
                  temp_est+quantile('normal',0.975)*std_err as CI_upper
	from temp_b_re;

quit;
run;

/* output the result */
proc print data=temp_b_re;
 title 'national average home temperature at night';
run;

proc export data=temp_b_re
 outfile='/folders/myfolders/Problem Set 6/ps6_q3b.csv' replace;
run;

/* c. Next, by census division, estimate the average winter home
temperatures at night, during the day with someone home, and during
the day with no one home (when applicable) */
proc sql;
 create table temp_c_ini as select DOEID,TEMPNITE,TEMPHOME,TEMPGONE,NWEIGHT,DIVISION
 from mylib.recs;
 create table bt as select *
 from mylib.brrwt_long;
 /*  diff from in q2, we can first merge the whole set and then drop the uncessary point */
 create table temp_c as
 select tc.DIVISION,tc.DOEID,tc.TEMPNITE,tc.TEMPHOME,tc.TEMPGONE,tc.NWEIGHT,bt.repl,bt.wt from bt left join
  (select * from temp_c_ini tc)
  on tc.DOEID = bt.DOEID
  where not TEMPNITE = -2 and not TEMPHOME = -2 and not TEMPGONE = -2;

  /*   calc the est and stderr */
  create table temp_c_re as
  select DIVISION,
 	 	   mean(night_temp_est) as night_temp_est,
  		 (mean(4*(night_temp_est - night_temp_repl)**2))**0.5 as night_std_err,
  		 mean(home_temp_est) as home_temp_est,
  		 (mean(4*(home_temp_est - home_temp_repl)**2))**0.5 as home_std_err,
  		 mean(gone_temp_est) as gone_temp_est,
  		 (mean(4*(gone_temp_est - gone_temp_repl)**2))**0.5 as gone_std_err
  from (select sum(TEMPNITE*NWEIGHT)/sum(NWEIGHT) as night_temp_est,
        	     sum(TEMPNITE*wt)/sum(wt) as night_temp_repl,
        	     sum(TEMPHOME*NWEIGHT)/sum(NWEIGHT) as home_temp_est,
        	     sum(TEMPHOME*wt)/sum(wt) as home_temp_repl,
        	     sum(TEMPGONE*NWEIGHT)/sum(NWEIGHT) as gone_temp_est,
        	     sum(TEMPGONE*wt)/sum(wt) as gone_temp_repl,
        	     DIVISION
        from temp_c
        group by DIVISION,repl
  )
  group by DIVISION;
/*   calc CI */
  create table temp_c_re as
	select night_temp_est, night_temp_est+quantile('normal',0.025)*night_std_err as night_lower,
         night_temp_est+quantile('normal',0.975)*night_std_err as night_upper,
         home_temp_est, home_temp_est+quantile('normal',0.025)*home_std_err as home_lower,
         home_temp_est+quantile('normal',0.975)*home_std_err as home_upper,
         gone_temp_est, gone_temp_est+quantile('normal',0.025)*gone_std_err as gone_lower,
         gone_temp_est+quantile('normal',0.975)*gone_std_err as gone_upper,
         DIVISION
	from temp_c_re;
/* send decoding information  */
  alter table temp_c_re add division_name char(20);
  update temp_c_re set division_name = 'New_England' WHERE DIVISION = 1;
  update temp_c_re set division_name = 'Middle_Atlantic' WHERE DIVISION = 2;
  update temp_c_re set division_name = 'East_North_Central' WHERE DIVISION = 3;
  update temp_c_re set division_name = 'West_North_Central' WHERE DIVISION = 4;
  update temp_c_re set division_name = 'South_Atlantic' WHERE DIVISION = 5;
  update temp_c_re set division_name = 'East_South_Central' WHERE DIVISION = 6;
  update temp_c_re set division_name = 'West_South_Central' WHERE DIVISION = 7;
  update temp_c_re set division_name = 'Mountain_North' WHERE DIVISION = 8;
  update temp_c_re set division_name = 'Mountain_South' WHERE DIVISION = 9;
  update temp_c_re set division_name = 'Pacific' WHERE DIVISION = 10;
  alter table temp_c_re drop division;

quit;
run;

/* output the result */
proc print data=temp_c_re;
 title 'national average home temperature by census division';
run;

proc export data=temp_c_re
 outfile='/folders/myfolders/Problem Set 6/ps6_q3c.csv' replace;
run;































/* -------------------------------------------------------------------------- *
 *
 * SAS code for Problem set 6, Question 2.
 * 
 * Author: Huayu Li
 * Updated: Dec 4, 2019
 * -------------------------------------------------------------------------- *
*/
/* 80: ---------------------------------------------------------------------- */

/* To begin with, set the working directory first. */
libname dir '~/STATS506/PS6/';
%let path=~/STATS506/PS6/;

/* Here we will directly import the dataset. */
proc import datafile="&path.recs2015_public_v4.csv" out=dir.r2015;
run;

/** Question 2-(a) **/
proc transpose data=dir.r2015 out=recs2015 prefix=BRRWT;
by DOEID NWEIGHT;
var BRRWT1-BRRWT96;

data recs2015;
set recs2015 (rename=(BRRWT1=weight_value));
Weight=_name_;
drop _name_;

/* Now save this weight dataset. */
data dir.brrwt_long;
set recs2015; 
run;

/** Question 2-(b) **/
data TEMP;
set dir.r2015;
if HEATHOME = 1 && TEMPNITE ~= -2;
keep DOEID TEMPNITE NWEIGHT BRRWT1-BRRWT96;
run;

proc transpose data=TEMP out=TEMPatNIGHT prefix=BRRWT;
by DOEID NWEIGHT TEMPNITE;
var BRRWT1-BRRWT96;

data TEMPatNIGHT;
set TEMPatNIGHT;
weight_value=BRRWT1;
Weight=_NAME_;
drop BRRWT1 _NAME_;
NTEMP=TEMPNITE*NWEIGHT;
RTEMP=TEMPNITE*weight_value;
run;

proc summary;
class Weight;
output out=T1
sum(NTEMP)=sum_NTEMP
sum(RTEMP)=sum_RTEMP
sum(NWEIGHT)=sum_nweight
sum(weight_value)=sum_rweight;
run;


data T2;
set T1;
if _TYPE_ = 1;
est_tempnite=sum_NTEMP/sum_nweight;
brr_tempnite=sum_RTEMP/sum_rweight;
drop Weight _TYPE_ _FREQ_ sum_NTEMP sum_RTEMP sum_nweight sum_rweight;
sq_err=(est_tempnite-brr_tempnite)**2;
drop brr_tempnite;

proc summary;
output out=Final_mtemp
sum(sq_err)=SSE
/* Note that for est_tempnite we use the mean value to summarize;
 * that is because the est_tempnite is constant. */
mean(est_tempnite)=Mean_Tempnite;
run;

/* Dataset Final is the result for this question. */
data Final_mtemp;
set Final_mtemp;
drop _TYPE_ _FREQ_;
SE = (SSE/24)**0.5;
drop SSE;
LOWER_CI=Mean_tempnite+quantile('NORMAL',0.025)*SE;
UPPER_CI=Mean_tempnite+quantile('NORMAL',0.975)*SE;
drop SE;
run;

proc print data=Final_mtemp;
run;

proc export data=Final_mtemp
outfile='/folders/myfolders/STATS506/PS6/PS6_q2b.csv' replace;
run;

/** Question 2-(c) **/
data DIV;
set dir.r2015;
if TEMPNITE ~= -2 & TEMPHOME ~= -2 & TEMPGONE ~= -2;
keep DOEID DIVISION TEMPNITE TEMPHOME TEMPGONE NWEIGHT BRRWT1-BRRWT96;
run;

proc transpose data=DIV out=TEMPbyDIV prefix=BRRWT;
by DOEID DIVISION TEMPNITE TEMPHOME TEMPGONE NWEIGHT;
var BRRWT1-BRRWT96;

data TEMPbyDIV;
set TEMPbyDIV;
weight_value=BRRWT1;
Weight = _NAME_ ;
drop BRRWT1 _NAME_ DOEID;
n_night=TEMPNITE*NWEIGHT;
r_night=TEMPNITE*weight_value;
n_home=TEMPHOME*NWEIGHT;
r_home=TEMPHOME*weight_value;
n_gone=TEMPGONE*NWEIGHT;
r_gone=TEMPGONE*weight_value;
drop TEMPNITE TEMPHOME TEMPGONE DOEID;

proc summary;
class Weight DIVISION;
output out=D1
sum(n_night)=sum_NNITE
sum(r_night)=sum_RNITE
sum(n_home)=sum_NHOME
sum(r_home)=sum_RHOME
sum(n_gone)=sum_NGONE
sum(r_gone)=sum_RGONE
sum(NWEIGHT)=sum_nweight
sum(weight_value)=sum_rweight;
run;

data D1;
set D1;
if _TYPE_ = 3;
est_nnite=sum_NNITE/sum_nweight;
est_rnite=sum_RNITE/sum_rweight;
est_nhome=sum_NHOME/sum_nweight;
est_rhome=sum_RHOME/sum_rweight;
est_ngone=sum_NGONE/sum_nweight;
est_rgone=sum_RGONE/sum_rweight;
drop _FREQ_ _TYPE_ sum_NNITE sum_RNITE sum_NHOME 
sum_RHOME sum_NGONE sum_RGONE sum_nweight sum_rweight;
sqerr_nite=(est_rnite-est_nnite)**2;
sqerr_home=(est_rhome-est_nhome)**2;
sqerr_gone=(est_rgone-est_ngone)**2;
drop est_rnite est_rhome est_rgone;

proc summary;
class DIVISION;
output out=D2
sum(sqerr_nite)=sse_nite
sum(sqerr_home)=sse_home
sum(sqerr_gone)=sse_gone
mean(est_nnite)=avg_night
mean(est_nhome)=avg_home
mean(est_ngone)=avg_gone;
run;

data D2;
set D2;
if _type_ = 1;
drop _type_ _freq_;
se_nite=(sse_nite/24)**0.5;
se_home=(sse_home/24)**0.5;
se_gone=(sse_gone/24)**0.5;
drop sse_nite sse_home sse_gone;

/* Here we will separate the dataset and combine by row. */
data part_NITE;
set D2;
timetype='night';
se=se_nite;
avg_temp=avg_night;
keep DIVISION timetype avg_temp se;

data part_HOME;
set D2;
timetype='home';
se=se_home;
avg_temp=avg_home;
keep DIVISION timetype avg_temp se;

data part_GONE;
set D2;
timetype='gone';
se=se_gone;
avg_temp=avg_gone;
keep DIVISION timetype avg_temp se;

Data Final_division;
set part_NITE part_HOME part_GONE;
lower_CI=avg_temp+quantile('normal',0.025)*se;
upper_CI=avg_temp+quantile('normal',0.975)*se;
drop se;
proc sort data=Final_division;
by DIVISION;

/* Finally, change the name of DIVISION; then we get the final result. */

data Divname;
input DIVISION divnames$;
cards;
1 NewEngland
2 MiddleAtlantic
3 EastNorthCentral
4 WestNorthCentral
5 SouthAtlantic
6 EastSouthCentral
7 WestSouthCentral
8 MountainNorth
9 MountainSouth
10 Pacific
;

data Final_division;
merge Final_division Divname;
by DIVISION;
drop DIVISION;
rename divnames=DIVISION;

proc sort data=Final_division;
by timetype;
run;

/* Output the result */
proc print data=Final_division;
run;

proc export data=Final_division
outfile='/folders/myfolders/STATS506/PS6/PS6_q2c.csv' replace;
run;


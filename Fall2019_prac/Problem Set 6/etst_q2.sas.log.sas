/* -------------------------------------------------------------------------- *
 *
 * SAS code for Problem set 6, Question 3.
 * 
 * Author: Huayu Li
 * Updated: Dec 4, 2019
 * -------------------------------------------------------------------------- *
*/
/* 80: ---------------------------------------------------------------------- */

/* To begin with, set the working directory first. */
libname dir '/folders/myfolders/Problem Set 6/';
%let path=/folders/myfolders/Problem Set 6/;

/* Here we will directly import the dataset. */
proc import datafile="&path.recs2015_public_v4.csv" out=dir.r2015;
run;

/* Now let's start */

/** For Question 2-(b) **/
proc sql;

create table recs as
select DOEID,TEMPNITE,HEATHOME,NWEIGHT
from dir.r2015;

create table wt as
select * from dir.brrwt_long;

create table TEMPDATA as
select r.TEMPNITE,r.HEATHOME,r.NWEIGHT,wt.weight_value,wt.WEIGHT
from recs r
right join wt
on wt.DOEID = r.DOEID
where HEATHOME = 1 and not TEMPNITE = -2;

create table AVG_TEMP as
select mean(TEMPNITE*NWEIGHT)/mean(NWEIGHT) as nmtemp,
       mean(TEMPNITE*weight_value)/mean(weight_value) as rmtemp
from TEMPDATA
group by Weight;

create table SE_TEMP as
select mean(nmtemp) as mean_temp, (sum((nmtemp-rmtemp)**2)/24)**0.5 as se_temp
from AVG_TEMP;

create table Final_mtemp as
select mean_temp, mean_temp+quantile('normal',0.025)*se_temp as Lower_CI,
                  mean_temp+quantile('normal',0.975)*se_temp as Upper_CI
from SE_TEMP;

quit;
run;

/* Output the final result */
proc print data=Final_mtemp;
run;


/** For Question 2-(c) **/
proc sql;

create table DIVNAME
(DIVISION num,
divname char(100));

insert into DIVNAME (DIVISION,divname)
values(1,'New England')
values(2,'Middle Atlantic')
values(3,'East North Central')
values(4,'West North Central')
values(5,'South Atlantic')
values(6,'East South Central')
values(7,'West South Central')
values(8,'Mountain North')
values(9,'Mountain South')
values(10,'Pacific');

create table recs as
select DOEID,DIVISION,TEMPNITE,TEMPHOME,TEMPGONE,NWEIGHT from dir.r2015;

create table wt as
select * from dir.brrwt_long;

create table DIV as
select r.DIVISION,r.TEMPNITE,r.TEMPHOME,r.TEMPGONE,
       r.NWEIGHT,wt.weight_value,wt.WEIGHT
from recs r
right join wt 
on r.DOEID = wt.DOEID
where not TEMPNITE = -2 and not TEMPHOME = -2 and not TEMPGONE = -2;

create table C_TEMP as
select DIVISION,Weight,sum(TEMPNITE*NWEIGHT)/sum(NWEIGHT) as m_nnite,
       sum(TEMPNITE*weight_value)/sum(weight_value) as m_rnite,
       sum(TEMPHOME*NWEIGHT)/sum(NWEIGHT) as m_nhome,
       sum(TEMPHOME*weight_value)/sum(weight_value) as m_rhome,
       sum(TEMPGONE*NWEIGHT)/sum(NWEIGHT) as m_ngone,
       sum(TEMPGONE*weight_value)/sum(weight_value) as m_rgone
from DIV
group by DIVISION,Weight;

create table SE_DTEMP as
select DIVISION,mean(m_nnite) as avg_night,
       (sum((m_nnite-m_rnite)**2)/24)**0.5 as se_nite,
       mean(m_nhome) as avg_home,
       (sum((m_nhome-m_rhome)**2)/24)**0.5 as se_home,
       mean(m_ngone) as avg_gone,
       (sum((m_ngone-m_rgone)**2)/24)**0.5 as se_gone
from C_TEMP
group by DIVISION;

create table AVG_NITE as
select DIVISION,'night' as timetype, avg_night as avg_temp,
       avg_night+quantile('normal',0.025)*se_nite as Lower_CI,
       avg_night+quantile('normal',0.975)*se_nite as Upper_CI
from SE_DTEMP;

create table AVG_HOME as
select DIVISION,'home' as timetype, avg_home as avg_temp,
       avg_home+quantile('normal',0.025)*se_home as Lower_CI,
       avg_home+quantile('normal',0.975)*se_home as Upper_CI
from SE_DTEMP;

create table AVG_GONE as
select DIVISION,'gone' as timetype, avg_gone as avg_temp,
       avg_gone+quantile('normal',0.025)*se_gone as Lower_CI,
       avg_gone+quantile('normal',0.975)*se_gone as Upper_CI
from SE_DTEMP;

create table AVG_TEMP as
select * from AVG_NITE
union
select * from AVG_HOME
union
select * from AVG_GONE;

create table Final_division as
select dn.divname,avg.timetype,avg.avg_temp,avg.Lower_CI,avg.Upper_CI
from AVG_TEMP avg
right join DIVNAME dn
on dn.DIVISION=avg.DIVISION
order by DIVNAME;
quit;
run;

/* Output the final result */
proc print data=Final_DIVISION;
run;




// https://wwwn.cdc.gov/Nchs/Nhanes/2005-2006/DEMO_D.XPT
// https://wwwn.cdc.gov/Nchs/Nhanes/2005-2006/DR1IFF_D.XPT
// https://wwwn.cdc.gov/Nchs/Nhanes/2005-2006/DR2IFF_D.XPT

*------------------------------------------------------------------------------*
* Stats 506, F19 Problem Set 4
*
* Author: Sijun Zhang
* Date:   Nov 15, 2019
*------------------------------------------------------------------------------*
* 80: --------------------------------------------------------------------------

* Script Setup: ----------------------------------------------------------------
version 16.0									// Stata version used
log using ps4_q3.log, text replace	 	   		// Generate a log
cd "C:\Users\Randyzhang\OneDrive\SYNC_FOLDER\STATS506\Fall2019_prac\Problem Set 4"

clear					// Start clean

* Part a  : ----------------------------------------------------------------

import sasxport5 DEMO_D, clear
keep seqn riagendr ridageyr indfmpir ridexmon
// find if it in winter
generate if_winter = ridexmon
generate age = ridageyr
generate gender = riagendr
generate pir = indfmpir
drop riagendr ridageyr indfmpir ridexmon
save demo_d.dta, replace

import sasxport5 DR1TOT_D, clear

keep seqn dr1day dr1_320z
// find if it was weekday
generate in_weekday1 = 1 if dr1day <= 6 & dr1day >=2
replace in_weekday1 = 0 if dr1day == 7 | dr1day == 1
// find if the responent has drank water
generate drank_water1 = 0 if dr1_320z == 0
replace drank_water1 = 1 if dr1_320z > 0
replace drank_water1 = . if dr1_320z == .
drop dr1_320z
save dr1tot_d.dta, replace

import sasxport5 DR2TOT_D, clear
keep seqn dr2day dr2_320z
// find if it was weekday
generate in_weekday2 = 1 if dr2day <= 6 & dr2day >=2
replace in_weekday2 = 0 if dr2day == 7 | dr2day == 1
// find if the responent has drank water
generate drank_water2 = 0 if dr2_320z == 0
replace drank_water = 1 if dr2_320z > 0
replace drank_water2 = . if dr2_320z == .
drop dr2_320z
merge m:1 seqn using dr1tot_d.dta
drop _merge

generate drday1 = dr1day
generate drday2 = dr2day
drop dr1day dr2day
reshape long in_weekday drank_water drday, i(seqn) j(svy_day)

merge m:1 seqn using demo_d.dta
drop _merge

label define winter_i 1 "in winter" 2 "not in winter"
label define drank_water_i 1 "has drank water" 0 "not drank water"
label define week_i 1 "weekday" 0 "weekend"
label define gender_i 1 "Male" 2 "Female"

label values in_weekday week_i
label values drank_water drank_water_i
label values if_winter winter_i
label values gender gender_i


export delimited ps4_q3_a_df.csv, replace

* Part b  : ----------------------------------------------------------------
generate missing = 0
replace missing = 1 if in_weekday == . | drank_water == . | age == . | gender == . | pir == . | if_winter == .

generate fid = 0
save ps4_q3_df_missing.dta, replace

// calc the mean of pir and age
drop if missing == 1
collapse (mean) pir age
generate mean_pir = pir
generate mean_age = age
drop pir age
export delimited mean_pir_age.csv, replace

generate fid = 0
merge 1:m fid using ps4_q3_df_missing.dta

// Centering and decading to as it is continuous
replace age = (age - mean_age) / 10
replace pir = pir - mean_pir

drop mean_age mean_pir _merge

export delimited ps4_q3_b_df.csv, replace

* Part c  : ----------------------------------------------------------------
save ps4_q3_log_df.dta, replace
// only use day 1 data
drop if svy_day == 2 | svy_day == .
drop if missing == 1
logistic drank_water in_weekday if_winter c.age##c.age gender c.pir
matrix b1 = e(b)
matrix v1 = e(V)

margins, dydx(*)
matrix b2 = r(b)
matrix v2 = r(V)

mata
    b1_m = st_matrix("b1")
    b1_m = b1_m[1],b1_m[2],b1_m[3],b1_m[5],b1_m[6],b1_m[4]
    v1_m = st_matrix("v1")
    v1_m = diagonal(v1_m)'
    v1_m = v1_m[1],v1_m[2],v1_m[3],v1_m[5],v1_m[6],b1_m[4]
    b2_m = st_matrix("b2"),(.)
    v2_m = st_matrix("v2")
    v2_m = diagonal(v2_m)',(.)

    a1 = exp(b1_m - 1.96*sqrt(v1_m))
    b1 = exp(b1_m + 1.96*sqrt(v1_m))
    re1 = a1\b1

    a2 = (b2_m - 1.96*sqrt(v2_m))
    b2 = (b2_m + 1.96*sqrt(v2_m))
    re2 = a2\b2

    re = exp(b1_m)\re1\b2_m\re2

    st_matrix("re_c",re)

end

matrix rownames re_c=Odds_ratios Odds_ratios_lwr Odds_ratios_upr Marginal_effect Marginal_effect_lwr Marginal_effect_upr
matrix colnames re_c=in_weekday if_winter age gender pir age^2

putexcel set ps4_q3_c.xls, replace
putexcel A1 = matrix(re_c), names

* Part d  : ----------------------------------------------------------------
use ps4_q3_log_df, clear
drop if missing == 1
meglm drank_water in_weekday if_winter c.age##c.age gender c.pir ||seqn:, family(binomial) link(logit)
matrix b1 = e(b)
matrix v1 = e(V)

margins, dydx(*)
matrix b2 = r(b)
matrix v2 = r(V)

mata
    b1_m = st_matrix("b1")
    b1_m = b1_m[1],b1_m[2],b1_m[3],b1_m[5],b1_m[6],b1_m[4]
    v1_m = st_matrix("v1")
    v1_m = diagonal(v1_m)'
    v1_m = v1_m[1],v1_m[2],v1_m[3],v1_m[5],v1_m[6],v1_m[4]
    b2_m = st_matrix("b2"),(.)
    v2_m = st_matrix("v2")
    v2_m = diagonal(v2_m)',(.)

    a1 = (b1_m - 1.96*sqrt(v1_m))
    b1 = (b1_m + 1.96*sqrt(v1_m))
    re1 = exp(a1)\exp(b1)
    re3 = a1\b1

    a2 = (b2_m - 1.96*sqrt(v2_m))
    b2 = (b2_m + 1.96*sqrt(v2_m))
    re2 = a2\b2

    re = exp(b1_m)\re1\b2_m\re2\b1_m\re3

    st_matrix("re_d",re)

end

matrix rownames re_d=Odds_ratios Odds_ratios_lwr Odds_ratios_upr Marginal_effect Marginal_effect_lwr Marginal_effect_upr Coefficients Coefficients_lwr Coefficients_lwr
matrix colnames re_d=in_weekday if_winter age gender pir age^2

putexcel set ps4_q3_d.xls, replace
putexcel A1 = matrix(re_d), names

* Script Cleanup: --------------------------------------------------------------
log close
exit

* 80: --------------------------------------------------------------------------

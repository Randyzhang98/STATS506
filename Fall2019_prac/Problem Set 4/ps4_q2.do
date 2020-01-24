*------------------------------------------------------------------------------*
* Stats 506, F19 Problem Set 4
*
* Data: recs2015_public_v4.CSV
*
* Author: Sijun Zhang
* Date:   Nov 17, 2019
*------------------------------------------------------------------------------*
* 80: --------------------------------------------------------------------------

* Script Setup: ----------------------------------------------------------------
version 16.0									// Stata version used
log using ps4_q2.log, text replace	 	   		// Generate a log
cd "C:\Users\Randyzhang\OneDrive\SYNC_FOLDER\STATS506\Fall2019_prac\Problem Set 4"

clear					// Start clean

* Data Prep: -------------------------------------------------------------------
// data prep
*? Maybe tidy up over-long line with locals?
local recs_file "recs2015_public_v4.csv"
local recs_url = "https://www.eia.gov/consumption/residential/data/2015/csv/"
capture confirm file `recs_file'
if _rc==0 {
  display "Loading local file"
  import delimited `recs_file', clear
}
else {
  display "Download file and create local copy"
  import delimited `recs_url'`recs_file', clear
  export delimited `recs_file'
}

encode uatyp10, generate(uatyp10_i)
replace uatyp10_i = 1 if uatyp10_i==3

label define division_i 1 "New England" 2 "Middle Atlantic" 3 "East North Central" 4 "West North Central" 5 "South Atlantic" 6 "East South Central" 7 "West South Central" 8 "Mountain North" 9 "Mountain South" 10 "Pacific"
label values division division_i

preserve

local vars = "uatyp10_i division internet"
keep doeid nweight `vars'
save recs2015_prop_est.dta, replace

// in order to merge the nweight and brrwt values
restore
keep doeid brrwt1-brrwt96 `vars'
reshape long brrwt, i(doeid) j(repl)
merge m:1 doeid using recs2015_prop_est.dta

// calc the proportion of homes with internet
generate brrwt_with_internet = internet*brrwt
generate nw_with_internet = internet*nweight
collapse (sum) brrwt brrwt_with_internet nweight nw_with_internet, by(division uatyp10_i repl)
generate prop_repl = brrwt_with_internet/brrwt
generate prop = nw_with_internet/nweight
drop brrwt brrwt_with_internet
drop nweight nw_with_internet

// calc the confidence interval using brrwt replications
reshape wide prop_repl prop,i(division repl) j(uatyp10_i)
generate diff_prop = prop1-prop2
generate diff_prop_repl = prop_repl1 - prop_repl2
generate rsq_prop1 = (prop1-prop_repl1)^2/(1-0.5)^2
generate rsq_prop2 = (prop2-prop_repl2)^2/(1-0.5)^2
generate rsq_prop_diff = (diff_prop-diff_prop_repl)^2/(1-0.5)^2
collapse (mean) prop1 prop2 diff_prop rsq_prop1 rsq_prop2 rsq_prop_diff , by(division)

generate diff_prop_lwr = diff_prop - 1.96*sqrt(rsq_prop_diff)
generate diff_prop_upr = diff_prop + 1.96*sqrt(rsq_prop_diff)

rename rsq_prop1 stderr_prop1
replace stderr_prop1 = sqrt(stderr_prop1)
rename rsq_prop2 stderr_prop2
replace stderr_prop2 = sqrt(stderr_prop2)
rename rsq_prop_diff stderr_diff_prop
replace stderr_diff_prop = sqrt(stderr_diff_prop)
export delimited recs_prop_with_internet.csv, replace

* Script Cleanup: --------------------------------------------------------------
log close
exit

* 80: --------------------------------------------------------------------------
*------------------------------------------------------------------------------*
* Stats 506, F19 Problem Set 4
*
* Data: df_ps4_q1.csv
* Source: Output from Problem Set 2 Question 2 c
*
* Author: Sijun Zhang
* Date:   Nov 15, 2019
*------------------------------------------------------------------------------*
* 80: --------------------------------------------------------------------------

* Script Setup: ----------------------------------------------------------------
version 16.0									// Stata version used
log using ps4_q1.log, text replace	 	   		// Generate a log
cd "C:\Users\Randyzhang\OneDrive\SYNC_FOLDER\STATS506\Fall2019_prac\Problem Set 4"

clear					// Start clean

* Data Prep: -------------------------------------------------------------------
// import delimited mousetrap_data.csv
// The data is downloaded from github Stats506_F19
// https://github.com/jbhender/Stats506_F19/blob/master/problem_sets/data/mousetrap_data.csv

local mouse_file "mousetrap_data.csv"
local mouse_url = "https://github.com/jbhender/Stats506_F19/blob/master/problem_sets/data"
capture confirm file `mouse_file'
if _rc==0 {
  display "Loading local file"
  import delimited `mouse_file', clear
}
else {
  display "Download file and create local copy"
  import delimited `mouse_url'`mouse_file', clear
  export delimited `mouse_file'
}

replace auc = max(auc,1)

replace tot_dist = log(tot_dist)
replace max_abs_dev = log(max_abs_dev)
replace avg_abs_dev = log(avg_abs_dev)
replace auc = log(auc)

encode condition, generate(condition_i)
replace condition_i = (-1)*condition_i+2
encode exemplar, generate(exemplar_i)

mixed tot_dist condition_i ||_all:R.subject_nr ||_all:R.exemplar
matrix b1 = e(b)
matrix v1 = e(V)

mixed max_abs_dev condition_i ||_all:R.subject_nr ||_all:R.exemplar
matrix b2 = e(b)
matrix v2 = e(V)

mixed avg_abs_dev condition_i ||_all:R.subject_nr ||_all:R.exemplar
matrix b3 = e(b)
matrix v3 = e(V)

mixed auc condition_i ||_all:R.subject_nr ||_all:R.exemplar
matrix b4 = e(b)
matrix v4 = e(V)

mata
b1_mata = st_matrix("b1")
b2_mata = st_matrix("b2")
b3_mata = st_matrix("b3")
b4_mata = st_matrix("b4")

v1_mata = st_matrix("v1")
v2_mata = st_matrix("v2")
v3_mata = st_matrix("v3")
v4_mata = st_matrix("v4")

a1 = (exp(b1_mata[1,1]), exp(b1_mata[1,1]-sqrt(v1_mata[1,1])*1.96),  exp(b1_mata[1,1]+sqrt(v1_mata[1,1])*1.96))
a2 = (exp(b2_mata[1,1]), exp(b2_mata[1,1]-sqrt(v2_mata[1,1])*1.96),  exp(b2_mata[1,1]+sqrt(v2_mata[1,1])*1.96))
a3 = (exp(b3_mata[1,1]), exp(b3_mata[1,1]-sqrt(v3_mata[1,1])*1.96),  exp(b3_mata[1,1]+sqrt(v3_mata[1,1])*1.96))
a4 = (exp(b4_mata[1,1]), exp(b4_mata[1,1]-sqrt(v4_mata[1,1])*1.96),  exp(b4_mata[1,1]+sqrt(v4_mata[1,1])*1.96))

a = a1\a2\a3\a4

st_matrix("re", a)

end
matrix rownames re=Total_Distance Maximum_Absolute_Deviation Average_Absolute_Deviation AUC
matrix colnames re=Relative_effect_est Relative_effect_lwr Relative_effect_upr

putexcel set ps4_q1.xls, replace
putexcel A1 = matrix(re), names

* Script Cleanup: --------------------------------------------------------------
log close
exit

* 80: --------------------------------------------------------------------------
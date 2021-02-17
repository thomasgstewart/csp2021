** Need rms package
* Recommended approach to install: 
* net install github, from("https://haghish.github.io/github/")
* gitget rms

*************************
* Framingham Heart Study, Dupont example 3.11, page 102
* Dataset: Levy (1999)
* N observations: 4699
* Response: Log Systolic Blood Pressure
* Predictors: log(bmi), age, sex, log(serum cholesterol)
*************************

getvdata 2.20.Framingham

generate logsbp = log(sbp)
label variable logsbp "Log Systolic Blood Pressure"
generate logbmi = log(bmi)
label variable logbmi "Log Body Mass Index"
generate logscl = log(scl)
label variable logscl "Log Serum Cholesterol"

label define sexlabel 1 "male" 2 "female" 
label values sex sexlabel

quietly regress logsbp i.sex##c.(logbmi age logscl)

* R2 Bootstrap VALIDATION
validate_regress, reps(100) seed(3243)

* In-sample calibration plot
regress_yhat_ci

twoway (scatter logsbp logsbp_hat) ///
 (line logsbp_hat logsbp_hat, sort) ///
 (lowess logsbp logsbp_hat)

**************************************************
*********** MICROECONOMETRICS PS1 ****************
********* Sauchanka Giannotti Acero **************
**************************************************

/*                                  Setup                                */


clear all

global ps2 "C:\Users\Xvast\OneDrive - Alma Mater Studiorum Università di Bologna\Master Unibo\microeconometrics\PS2"

use "$ps2\PS22.dta", clear

*Run once to create the folder
*mkdir "$ps2\tex_outputs"
*mkdir "$ps2\graph_folder"

*Tex folder ouputs
global tex_folder "$ps2\tex_outputs"
*Graph folder output
global graphs "$ps2\graph_folder"

*policy objective -> Improve voters learning
*policy: Upscale gpt app.


*Labeling
label variable empl "Employment status"
label variable age "Age in years"
label variable educ "Years of education"
label variable party "Political view"
label variable app "Hours using the app"
label variable fem "Sex"
label variable enc "Encouraged dummy"
label variable pol_kn0 "Base political knowledge"
label variable pol_kn "After political knowledge"

*Labels
label define enclbl 0 "Not encouraged" 1 "Encouraged"
label values enc enclbl

*Ordering
order id  age educ pol_kn0 pol_kn app fem party empl enc 

/*                                  Question 1                                */


**1 - Basic summary 

estpost sum,detail
esttab using "$tex_folder\point 1.1.tex", cells("mean(label(Mean)fmt(%9.2f)) sd(label(S.Deviation)fmt(%9.2f)) min(label(Min.)fmt(%9.2f)) p50(label(Median)fmt(%9.2f)) max(label(Max.)fmt(%9.2f)) ") replace label nomtitle nonumber 

**2 - Comparison between group means.

*ssc install ietoolkit

iebaltab age educ pol_kn app fem party empl, grpvar(enc) savetex("$tex_folder\point 1.2.tex") texdoc ftest  onerow replace


/*                                  Question 2                                */


**1 - Basic OLS
reg pol_kn app age educ fem party empl pol_kn0 enc,r

twoway (scatter pol_kn app, mcolor("161 225 225")) /// 
(lfit pol_kn app,lcolor("18 100 138") lwidth(0.41)) ///
(lfit pol_kn app if enc==1, lpattern(dash) lcolor("18 100 138") lwidth(0.21)) ///
(lfit pol_kn app if enc==0, lpattern(dash) lcolor("18 100 138") lwidth(0.21)), ///
 xtitle("Usage of the app in hours") ytitle("Political knowledge score") ///
 legend(off)

graph export "$graphs\Point2.1.png", as(png) name("Graph") replace

**3 - IVregression

eststo clear
ivregress 2sls pol_kn age educ fem party empl (app=enc), robust
eststo ivreg

**4 -  Exogeneity Hausman test
estat endogenous 
estadd scalar Endogeneity_test_chi2 = r(r_score)
estadd scalar pvalue_chi2 = r(p_r_score)
estadd scalar Endogeneity_test_F = r(regF)
estadd scalar pvalue_F = r(p_regF)


**5 - Manual 2SLS

reg app enc age educ fem party empl,r
eststo model1s
predict app_hat, xb
label variable app_hat "Estimated hours in app"
reg pol_kn app_hat age educ fem party empl, robust
eststo model2stage

esttab using "$tex_folder\iv2s.tex", stats(r2 N Endogeneity_test_chi2 Endogeneity_test_F) label se replace 








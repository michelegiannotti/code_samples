clear all
global ps3 "C:\Users\Xvast\OneDrive - Alma Mater Studiorum Università di Bologna\Master Unibo\microeconometrics\PS3"


*Run once to create the folder
*mkdir "$ps3\tex_outputs"
*mkdir "$ps3\graph_folder"

*Tex folder ouputs
global tex_folder "$ps3\tex_outputs"
*Graph folder output
global graphs "$ps3\graph_folder"


use "$ps3\life_sat.dta"


***************************** 1.1  Estimation of the Model *******************************

*Taking a look over the composition of the variables

tab cjs

*We observe that there are mainly three categories with data, grouping categories

gen cjs_mod= cjs
replace cjs_mod= 6 if cjs==3 | cjs==4 | cjs==97

*nchild
tab nchild
* most of the data is under 4 childs, thus we are grouping.
gen nchild_mod= nchild
replace nchild_mod=4 if nchild>=4

tab ngrchild
*Again 86% of the observations have 4 grandchildren or less.
gen ngrchild_mod= ngrchild
replace ngrchild_mod=4 if ngrchild>=4

*otrf

tab otrf

*Categories 2 and 4 are very limited. So, we should group them in the sense of those who pay rent, those who own the house and those who live rent free. Thus we group 2,3 and 4 together.

gen otrf_mod=otrf
replace otrf_mod= 2 if otrf>=2 & otrf<=4

tab hstatus
*We can see that this category is well-behaved.

tab mstat
*Categories 3 and 2 are very small that might affect the behavior of the variable, we group the married with the registered partnerships and the married but not living with the spouse with the divorced.
gen mstat_mod=mstat
replace mstat_mod=1 if mstat_mod==2
replace mstat_mod=5 if mstat_mod==3

*Keep the variables that we will use:
keep life_sat gender yedu mstat hstatus income year otrf_mod single ///
			thexp hnetw age nchild_mod ngrchild_mod ///
			phinact cjs_mod nchild otrf ngrchild cjs gali mstat_mod
			
*Labeling
label variable life_sat "Life satisfaction"
label variable cjs_mod "Current job status"
label variable nchild_mod "Number of children"
label variable ngrchild_mod "Number of grandchildren"
label variable otrf_mod "Tenure status"
label variable mstat_mod "Marriage status"
	

*We do a first exploratory regression
eststo clear
eststo: probit life_sat i.gender yedu i.mstat_mod gali i.hstatus income i.year  i.otrf_mod single thexp hnetw age i.nchild_mod i.ngrchild_mod phinact i.cjs_mod,r
			
*We still think that having children and grandchildren could contribute to the happiness but maybe its not about the number but just having or not.

gen hchild = (nchild!=0)
gen hgrchild= (ngrchild!=0)
label variable hchild "Having children"
label variable hgrchild "Having grandchildren"


eststo: probit life_sat i.gender yedu i.mstat_mod gali i.hstatus income i.year  i.otrf_mod single thexp hnetw age i.hchild i.hgrchild phinact i.cjs_mod,r
			
*Having children is still not significative so we remove it.

eststo: probit life_sat i.gender yedu i.mstat_mod gali i.hstatus income i.year  i.otrf_mod single thexp hnetw age phinact i.cjs_mod,r

esttab using "$tex_folder\point 1.1.tex", se ar2 depvars replace label interaction("*") noomitted nobaselevels


	
************** 1.2 Manual estimation of marginal effects of continous variable ****************************

probit life_sat i.gender yedu i.mstat_mod gali i.hstatus income i.year  i.otrf_mod single thexp hnetw age phinact i.cjs_mod,r
*matrix list e(b)

*To see the averages
sum gender yedu mstat_mod gali hstatus income year otrf_mod single thexp hnetw age phinact cjs_mod, detail

*We don't put the beta of mstatus, gali otrf(tenure status),single, phinact(physical inactivity) and cjs(Current job situation) because the base categories are the median representative person.
scalar xb1=_b[_cons]+_b[2.gender]+_b[yedu]*8.714448+_b[3.hstatus]+_b[income]*0.3097918+_b[2013.year]+_b[thexp]*8421.839+_b[hnetw]*248858.4+_b[age]*67
scalar meperinc=normalden(xb1)*_b[income]
display meperinc 

*********************** 1.3 Manual estimation of marginal effects of dummy variable gali *******************************

scalar prxb1=normal(xb1) 
scalar xb2=_b[_cons]+_b[2.gender]+_b[yedu]*8.714448+_b[3.hstatus]+_b[income]*0.3097918+_b[2013.year]+_b[thexp]*8421.839+_b[hnetw]*248858.4+_b[age]*67+_b[gali]
scalar prxb2=normal(xb2)

display prxb2-prxb1 

*********************** 1.4 Command estimation of marginal effects *******************************************

eststo clear
probit life_sat i.gender yedu i.mstat_mod gali i.hstatus income i.year  i.otrf_mod single thexp hnetw age phinact i.cjs_mod,r
* Marginal effects at the mean
estpost margins, dydx(_all) at(gender=2 yedu=8.714448 mstat_mod=1 gali=0  hstatus=3 income=0.3097918 year=2013 otrf_mod=1 single=0 thexp=8421.839 hnetw=248858.4 age=67 phinact=0 cjs_mod=1)
eststo PEAs

* Average marginal effects
probit life_sat i.gender yedu i.mstat_mod gali i.hstatus income i.year  i.otrf_mod single thexp hnetw age phinact i.cjs_mod,r
estpost margins, dydx(_all)
eststo APEs

esttab using "$tex_folder\point 1.4.tex", replace label noobs nobaselevels not

********************* 1.5  Confusion matrix of predicted probabilities ******************************************

*Model
probit life_sat i.gender yedu i.mstat_mod gali i.hstatus income i.year  i.otrf_mod single thexp hnetw age phinact i.cjs_mod,r

* Predict probabilities
predict phat_lifesat
label variable phat_lifesat "Predicted life satisfaction"

*Set threshold for clasification
replace phat_lifesat = phat_lifesat>0.5

*Labels
label define phat_lifesatlbl 0 "Predicted zeros" 1 "Predicted 1"
label values phat_lifesat phat_lifesatlbl

label define phat_lifesatlbl2 0 "Original zeros" 1 "Original 1"
label values life_sat phat_lifesatlbl2


**Get results

eststo clear
estpost tabulate life_sat phat_lifesat
esttab using "$tex_folder\point 1.5.tex", cell( b(fmt(2)) colpct(fmt(2))) unstack noobs collabels(none) nonumber replace

************************* 1.6  Differences due to gender *************************************

eststo clear

* Average marginal effects
probit life_sat income if gender==1 ,r
predict phat_male 
estpost margins, dydx(_all)
eststo males


probit life_sat income if gender==2 ,r 
predict phat_female
estpost margins, dydx(_all)
eststo female

esttab using "$tex_folder\point 1.6.tex", replace label noobs nobaselevels


twoway (scatter phat_male income if gender==1, mcolor("18 100 138")) /// 
(scatter phat_female income if gender==1, mcolor("18 150 20")), /// 
 xtitle("Income") ytitle("Predicted probabilities") ///
 legend(order(1 "Male predicted values" 2 "Female predicted values") rows(1) pos(6)) 
 
graph export "$graphs\Point 1.6.png", as(png) name("Graph") replace




	

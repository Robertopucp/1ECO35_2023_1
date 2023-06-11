******************************************************************************
**** Tables for Gender Differences in the choice of major ******************** 
**** Danila Serra and Catherine Porter ***************************************
**** Please contact catherine.porter@lancaster.ac.uk for queries **************
******************************************************************************




version 14.2
clear all
set more off, perm
*set maxvar 10000
set mem 32m, perm


* Set drives to work in
use SerraPorterAEJ.dta, clear


global nocontrols "yr_2016 treatment_class treat2016 "
global controls "yr_2016 treatment_class treat2016 female_prof instate freshman american ACumGPA gradePrinciples small_class" 
global controlsfe "yr_2016 treat2016  instate freshman american ACumGPA gradePrinciples " 
global controlsgrade "yr_2016 treatment_class treat2016 female_prof instate freshman american ACumGPA  small_class" 
global controlsfegrade "yr_2016 treat2016  instate freshman american ACumGPA  " 



count

********************************************************************************************************************
******************************* Table 1 Balance tests***************************************************************
********************************************************************************************************************

tab american treatment_class if yr_2016==0 & female==1, col chi
tab american treatment_class if yr_2016==1 & female==1, col chi

tab instate treatment_class if yr_2016==0 & female==1, col chi
tab instate treatment_class if yr_2016==1 & female==1, col chi

tab freshman treatment_class  if yr_2016==0 & female==1, col chi
tab freshman treatment_class  if yr_2016==1 & female==1, col chi

ttest ACumGPA if yr_2016==0 & female==1, by(treatment_class)
ttest ACumGPA if yr_2016==1 & female==1, by(treatment_class)

*SURVEY
tab greek treatment_class  if yr_2016==0 & female==1, col chi
tab greek treatment_class  if yr_2016==1 & female==1, col chi

tab econ_hs treatment_class if female==1 & yr_2016==0, col chi
tab econ_hs treatment_class if female==1 & yr_2016==1, col chi

tab varsity treatment_class if female==1 & yr_2016==0, col chi
tab varsity treatment_class if female==1 & yr_2016==1, col chi


********************************************************************************************************************
**************************** Table 2: unconditional outcome comparison **********************************************
********************************************************************************************************************

tab took_year treatment_class if female==1 & yr_2016==0, col chi
tab took_year treatment_class if female==1 & yr_2016==1, col chi

tab tookanother treatment_class if female==1 & yr_2016==0, col chi
tab tookanother treatment_class if female==1 & yr_2016==1, col chi

ttest numeconclass if yr_2016==0 & female==1, by(treatment_class)
ttest numeconclass if yr_2016==1 & female==1, by(treatment_class)

tab econmajor treatment_class if female==1 & yr_2016==0, col chi
tab econmajor treatment_class if female==1 & yr_2016==1, col chi


*************************************************************************************************************************************
****************************** TABLE 3: Treatment Effects on Intermediate Outcomes ****************************************************************
*************************************************************************************************************************************




*cd $localtemp

matrix drop _all

set seed 123456789
reg took_year $nocontrols if female==1, cluster(class_fe2) 
est sto m1
foreach x in $nocontrols _cons { 
boottest `x', reps(1000) weight(webb)  bootcluster(class_fe2) nograph
glo p`x'=r(p)
}

mat p1= ($pyr_2016, $ptreatment_class, $ptreat2016, $p_cons)
mat colnames p1 = yr_2016 treatment_class treat2016 _cons
estadd matrix p1


xi:  reg took_year $controls if female==1, cluster(class_fe2) 
est sto m2
foreach x in $controls _cons { 
boottest `x', reps(1000)  weight(webb) bootcluster(class_fe2) nograph
glo p`x'=r(p)
}
mat p1= ($pyr_2016, $ptreatment_class, $ptreat2016, $pfemale_prof, $pinstate, $pfreshman, $pamerican, $pACumGPA, $pgradePrinciples, $psmall_class, $p_cons)

mat colnames p1 = yr_2016 treatment_class treat2016 female_prof instate  freshman american ACumGPA gradePrinciples small_class _cons

estadd matrix p1

reg tookanother $nocontrols if female==1, cluster(class_fe2) 
est sto m3
foreach x in $nocontrols _cons { 
boottest `x', reps(1000)  weight(webb)  bootcluster(class_fe2) nograph
glo p`x'=r(p)
}
mat p1= ($pyr_2016, $ptreatment_class, $ptreat2016, $p_cons)

mat colnames p1 = yr_2016 treatment_class treat2016 _cons

estadd matrix p1

xi:  reg tookanother $controls if female==1, cluster(class_fe2) 
est sto m4
foreach x in $controls _cons { 
boottest `x', reps(1000) weight(webb)  bootcluster(class_fe2) nograph
glo p`x'=r(p)
}
mat p1= ($pyr_2016, $ptreatment_class, $ptreat2016, $pfemale_prof, $pinstate, $pfreshman, $pamerican, $pACumGPA, $pgradePrinciples, $psmall_class, $p_cons)

mat colnames p1 = yr_2016 treatment_class treat2016 female_prof instate freshman american ACumGPA gradePrinciples small_class _cons

estadd matrix p1

*esttab m1 m2 m3 m4 using TookyearLPM.tex, replace label  cells(b(star fmt(3)) p1(par fmt(3))) fragment


******************************************* *******************************************************************************************
******************************** TABLE 4: Treatment effects on FINAL outcomes *************************************************************************
***************************************************************************************************************************************
est clear 
matrix drop _all
set seed 123456789

reg numeconclass $nocontrols if female==1, cluster(class_fe2) 
est sto m1
foreach x in $nocontrols _cons { 
boottest `x', reps(10000)  weight(webb) bootcluster(class_fe2) nograph
glo p`x'=r(p)
}
mat p1= ($pyr_2016, $ptreatment_class, $ptreat2016, $p_cons)

mat colnames p1 = yr_2016 treatment_class treat2016 _cons
estadd matrix p1

xi:  reg numeconclass $controls if female==1, cluster(class_fe2) 
est sto m2
foreach x in $controls _cons { 
boottest `x', reps(10000) weight(webb)  bootcluster(class_fe2) nograph
glo p`x'=r(p)
}
mat p1= ($pyr_2016, $ptreatment_class, $ptreat2016, $pfemale_prof, $pinstate, $pfreshman, $pamerican, $pACumGPA, $pgradePrinciples, $psmall_class, $p_cons)

mat colnames p1 = yr_2016 treatment_class treat2016 female_prof instate freshman american ACumGPA gradePrinciples small_class _cons
estadd matrix p1

reg econmajor $nocontrols if female==1, cluster(class_fe2) 
est sto m3
foreach x in $nocontrols _cons { 
boottest `x', reps(10000) weight(webb)  bootcluster(class_fe2) nograph
glo p`x'=r(p)
}
mat p1= ($pyr_2016, $ptreatment_class, $ptreat2016, $p_cons)

mat colnames p1 = yr_2016 treatment_class treat2016 _cons
estadd matrix p1

xi:  reg econmajor $controls if female==1, cluster(class_fe2) 
est sto m4
foreach x in $controls _cons { 
boottest `x', reps(10000) weight(webb) bootcluster(class_fe2) nograph
glo p`x'=r(p)
}
mat p1= ($pyr_2016, $ptreatment_class, $ptreat2016, $pfemale_prof, $pinstate, $pfreshman, $pamerican, $pACumGPA, $pgradePrinciples, $psmall_class, $p_cons)

mat colnames p1 = yr_2016 treatment_class treat2016 female_prof instate freshman american ACumGPA gradePrinciples small_class _cons
estadd matrix p1

*esttab m1 m2 m3 m4 using EconmajorLPM.tex, replace label  cells(b(star fmt(3)) p1(par fmt(3))) fragment



****************************************************************************************************************************
***********************************TABLE 5 EFFECTS ON OTHER MAJORS: HIGH EARNING *******************************************
****************************************************************************************************************************

est clear 
matrix drop _all
set seed 123456789
 
reg Major_STEM $controls if female==1 , cluster(class_fe2) 
est sto m1
foreach x in $controls _cons { 
boottest `x', reps(10000)  weight(webb) bootcluster(class_fe2) nograph
glo p`x'=r(p)
}
mat p1= ($pyr_2016, $ptreatment_class, $ptreat2016, $pfemale_prof, $pinstate, $pfreshman, $pamerican, $pACumGPA, $pgradePrinciples, $psmall_class, $p_cons)

mat colnames p1 = yr_2016 treatment_class treat2016 female_prof instate freshman american ACumGPA gradePrinciples small_class _cons
estadd matrix p1

reg  Major_Business $controls if female==1 , cluster(class_fe2) 
est sto m2
foreach x in $controls _cons { 
boottest `x', reps(10000)  weight(webb) bootcluster(class_fe2) nograph
glo p`x'=r(p)
}
mat p1= ($pyr_2016, $ptreatment_class, $ptreat2016, $pfemale_prof, $pinstate, $pfreshman, $pamerican, $pACumGPA, $pgradePrinciples, $psmall_class, $p_cons)

mat colnames p1 = yr_2016 treatment_class treat2016 female_prof instate freshman american ACumGPA gradePrinciples small_class _cons
estadd matrix p1

reg  Major_Finance $controls if female==1 , cluster(class_fe2) 
est sto m3
foreach x in $controls _cons { 
boottest `x', reps(10000)  weight(webb) bootcluster(class_fe2) nograph
glo p`x'=r(p)
}
mat p1= ($pyr_2016, $ptreatment_class, $ptreat2016, $pfemale_prof, $pinstate, $pfreshman, $pamerican, $pACumGPA, $pgradePrinciples, $psmall_class, $p_cons)

mat colnames p1 = yr_2016 treatment_class treat2016 female_prof instate freshman american ACumGPA gradePrinciples small_class _cons
estadd matrix p1

reg  Major_Marketing $controls if female==1 , cluster(class_fe2) 
est sto m4
foreach x in $controls _cons { 
boottest `x', reps(10000)  weight(webb) bootcluster(class_fe2) nograph
glo p`x'=r(p)
}
mat p1= ($pyr_2016, $ptreatment_class, $ptreat2016, $pfemale_prof, $pinstate, $pfreshman, $pamerican, $pACumGPA, $pgradePrinciples, $psmall_class, $p_cons)

mat colnames p1 = yr_2016 treatment_class treat2016 female_prof instate freshman american ACumGPA gradePrinciples small_class _cons
estadd matrix p1

*esttab m1 m2 m3 m4 using HighearnmajorsLPM.tex, replace label  cells(b(star fmt(3)) p1(par fmt(3))) fragment


****************************************************************************************************************************
***********************************TABLE 6 EFFECTS ON OTHER MAJORS: LOW EARNING *******************************************
****************************************************************************************************************************
est clear 
matrix drop _all
set seed 123456789

reg Major_SocSc $controls if female==1 , cluster(class_fe2) 
est sto m1
foreach x in $controls _cons { 
boottest `x', reps(10000)  weight(webb) bootcluster(class_fe2) nograph
glo p`x'=r(p)
}
mat p1= ($pyr_2016, $ptreatment_class, $ptreat2016, $pfemale_prof, $pinstate, $pfreshman, $pamerican, $pACumGPA, $pgradePrinciples, $psmall_class, $p_cons)

mat colnames p1 = yr_2016 treatment_class treat2016 female_prof instate freshman american ACumGPA gradePrinciples small_class _cons
estadd matrix p1

reg  Major_Arts $controls if female==1 , cluster(class_fe2) 
est sto m2
foreach x in $controls _cons { 
boottest `x', reps(10000)  weight(webb) bootcluster(class_fe2) nograph
glo p`x'=r(p)
}
mat p1= ($pyr_2016, $ptreatment_class, $ptreat2016, $pfemale_prof, $pinstate, $pfreshman, $pamerican, $pACumGPA, $pgradePrinciples, $psmall_class, $p_cons)

mat colnames p1 = yr_2016 treatment_class treat2016 female_prof instate freshman american ACumGPA gradePrinciples small_class _cons
estadd matrix p1

reg  Major_Comm $controls if female==1 , cluster(class_fe2) 
est sto m3
foreach x in $controls _cons { 
boottest `x', reps(10000)  weight(webb) bootcluster(class_fe2) nograph
glo p`x'=r(p)
}
mat p1= ($pyr_2016, $ptreatment_class, $ptreat2016, $pfemale_prof, $pinstate, $pfreshman, $pamerican, $pACumGPA, $pgradePrinciples, $psmall_class, $p_cons)
mat colnames p1 = yr_2016 treatment_class treat2016 female_prof instate freshman american ACumGPA gradePrinciples small_class _cons
estadd matrix p1

reg   Major_Hum $controls if female==1 , cluster(class_fe2) 
est sto m4
foreach x in $controls _cons { 
boottest `x', reps(10000)  weight(webb) bootcluster(class_fe2) nograph
glo p`x'=r(p)
}
mat p1= ($pyr_2016, $ptreatment_class, $ptreat2016, $pfemale_prof, $pinstate, $pfreshman, $pamerican, $pACumGPA, $pgradePrinciples, $psmall_class, $p_cons)
mat colnames p1 = yr_2016 treatment_class treat2016 female_prof instate freshman american ACumGPA gradePrinciples small_class _cons
estadd matrix p1
*esttab m1 m2 m3 m4 using LowearnmajorsLPM.tex, replace label  cells(b(star fmt(3)) p1(par fmt(3))) fragment

********************************************************************************************************************************
** TABLE 7 PERFORMANCE IN INTERMEDIATE MICRO

*women 2015
ttest grade3301 if yr_2016==0 & female==1 , by(treatment_class)
ttest Testscore3301 if yr_2016==0 & female==1, by(treatment_class)
*missing 2 female students in 2015(did not take the test)

*women 2016
*We are missing the test scores of 8 women from the 2016 cohort
ttest grade3301 if yr_2016==1 & female==1, by(treatment_class)
ttest Testscore3301 if yr_2016==1 & female==1, by(treatment_class)



**********************************************************************************************************************************
*************************************TABLE 8: EFFECT ON GRADEP********************************************************************
**********************************************************************************************************************************

matrix drop _all
set seed 123456789


reg gradeP $nocontrols if female==1, cluster(class_fe2) 
est sto m1
foreach x in $nocontrols _cons { 
boottest `x', reps(1000)  bootcluster(class_fe2) weight (webb) nograph
glo p`x'=r(p)
}
mat p1= ($pyr_2016, $ptreatment_class, $ptreat2016, $p_cons)
mat colnames p1 = yr_2016 treatment_class treat2016 _cons
estadd matrix p1

xi:  reg gradeP $controlsgrade if female==1, cluster(class_fe2) 
est sto m2
foreach x in $controlsgrade _cons { 
boottest `x', reps(1000) bootcluster(class_fe2) weight (webb) nograph
glo p`x'=r(p)
}
mat p1= ($pyr_2016, $ptreatment_class, $ptreat2016, $pfemale_prof, $pinstate, $pfreshman, $pamerican, $pACumGPA,  $psmall_class, $p_cons)
mat colnames p1 = yr_2016 treatment_class treat2016 female_prof instate  freshman american ACumGPA  small_class _cons
estadd matrix p1

reg gradeP $nocontrols i.class_fe if female==1, cluster(class_fe2) 
est sto m3
foreach x in yr_2016  treat2016 _cons { 
boottest `x', reps(1000)  bootcluster(class_fe2) weight (webb) nograph
glo p`x'=r(p)
}
mat p1= ($pyr_2016,  $ptreat2016, $p_cons)
mat colnames p1 = yr_2016  treat2016 _cons
estadd matrix p1

xi:  reg gradeP $controlsfegrade i.class_fe if female==1, cluster(class_fe2) 
est sto m4
foreach x in $controlsfegrade _cons { 
boottest `x', reps(1000) bootcluster(class_fe2) weight (webb) nograph
glo p`x'=r(p)
}
mat p1= ($pyr_2016,  $ptreat2016,  $pinstate, $pfreshman, $pamerican, $pACumGPA,  $p_cons)
mat colnames p1 = yr_2016  treat2016  instate freshman american ACumGPA   _cons
estadd matrix p1

*esttab m1 m2 m3 m4 using GradePLPM.tex, replace label  cells(b(star fmt(3)) p1(par fmt(3))) fragment



***********************************************************************************************************************************
********************************TABLE 9 CLASS FIXED EFFECTS,  NO CONTROLS *********************************************************
***********************************************************************************************************************************
est clear 
matrix drop _all
set seed 123456789
*** Four outcomes (tookyear, tookanother, numeconclass, econmajor) with class fixed effects and no controls

xi: reg took_year yr_2016 treat2016 i.class_fe if female==1, cluster(class_fe2) 
g fixedsample=e(sample)
lab var fixedsample "In a class which didn't change"
est sto m1
foreach x in yr_2016 treat2016 _cons { 
boottest `x', reps(10000) weight(webb) bootcluster(class_fe2) nograph
glo p`x'=r(p)
}
mat p1= ($pyr_2016, $ptreat2016, $p_cons)
mat colnames p1 = yr_2016  treat2016 _cons
estadd matrix p1

xi: reg tookanother yr_2016 treat2016 i.class_fe if female==1, cluster(class_fe2) 
est sto m2
foreach x in yr_2016 treat2016 _cons { 
boottest `x', reps(10000) weight(webb) bootcluster(class_fe2) nograph
glo p`x'=r(p)
}
mat p1= ($pyr_2016, $ptreat2016, $p_cons)
mat colnames p1 = yr_2016  treat2016 _cons
estadd matrix p1

xi: reg numeconclass yr_2016 treat2016 i.class_fe if female==1, cluster(class_fe2) 
est sto m3
foreach x in yr_2016 treat2016 _cons { 
boottest `x', reps(10000)  weight(webb) bootcluster(class_fe2) nograph
glo p`x'=r(p)
}
mat p1= ($pyr_2016, $ptreat2016, $p_cons)
mat colnames p1 = yr_2016  treat2016 _cons
estadd matrix p1

xi: reg econmajor yr_2016 treat2016 i.class_fe if female==1, cluster(class_fe2) 
est sto m4
foreach x in yr_2016 treat2016 _cons { 
boottest `x', reps(10000) weight(webb) bootcluster(class_fe2) nograph
glo p`x'=r(p)
}
mat p1= ($pyr_2016, $ptreat2016, $p_cons)
mat colnames p1 = yr_2016  treat2016 _cons
estadd matrix p1


*esttab m1 m2 m3 m4 using Classfe1LPM.tex, keep (yr_2016  treat2016 _cons) replace label  cells(b(star fmt(3)) p1(par fmt(3))) fragment

***********************************************************************************************************************************
********************************************Lower panel:  with controls************************************************************
***********************************************************************************************************************************
est clear 
matrix drop _all
set seed 123456789
*** Four outcomes (tookyear, tookanother, numeconclass, econmajor) with class fixed effects and  controls

xi: reg took_year $controlsfe i.class_fe if female==1, cluster(class_fe2) 
est sto m1
foreach x in $controlsfe _cons { 
boottest `x', reps(10000) weight(webb) bootcluster(class_fe2) nograph
glo p`x'=r(p)
}
mat p1= ($pyr_2016, $ptreat2016, $pinstate, $pfreshman, $pamerican, $pACumGPA, $pgradePrinciples, $p_cons)

mat colnames p1 = yr_2016 treat2016 instate freshman american ACumGPA gradePrinciples _cons
estadd matrix p1

xi: reg tookanother $controlsfe i.class_fe if female==1, cluster(class_fe2) 
est sto m2
foreach x in $controlsfe _cons { 
boottest `x', reps(10000) weight(webb) bootcluster(class_fe2) nograph
glo p`x'=r(p)
}
mat p1= ($pyr_2016, $ptreat2016, $pinstate, $pfreshman, $pamerican, $pACumGPA, $pgradePrinciples, $p_cons)

mat colnames p1 = yr_2016 treat2016 instate freshman american ACumGPA gradePrinciples _cons
estadd matrix p1

xi: reg numeconclass $controlsfe i.class_fe if female==1, cluster(class_fe2) 
est sto m3
foreach x in yr_2016 treat2016 _cons { 
boottest `x', reps(10000)  weight(webb) bootcluster(class_fe2) nograph
glo p`x'=r(p)
}
mat p1= ($pyr_2016, $ptreat2016, $pinstate, $pfreshman, $pamerican, $pACumGPA, $pgradePrinciples, $p_cons)

mat colnames p1 = yr_2016 treat2016 instate freshman american ACumGPA gradePrinciples _cons
estadd matrix p1

xi: reg econmajor $controlsfe i.class_fe if female==1, cluster(class_fe2) 
est sto m4
foreach x in yr_2016 treat2016 _cons { 
boottest `x', reps(10000) weight(webb) bootcluster(class_fe2) nograph
glo p`x'=r(p)
}
mat p1= ($pyr_2016, $ptreat2016, $pinstate, $pfreshman, $pamerican, $pACumGPA, $pgradePrinciples, $p_cons)
mat colnames p1 = yr_2016 treat2016 instate freshman american ACumGPA gradePrinciples _cons
estadd matrix p1


*****************************************************************************************************************************************************
************************************ANNEX TABLES ONLINE APPENDIX ************************************************************************************
*****************************************************************************************************************************************************

*** TABLE A1: Balance tests Men

tab american treatment_class if yr_2016==0 & female==0, col chi
tab american treatment_class if yr_2016==1 & female==0, col chi

tab instate treatment_class if yr_2016==0 & female==0, col chi
tab instate treatment_class if yr_2016==1 & female==0, col chi

tab freshman treatment_class  if yr_2016==0 & female==0, col chi
tab freshman treatment_class  if yr_2016==1 & female==0, col chi

ttest ACumGPA if yr_2016==0 & female==0, by(treatment_class)
ttest ACumGPA if yr_2016==1 & female==0, by(treatment_class)


*SURVEY
tab greek treatment_class  if yr_2016==0 & female==0, col chi
tab greek treatment_class  if yr_2016==1 & female==0, col chi

tab econ_hs treatment_class if female==0 & yr_2016==0, col chi
tab econ_hs treatment_class if female==0 & yr_2016==1, col chi

tab varsity treatment_class if female==0 & yr_2016==0, col chi
tab varsity treatment_class if female==0 & yr_2016==1, col chi

***************************************************************************************************************************************************************
*****************************************************TABLE A2: PROBIT RESULTS WITH CONTROLS*******************************************************************
***************************************************************************************************************************************************************
******************************************************************************************************************************************************************************************
est clear 
matrix drop _all
set seed 123456789
xi: probit took_year $controls  if female==1 , cluster(class_fe2)
est sto m1
foreach x in $controls _cons { 
boottest `x', reps(10000) weight (webb) robust cluster (class_fe2) small
glo p`x'=r(p)
}
mat p1= ($pyr_2016, $ptreatment_class, $ptreat2016, $pfemale_prof, $pinstate,  $pfreshman, $pamerican, $pACumGPA, $pgradePrinciples, $psmall_class, $p_cons)
mat colnames p1 = yr_2016 treatment_class treat2016 female_prof instate freshman  american ACumGPA gradePrinciples small_class _cons
estadd matrix p1

xi: probit tookanother $controls  if female==1 , cluster(class_fe2)
est sto m2
foreach x in $controls _cons { 
boottest `x', reps(10000) weight (webb) robust cluster (class_fe2) small
glo p`x'=r(p)
}
mat p1= ($pyr_2016, $ptreatment_class, $ptreat2016, $pfemale_prof, $pinstate,  $pfreshman, $pamerican, $pACumGPA, $pgradePrinciples, $psmall_class, $p_cons)
mat colnames p1 = yr_2016 treatment_class treat2016 female_prof instate freshman  american ACumGPA gradePrinciples small_class _cons
estadd matrix p1


xi: probit econmajor $controls  if female==1 , cluster(class_fe2)
est sto m3
foreach x in $controls _cons { 
boottest `x', reps(10000) weight (webb) robust cluster (class_fe2) small
glo p`x'=r(p)
}
mat p1= ($pyr_2016, $ptreatment_class, $ptreat2016, $pfemale_prof, $pinstate,  $pfreshman, $pamerican, $pACumGPA, $pgradePrinciples, $psmall_class, $p_cons)
mat colnames p1 = yr_2016 treatment_class treat2016 female_prof instate freshman  american ACumGPA gradePrinciples small_class _cons
estadd matrix p1

*esttab m1 m2 m3  using PROBITwithcontrols.tex, replace label  cells(b(star fmt(3)) p1(par fmt(3))) fragment

***************************************************************************************************************************************
***LOWER PANEL: ADD FIXED EFFECTS
*********************************
est clear 
matrix drop _all
set seed 123456789
xi: probit took_year $controlsfe i.class_fe if female==1 , cluster(class_fe2)
est sto m1
foreach x in $controlsfe _cons { 
boottest `x', reps(10000) weight (webb) robust cluster (class_fe2) small
glo p`x'=r(p)
}
mat p1= ($pyr_2016, $ptreatment_class, $ptreat2016,  $pinstate,  $pfreshman, $pamerican, $pACumGPA, $pgradePrinciples,  $p_cons)
mat colnames p1 = yr_2016 treatment_class treat2016  instate freshman  american ACumGPA gradePrinciples  _cons
estadd matrix p1

xi: probit tookanother $controlsfe i.class_fe if female==1 , cluster(class_fe2)
est sto m2
foreach x in $controlsfe _cons { 
boottest `x', reps(10000) weight (webb) robust cluster (class_fe2) small
glo p`x'=r(p)
}
mat p1= ($pyr_2016, $ptreatment_class, $ptreat2016, $pinstate,  $pfreshman, $pamerican, $pACumGPA, $pgradePrinciples,  $p_cons)
mat colnames p1 = yr_2016 treatment_class treat2016  instate freshman  american ACumGPA gradePrinciples  _cons
estadd matrix p1


xi: probit econmajor $controlsfe  if female==1 , cluster(class_fe2)
est sto m3
foreach x in $controlsfe _cons { 
boottest `x', reps(10000) weight (webb) robust cluster (class_fe2) small
glo p`x'=r(p)
}
mat p1= ($pyr_2016, $ptreatment_class, $ptreat2016,  $pinstate,  $pfreshman, $pamerican, $pACumGPA, $pgradePrinciples,  $p_cons)
mat colnames p1 = yr_2016 treatment_class treat2016  instate freshman  american ACumGPA gradePrinciples  _cons
estadd matrix p1

*esttab m1 m2 m3  using PROBITwithcontrolsfe.tex, replace label  cells(b(star fmt(3)) p1(par fmt(3))) fragment

******************************************************************************************************************************************************************************************
***************************************Robustness: TABLE A3 Top female students   *********************************************************************************************************
******************************************************************************************************************************************************************************************

est clear 
matrix drop _all
set seed 123456789

reg took_year $controls if female==1 & top==1, cluster(class_fe2) 
est sto m1
foreach x in $controls _cons { 
boottest `x', reps(10000)  weight(webb) bootcluster(class_fe2) nograph
glo p`x'=r(p)
}
mat p1= ($pyr_2016, $ptreatment_class, $ptreat2016, $pfemale_prof, $pinstate, $pfreshman, $pamerican, $pACumGPA, $pgradePrinciples, $psmall_class, $p_cons)

mat colnames p1 = yr_2016 treatment_class treat2016 female_prof instate freshman american ACumGPA gradePrinciples small_class _cons
estadd matrix p1

xi:  reg tookanother $controls if female==1 & top==1, cluster(class_fe2) 
est sto m2
foreach x in $controls _cons { 
boottest `x', reps(10000) weight(webb)  bootcluster(class_fe2) nograph
glo p`x'=r(p)
}
mat p1= ($pyr_2016, $ptreatment_class, $ptreat2016, $pfemale_prof, $pinstate, $pfreshman, $pamerican, $pACumGPA, $pgradePrinciples, $psmall_class, $p_cons)

mat colnames p1 = yr_2016 treatment_class treat2016 female_prof instate freshman american ACumGPA gradePrinciples small_class _cons
estadd matrix p1

reg numeconclass $controls if female==1 & top==1, cluster(class_fe2) 
est sto m3
foreach x in $controls _cons { 
boottest `x', reps(10000) weight(webb)  bootcluster(class_fe2) nograph
glo p`x'=r(p)
}
mat p1= ($pyr_2016, $ptreatment_class, $ptreat2016, $pfemale_prof, $pinstate, $pfreshman, $pamerican, $pACumGPA, $pgradePrinciples, $psmall_class, $p_cons)

mat colnames p1 = yr_2016 treatment_class treat2016 female_prof instate freshman american ACumGPA gradePrinciples small_class _cons
estadd matrix p1

xi:  reg econmajor $controls if female==1 & top==1, cluster(class_fe2) 
est sto m4
foreach x in $controls _cons { 
boottest `x', reps(10000) weight(webb) bootcluster(class_fe2) nograph
glo p`x'=r(p)
}
mat p1= ($pyr_2016, $ptreatment_class, $ptreat2016, $pfemale_prof, $pinstate, $pfreshman, $pamerican, $pACumGPA, $pgradePrinciples, $psmall_class, $p_cons)

mat colnames p1 = yr_2016 treatment_class treat2016 female_prof instate freshman american ACumGPA gradePrinciples small_class _cons
estadd matrix p1

esttab m1 m2 m3 m4 using TopLPM.tex, replace label  cells(b(star fmt(3)) p1(par fmt(3))) fragment

*****************************************************************************************************************************************************
********************TABLE A4: Treatment Effects on Male Students **************************************************************************************
*****************************************************************************************************************************************************

est clear 
matrix drop _all
set seed 123456789

reg took_year $controls if female==0, cluster(class_fe2) 
est sto m1
foreach x in $controls _cons { 
boottest `x', reps(10000)  weight(webb) bootcluster(class_fe2) nograph
glo p`x'=r(p)
}
mat p1= ($pyr_2016, $ptreatment_class, $ptreat2016, $pfemale_prof, $pinstate, $pfreshman, $pamerican, $pACumGPA, $pgradePrinciples, $psmall_class, $p_cons)

mat colnames p1 = yr_2016 treatment_class treat2016 female_prof instate freshman american ACumGPA gradePrinciples small_class _cons
estadd matrix p1

xi:  reg tookanother $controls if female==0, cluster(class_fe2) 
est sto m2
foreach x in $controls _cons { 
boottest `x', reps(10000) weight(webb)  bootcluster(class_fe2) nograph
glo p`x'=r(p)
}
mat p1= ($pyr_2016, $ptreatment_class, $ptreat2016, $pfemale_prof, $pinstate, $pfreshman, $pamerican, $pACumGPA, $pgradePrinciples, $psmall_class, $p_cons)

mat colnames p1 = yr_2016 treatment_class treat2016 female_prof instate freshman american ACumGPA gradePrinciples small_class _cons
estadd matrix p1

reg numeconclass $controls if female==0, cluster(class_fe2) 
est sto m3
foreach x in $controls _cons { 
boottest `x', reps(10000) weight(webb)  bootcluster(class_fe2) nograph
glo p`x'=r(p)
}
mat p1= ($pyr_2016, $ptreatment_class, $ptreat2016, $pfemale_prof, $pinstate, $pfreshman, $pamerican, $pACumGPA, $pgradePrinciples, $psmall_class, $p_cons)

mat colnames p1 = yr_2016 treatment_class treat2016 female_prof instate freshman american ACumGPA gradePrinciples small_class _cons
estadd matrix p1

xi:  reg econmajor $controls if female==0, cluster(class_fe2) 
est sto m4
foreach x in $controls _cons { 
boottest `x', reps(10000) weight(webb) bootcluster(class_fe2) nograph
glo p`x'=r(p)
}
mat p1= ($pyr_2016, $ptreatment_class, $ptreat2016, $pfemale_prof, $pinstate, $pfreshman, $pamerican, $pACumGPA, $pgradePrinciples, $psmall_class, $p_cons)
mat colnames p1 = yr_2016 treatment_class treat2016 female_prof instate freshman american ACumGPA gradePrinciples small_class _cons
estadd matrix p1

*esttab m1 m2 m3 m4 using MenwithcontrolsLPM.tex, replace label  cells(b(star fmt(3)) p1(par fmt(3))) fragment
 
 
*****************************************************************************************************************************************************************************************
*********************************************TABLE A5 ROBUSTNESS: TRIPLE DIFFERENCE WITH MEN,  CLASS FE **********************************************************************************************
******************************************************************************************************************************************************************************************
est clear 
matrix drop _all
set seed 123456789
xi: clustse reg took_year  yr_2016 female female_treat fem2016 treat2016 femtreat16  instate freshman american ACumGPA gradePrinciples i.class_fe, cluster(class_fe2) method(wild) reps(1000)
 
*outreg2 using Triplefe.out, replace label bdec(3) word tex p
xi: clustse reg tookanother yr_2016 female female_treat fem2016 treat2016 femtreat16 instate freshman american ACumGPA gradePrinciples i.class_fe, cluster(class_fe2) method(wild) reps(1000)
*outreg2 using Triplefe.out, append label bdec(3) word tex p

xi: clustse reg numeconclass yr_2016 female female_treat fem2016 treat2016 femtreat16 instate freshman american ACumGPA gradePrinciples i.class_fe, cluster(class_fe2) method(wild) reps(1000)
*outreg2 using Triplefe.out, append label bdec(3) word tex p

xi: clustse reg econmajor  yr_2016 female female_treat fem2016 treat2016 femtreat16 instate freshman american ACumGPA gradePrinciples i.class_fe, cluster(class_fe2) method(wild) reps(1000)
*outreg2 using Triplefe.out, append label bdec(3) word tex p 



************************************************************************************************************************************************
******************************TABLE A6: TRIPLE DIFFERENCE, NO FIXED EFFECTS ********************************************************************
************************************************************************************************************************************************
est clear 
matrix drop _all
set seed 123456789

xi: clustse reg took_year treatment_class yr_2016 female female_treat fem2016 treat2016 femtreat16  instate freshman american ACumGPA gradePrinciples female_prof small_class , cluster(class_fe2) method(wild) reps(1000)
*** outreg2 using Triple.out, replace label bdec(3) word tex p
xi: clustse reg tookanother treatment_class yr_2016 female female_treat fem2016 treat2016 femtreat16 instate freshman american ACumGPA gradePrinciples female_prof small_class, cluster(class_fe2) method(wild) reps(1000)
**outreg2 using Triple.out, append label bdec(3) word tex p

   
xi: clustse reg numeconclass treatment_class yr_2016 female female_treat fem2016 treat2016 femtreat16 instate freshman american ACumGPA gradePrinciples female_prof small_class, cluster(class_fe2) method(wild) reps(1000)
*outreg2 using Triple.out, append label bdec(3) word tex p

xi: clustse reg econmajor treatment_class yr_2016 female female_treat fem2016 treat2016 femtreat16 instate freshman american ACumGPA gradePrinciples female_prof small_class, cluster(class_fe2) method(wild) reps(1000)
*outreg2 using Triple.out, append label bdec(3) word tex p
 

************************************************************************************************************************************************
*******************************************ANALYSIS OF MECHANISMS***********************
************************************************************************************************************************************************

keep if surveyed==1

gen job_marketing=1 if  strpos(ecojobs_a, "Marketing") > 0|strpos(ecojobs_b, "Marketing") > 0|strpos(ecojobs_c, "Marketing") > 0|strpos(ecojobs_d, "Marketing") > 0|strpos(ecojobs_e, "Marketing") > 0|strpos(ecojobs_f, "Marketing") > 0|strpos(ecojobs_g, "Marketing") > 0|strpos(ecojobs_g, "Marketing") > 0
replace job_marketing=0 if job_marketing==. & ecojobs_a!=""

gen job_manager=1 if  strpos(ecojobs_a, "Manag") > 0|strpos(ecojobs_b, "Manag") > 0|strpos(ecojobs_c, "Manag") > 0|strpos(ecojobs_d, "Manag") > 0|strpos(ecojobs_e, "Manag") > 0|strpos(ecojobs_f, "Manag") > 0|strpos(ecojobs_g, "Manag") > 0|strpos(ecojobs_g, "Manag") > 0
replace job_manager=0 if job_manager==. & ecojobs_a!=""

gen job_markmng=1 if job_marketing==1|job_manager==1
replace job_markmng=0 if job_markmng==.
replace job_markmng=. if job_manager==.

tab job_markmng treatment_class if yr_2016==0&female==1, col chi
tab job_markmng treatment_class if yr_2016==1&female==1, col chi

tab econmajor job_markmng if yr_2016==1&female==1, col chi
*None of the students who want a job in marketing or management majored in economics

************************************************************************************************************************************************
*************************************************END OF FILE ***********************************************************************************
************************************************************************************************************************************************



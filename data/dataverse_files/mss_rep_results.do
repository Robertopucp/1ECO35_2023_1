clear
set mem 150m
set more off
set matsize 775
#delimit ;


log using t:\mss_civil\rep_results.log, replace;


use t:\stata\mss_repdata.dta; 



global x_fl = "y_0 polity2l ethfrac relfrac Oil lpopl1 lmtnest";
global x_year = "Iccyear*";



/* Construct Country Fixed Effects */;
tab ccode, gen(Iccode);  

/* Construct Time Trend */;
rename year year_actual;
gen year = year_actual - 1978;
label variable year "Year Trend";

/* Construct Country-Specific Time Trend */;
local i=1;
while (`i'< 42)
{;
gen Iccyear`i' = Iccode`i'*year;
label variable Iccyear`i' "Country-Specific Time Trend for Iccode`i'";
local i = `i' + 1;
};




/* TABLE 1: DESCRIPTIVE STATISTICS */; 

summ any_prio any_prio_on any_prio_off war_prio war_prio_on war_prio_off war_col war_inc war;
summ GPCP GPCP_g GPCP_g_l;
summ gdp_g gdp_g_l;
summ y_0 polity2l polity2l_6 ethfrac relfrac Oil lmtnest lpopl1 tot_100_g;	



/* TABLE 2: FIRST STAGE */

regress gdp_g GPCP_g GPCP_g_l, robust cluster(ccode); 
regress gdp_g GPCP_g GPCP_g_l $x_fl $x_year, robust cluster(ccode);
regress gdp_g GPCP_g GPCP_g_l Iccode* $x_year, robust cluster(ccode); 
regress gdp_g GPCP_g GPCP_g_l GPCP_g_fl Iccode* $x_year, robust cluster(ccode); 
regress gdp_g GPCP_g GPCP_g_l tot_100_g Iccode* $x_year, robust cluster(ccode); 



/* TABLE 3: REDUCED FORM */

regress any_prio GPCP_g GPCP_g_l Iccode* $x_year, robust cluster(ccode); 
regress war_prio GPCP_g GPCP_g_l Iccode* $x_year, robust cluster(ccode);



/* TABLE 4: OLS VERSUS 2SLS */

dprobit any_prio gdp_g gdp_g_l $x_fl year, robust cluster(ccode);
regress any_prio gdp_g gdp_g_l $x_fl year, robust cluster(ccode);
regress any_prio gdp_g gdp_g_l $x_fl $x_year, robust cluster(ccode);
regress any_prio gdp_g gdp_g_l Iccode* $x_year, robust cluster(ccode);

regress any_prio gdp_g gdp_g_l $x_fl $x_year (GPCP_g GPCP_g_l $x_fl $x_year), robust cluster(ccode);
regress any_prio gdp_g gdp_g_l Iccode* $x_year (GPCP_g GPCP_g_l Iccode* $x_year), robust cluster(ccode);
regress war_prio gdp_g gdp_g_l Iccode* $x_year (GPCP_g GPCP_g_l Iccode* $x_year), robust cluster(ccode);



/* TABLE 5: INTERACTIONS */

/* Construct Interactions */;

foreach var1 in
	gdp_g gdp_g_l GPCP_g GPCP_g_l
{;
foreach var2 in
	polity2l y_0 ethfrac Oil lmtnest 
{;
quietly gen `var1'_`var2' = `var1'*`var2';
};
};

regress any_prio gdp_g gdp_g_l gdp_g_polity2l gdp_g_l_polity2l Iccode* $x_year
(GPCP_g GPCP_g_l GPCP_g_polity2l GPCP_g_l_polity2l Iccode* $x_year), robust cluster(ccode);
regress any_prio gdp_g gdp_g_l gdp_g_y_0 gdp_g_l_y_0 Iccode* $x_year 
(GPCP_g GPCP_g_l GPCP_g_y_0 GPCP_g_l_y_0 Iccode* $x_year), robust cluster(ccode);
regress any_prio gdp_g gdp_g_l gdp_g_ethfrac gdp_g_l_ethfrac Iccode* $x_year 
(GPCP_g GPCP_g_l GPCP_g_ethfrac GPCP_g_l_ethfrac Iccode* $x_year), robust cluster(ccode);
regress any_prio gdp_g gdp_g_l gdp_g_Oil gdp_g_l_Oil Iccode* $x_year 
(GPCP_g GPCP_g_l GPCP_g_Oil GPCP_g_l_Oil Iccode* $x_year), robust cluster(ccode);
regress any_prio gdp_g gdp_g_l gdp_g_lmtnest gdp_g_l_lmtnest Iccode* $x_year 
(GPCP_g GPCP_g_l GPCP_g_lmtnest GPCP_g_l_lmtnest Iccode* $x_year), robust cluster(ccode);



/* TABLE 6: ONSET */

regress any_prio_on gdp_g gdp_g_l Iccode* $x_year (GPCP_g GPCP_g_l Iccode* $x_year), robust cluster(ccode);
regress war_prio_on gdp_g gdp_g_l Iccode* $x_year (GPCP_g GPCP_g_l Iccode* $x_year), robust cluster(ccode);



/* TABLE A2: OTHER RAINFALL MEASURES */

regress any_prio gdp_g gdp_g_l Iccode* $x_year (GPCP_g GPCP_g_l Iccode* $x_year), robust cluster(ccode);
regress any_prio gdp_g gdp_g_l Iccode* $x_year (NCEP_g NCEP_g_l Iccode* $x_year), robust cluster(ccode);
regress any_prio gdp_g gdp_g_l Iccode* $x_year (FAO_g FAO_g_l Iccode* $x_year), robust cluster(ccode);



/* TABLE A3: OTHER CONFLICT MEASURES */

regress any_prio gdp_g gdp_g_l Iccode* $x_year (GPCP_g GPCP_g_l Iccode* $x_year), robust cluster(ccode);
regress war_prio gdp_g gdp_g_l Iccode* $x_year (GPCP_g GPCP_g_l Iccode* $x_year), robust cluster(ccode);
regress war_col gdp_g gdp_g_l Iccode* $x_year (GPCP_g GPCP_g_l Iccode* $x_year), robust cluster(ccode);
regress war_inc gdp_g gdp_g_l Iccode* $x_year (GPCP_g GPCP_g_l Iccode* $x_year), robust cluster(ccode);
regress war gdp_g gdp_g_l Iccode* $x_year (GPCP_g GPCP_g_l Iccode* $x_year), robust cluster(ccode);



log c;
exit;

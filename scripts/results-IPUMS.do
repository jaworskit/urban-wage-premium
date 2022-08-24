********************************************************************************
* results-IPUMS.do
*
* This file replicates Boustan's Urban Wage gap figure in the Urbanization in the United States paper. Then it extends this to include a market access variable.
********************************************************************************

cls
clear all

********************************************************************************

global project "/Users/taylorjaworski/Dropbox/Research/Papers/UrbanWagePremium/"
global gh "/Users/taylorjaworski/Github/urban-wage-premium"
if c(username) == "kylebutts" {
	global project "/Users/kylebutts/Dropbox/UrbanWagePremium"
	global gh "/Users/kylebutts/Documents/Projects/urban-wage-premium"
}

********************************************************************************

use "$project/data/dta/urban_wage_final.dta", clear
* sample 4
* save "$project/data/dta/urban_wage_final_sample.dta", replace 
* use "$project/data/dta/urban_wage_final_sample.dta", clear

********************************************************************************
* Results
********************************************************************************

*-> Replicate Boustan et al figure's Urban Wage Premium Estimate

	display "** Boustan et al Replication *****************************"

	preserve

	collapse (sum) totalweeks totalincome perwt, by(year urban)

	qui g weeklywage = totalincome/perwt

	reshape wide weeklywage totalweeks totalincome perwt, i(year) j(urban)

	qui g urbanpremium = log(weeklywage1 / weeklywage0)
	
	foreach year of numlist 1940 1950 1960 1970 1980 1990 2000 2010 2020 {
		qui sum urbanpremium if year == `year'
		scalar urban_est0_`year' = `=r(mean)'
	}

	restore

*-> Regression based estimates
	
	foreach year of numlist 1940 1950 1960 1970 1980 1990 2000 2010 2020 {
		qui g urban_`year' = urban*(year==`year')
	}

	***** Urban ****************************************************************
	
	qui reghdfe ln_weeklywage urban_* [aw=perwt], cluster(metarea) a(year)

	foreach year of numlist 1940 1950 1960 1970 1980 1990 2000 2010 2020 {
		scalar urban_est1_`year' = _b[urban_`year']
	}
	
	qui eststo est1
	estadd local cov ""
	estadd local group ""

	***** Urban, & Controls ****************************************************

	qui reghdfe ln_weeklywage urban_* [aw=perwt], cluster(metarea) a(year  agegroup educ white)

	foreach year of numlist 1940 1950 1960 1970 1980 1990 2000 2010 2020 {
		scalar urban_est2_`year' = _b[urban_`year']
	}
	
	qui eststo est2
	estadd local cov "X"
	estadd local group ""

	***** Urban, Controls, & Group Averages *********************
	
	qui reghdfe ln_weeklywage urban_* ln_ma_removeown (c.share_*)#i.year c.rent_avg#i.year [aw=perwt], cluster(metarea) a(statefip year agegroup educ white) noconstant

	foreach year of numlist 1940 1950 1960 1970 1980 1990 2000 2010 2020 {
		scalar urban_est3_`year' = _b[urban_`year']
	}
		
	qui eststo est3
	estadd local cov "X"
	estadd local group "X"

	****************************************************************************

*-> Display results

	if(1 == 1){
		foreach year of numlist 1940 1950 1960 1970 1980 1990 2000 2005 2010 2015 {
			disp "** `year' ******************************************"
			disp "** urban raw: `= urban_est0_`year''"
			disp "** urban only: `= urban_est1_`year''"
			disp "** urban w/ controls: `= urban_est2_`year''"
			disp "** urban w/ market access: `= urban_est3_`year''"
			disp ""
			disp "** % premium explained by ma: `= (urban_est2_`year' - urban_est3_`year')/(urban_est2_`year')'"
			disp ""
		}
	}


*-> Create dataset of point estimates

	preserve
	drop _all
	set obs 10
	qui g year = .
	qui replace year = 1940 if _n==1
	qui replace year = 1950 if _n==2
	qui replace year = 1960 if _n==3
	qui replace year = 1970 if _n==4
	qui replace year = 1980 if _n==5
	qui replace year = 1990 if _n==6
	qui replace year = 2000 if _n==7
	qui replace year = 2005 if _n==8
	qui replace year = 2010 if _n==9
	qui replace year = 2015 if _n==10
	qui g urban_est0 = .
	qui g urban_est1 = .
	qui g urban_est2 = .
	qui g urban_est3 = .
	qui g urban_est4 = .
	foreach year of numlist 1940 1950 1960 1970 1980 1990 2000 2005 2010 2015 {
		qui replace urban_est0 = `=urban_est0_`year'' if year==`year'
		qui replace urban_est1 = `=urban_est1_`year'' if year==`year'
		qui replace urban_est2 = `=urban_est2_`year'' if year==`year'
		qui replace urban_est3 = `=urban_est3_`year'' if year==`year'
		qui replace urban_est4 = `=urban_est4_`year'' if year==`year'
	}

	* Export point estimates
	save "$project/data/dta/results-IPUMS.dta", replace 


*-> Figure: Urban Premium over time

	gr tw 	(scatter urban_est0 year, c(l) m(s) lc(gs10) mfc(white) mc(gs10) lp(dash)) ///
			(scatter urban_est1 year, c(l) m(d) lc(black) mfc(white) mc(black) lp(dash)) ///
			(scatter urban_est2 year, c(l) m(t) lc(black) mfc(white) mc(black) lp(dash)) ///
			(scatter urban_est3 year, c(l) m(o) lc(black) mfc(black) mc(black) lp(solid)) ///
			(scatter urban_est4 year, c(l) m(x) lc(black) mfc(black) mc(black) lp(solid)) ///
			, graphregion(color(white)) ylabel(0(.1).3,nogrid labsize(small) format(%12.1fc)) yscale(range(0 .35)) ///
			legend(region(lcolor(white)) rows(1) size(tiny) ///
			order(1 "urban raw" 2 "urban only" 3 "urban w/ controls" 4 "urban w/ market access" 5 "urban w/ ") /// 
			title("{bf:Urban Wage Premium:}", ///
			size(tiny) pos(9) color(black))) xtitle("") ///
			l1title("Urban Wage Premium (in percent)", size(small))

	gr export "$gh/paper/figures/urbanpremium_IPUMS.pdf",as(pdf) replace
	
	restore


	* Export results to table
	if(1 == 1){
		estout est* ///
			using "$gh/paper/tables/results-IPUMS/premium.tex" /// 
			, replace type style(tex) collabels(none) mlabels(none) eqlabels(none) varwidth(40) cells(b(fmt(4)) se(par fmt(4))) ///
			stats(cov ma group N, fmt(%12.0fc) labels("\hline Individual Covariates" "Market Access" "Group Averages" "\hline N" )) /// 
			keep(urban_*) varlabels(urban_1940 "Urban $\times$ 1940" urban_1950 "Urban $\times$ 1950" urban_1960 "Urban $\times$ 1960" urban_1970 "Urban $\times$ 1970" urban_1980 "Urban $\times$ 1980" urban_1990 "Urban $\times$ 1990" urban_2000 "Urban $\times$ 2000" urban_2005 "Urban $\times$ 2005" urban_2010 "Urban $\times$ 2010" urban_2015 "Urban $\times$ 2015")
	}

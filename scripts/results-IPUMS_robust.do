********************************************************************************
* results-IPUMS_robust.do
*
* This file runs robustness checks for results-IPUMS.do
*
* Files to Run Before: 
*   - do/data-marketaccess.do 
********************************************************************************

cls
clear all

********************************************************************************

*global project "/Users/taylorjaworski/Dropbox/Papers/EH/RegionalDevelopment/transportation/UrbanWagePremium/"
*global gh "/Users/taylorjaworski/Documents/GitHub/urban-wage-premium/"
global project "/Users/kylebutts/Dropbox/UrbanWagePremium"
global gh "/Users/kylebutts/Documents/Projects/urban-wage-premium"
********************************************************************************
	
********************************************************************************
* Robustness Check: MA averages weighted by population
********************************************************************************	

est clear
use "$project/data/dta/urban_wage_with_ma.dta", clear

*-> Replicate Boustan figure
	
	display "** Boustan Replication *****************************"
	
	preserve
	
	collapse (sum) totalweeks totalincome perwt, by(year urban)

	qui g weeklywage = totalincome/perwt
	
	reshape wide weeklywage totalweeks totalincome perwt, i(year) j(urban)
	
	qui g urbanpremium = log(weeklywage1 / weeklywage0)
	
	foreach year of numlist 1940 1950 1960 1970 1980 1990 2000 2005 2010 2015 {
		qui sum urbanpremium if year == `year'
		scalar urban_est0_`year' = `=r(mean)'
	}
		
	*gr tw (scatter urbanpremium year, c(l) lc(black) mfc(white) mc(black))
	*gr export "$gh/paper/figures/urbanpremium_IPUMS.pdf",as(pdf) replace
	
	restore
	
*-> Robustness Check: MA averages weighted by population

	foreach year of numlist 1940 1950 1960 1970 1980 1990 2000 2005 2010 2015 {
		
		display "** `year' ******************************************"
		
			qui reg ln_weeklywage urban [aw=perwt] if year==`year', cluster(metarea)
			qui eststo est1_`year'
			scalar urban_est1_`year' = _b[urban]
			
			qui reghdfe ln_weeklywage urban [aw=perwt] if year==`year', cluster(metarea) a(agegroup educ white)
			qui eststo est2_`year'
			scalar urban_est2_`year' = _b[urban]
			
			qui reghdfe ln_weeklywage urban ln_ma_weighted [aw=perwt] if year==`year', cluster(metarea) a(agegroup educ white)
			qui eststo est3_`year'
			scalar urban_est3_`year' = _b[urban]

		display "**************************************************"
		
	}
		
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
	foreach year of numlist 1940 1950 1960 1970 1980 1990 2000 2005 2010 2015 {
		qui replace urban_est0 = `=urban_est0_`year'' if year==`year'
		qui replace urban_est1 = `=urban_est1_`year'' if year==`year'
		qui replace urban_est2 = `=urban_est2_`year'' if year==`year'
		qui replace urban_est3 = `=urban_est3_`year'' if year==`year'	
	}
		
	gr tw 	(scatter urban_est0 year, c(l) m(s) lc(gs10) mfc(white) mc(gs10) lp(dash)) ///
			(scatter urban_est1 year, c(l) m(d) lc(black) mfc(white) mc(black) lp(dash)) ///
			(scatter urban_est2 year, c(l) m(t) lc(black) mfc(white) mc(black) lp(dash)) ///
			(scatter urban_est3 year, c(l) m(o) lc(black) mfc(black) mc(black) lp(solid)) ///
			, graphregion(color(white)) ylabel(0(.1).3,nogrid labsize(small) format(%12.1fc)) yscale(range(0 .35)) ///
			legend(region(lcolor(white)) rows(1) size(tiny) /// 
			order(1 "urban raw" 2 "urban only" 3 "urban w/ controls" 4 "urban w/ weighted market access") /// 
			title("{bf:Urban Wage Premium:}", ///
			size(tiny) pos(9) color(black))) xtitle("") l1title("Urban Wage Premium (in percent)", size(small))
			
	gr export "$gh/paper/figures/urbanpremium_IPUMS_ma_weighted.pdf", as(pdf) replace
	restore	
	
	


********************************************************************************
* Robustness Check: MSA selection over time
********************************************************************************	
	
est clear
use "$project/data/dta/urban_wage_with_ma.dta", clear
	

*-> 2015 CBSA to MSA
	merge m:1 code using "$project/data/urbanareas/cbsa2015_to_msa1950.dta"
	replace code = msa if ~missing(msa)
	drop if _merge == 2
	drop msa _merge

*-> Get Indicator for being in MSA for entire time
	preserve
	
	drop if missing(year)
	collapse (first) urban, by(year code)
	bys code: egen n = count(year)
	sort code year
	bys code: keep if _n == 1 & n == 10
	drop year urban n 
	qui g in_all = 1
	
	save "$project/data/temp/in_all_yrs.dta", replace
	
	restore
	
	merge m:1 code using "$project/data/temp/in_all_yrs"
	
*-> Keep only MSAs in all or non-msa 
	
	keep if in_all == 1 | code == 0
	
*-> Replicate Boustan figure
	
	display "** Boustan Replication *****************************"
	
	preserve
	
	collapse (sum) totalweeks totalincome perwt, by(year urban)

	qui g weeklywage = totalincome/perwt
	
	reshape wide weeklywage totalweeks totalincome perwt, i(year) j(urban)
	
	qui g urbanpremium = log(weeklywage1 / weeklywage0)
	
	foreach year of numlist 1940 1950 1960 1970 1980 1990 2000 2005 2010 2015 {
		qui sum urbanpremium if year == `year'
		scalar urban_est0_`year' = `=r(mean)'
	}
		
	*gr tw (scatter urbanpremium year, c(l) lc(black) mfc(white) mc(black))
	*gr export "$gh/paper/figures/urbanpremium_IPUMS.pdf",as(pdf) replace
	
	restore
	
*-> Robustness Check: MSA that are there the whole time

	foreach year of numlist 1940 1950 1960 1970 1980 1990 2000 2005 2010 2015 {
		
		display "** `year' ******************************************"
		
			qui reg ln_weeklywage urban [aw=perwt] if year==`year', cluster(metarea)
			qui eststo est1_`year'
			scalar urban_est1_`year' = _b[urban]
			
			qui reghdfe ln_weeklywage urban [aw=perwt] if year==`year', cluster(metarea) a(agegroup educ white)
			qui eststo est2_`year'
			scalar urban_est2_`year' = _b[urban]
			
			qui reghdfe ln_weeklywage urban ln_ma_weighted [aw=perwt] if year==`year', cluster(metarea) a(agegroup educ white)
			qui eststo est3_`year'
			scalar urban_est3_`year' = _b[urban]

		display "** Finished `year' *******************************"
		
	}
		
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
	foreach year of numlist 1940 1950 1960 1970 1980 1990 2000 2005 2010 2015 {
		qui replace urban_est0 = `=urban_est0_`year'' if year==`year'
		qui replace urban_est1 = `=urban_est1_`year'' if year==`year'
		qui replace urban_est2 = `=urban_est2_`year'' if year==`year'
		qui replace urban_est3 = `=urban_est3_`year'' if year==`year'	
	}
		
	gr tw 	(scatter urban_est0 year, c(l) m(s) lc(gs10) mfc(white) mc(gs10) lp(dash)) ///
			(scatter urban_est1 year, c(l) m(d) lc(black) mfc(white) mc(black) lp(dash)) ///
			(scatter urban_est2 year, c(l) m(t) lc(black) mfc(white) mc(black) lp(dash)) ///
			(scatter urban_est3 year, c(l) m(o) lc(black) mfc(black) mc(black) lp(solid)) ///
			, graphregion(color(white)) ylabel(0(.1).3,nogrid labsize(small) format(%12.1fc)) yscale(range(0 .35)) ///
			legend(region(lcolor(white)) rows(1) size(tiny) /// 
			order(1 "urban raw" 2 "urban only" 3 "urban w/ controls" 4 "urban w/ market access") /// 
			title("{bf:Urban Wage Premium:}", ///
			size(tiny) pos(9) color(black))) xtitle("") l1title("Urban Wage Premium (in percent)", size(small))
			
	gr export "$gh/paper/figures/urbanpremium_IPUMS_msa_in_all_yrs.pdf", as(pdf) replace
	restore	
	
	
	
********************************************************************************
* Robustness Check: MA remove own MSA
********************************************************************************	
	
est clear
use "$project/data/dta/urban_wage_with_ma.dta", clear
	
*-> Replicate Boustan figure
	
	display "** Boustan Replication *****************************"
	
	preserve
	
	collapse (sum) totalweeks totalincome perwt, by(year urban)

	qui g weeklywage = totalincome/perwt
	
	reshape wide weeklywage totalweeks totalincome perwt, i(year) j(urban)
	
	qui g urbanpremium = log(weeklywage1 / weeklywage0)
	
	foreach year of numlist 1940 1950 1960 1970 1980 1990 2000 2005 2010 2015 {
		qui sum urbanpremium if year == `year'
		scalar urban_est0_`year' = `=r(mean)'
	}
		
	*gr tw (scatter urbanpremium year, c(l) lc(black) mfc(white) mc(black))
	*gr export "$gh/paper/figures/urbanpremium_IPUMS.pdf",as(pdf) replace
	
	restore
	
*-> Robustness Check: MSA that are there the whole time

	foreach year of numlist 1940 1950 1960 1970 1980 1990 2000 2005 2010 2015 {
		
		display "** `year' ******************************************"
		
			qui reg ln_weeklywage urban [aw=perwt] if year==`year', cluster(metarea)
			qui eststo est1_`year'
			scalar urban_est1_`year' = _b[urban]
			
			qui reghdfe ln_weeklywage urban [aw=perwt] if year==`year', cluster(metarea) a(agegroup educ white)
			qui eststo est2_`year'
			scalar urban_est2_`year' = _b[urban]
			
			qui reghdfe ln_weeklywage urban ln_ma_removeown [aw=perwt] if year==`year', cluster(metarea) a(agegroup educ white)
			qui eststo est3_`year'
			scalar urban_est3_`year' = _b[urban]

		display "** Finished `year' *******************************"
		
	}
		
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
	foreach year of numlist 1940 1950 1960 1970 1980 1990 2000 2005 2010 2015 {
		qui replace urban_est0 = `=urban_est0_`year'' if year==`year'
		qui replace urban_est1 = `=urban_est1_`year'' if year==`year'
		qui replace urban_est2 = `=urban_est2_`year'' if year==`year'
		qui replace urban_est3 = `=urban_est3_`year'' if year==`year'	
	}
		
	gr tw 	(scatter urban_est0 year, c(l) m(s) lc(gs10) mfc(white) mc(gs10) lp(dash)) ///
			(scatter urban_est1 year, c(l) m(d) lc(black) mfc(white) mc(black) lp(dash)) ///
			(scatter urban_est2 year, c(l) m(t) lc(black) mfc(white) mc(black) lp(dash)) ///
			(scatter urban_est3 year, c(l) m(o) lc(black) mfc(black) mc(black) lp(solid)) ///
			, graphregion(color(white)) ylabel(0(.1).3,nogrid labsize(small) format(%12.1fc)) yscale(range(0 .35)) ///
			legend(region(lcolor(white)) rows(1) size(tiny) /// 
			order(1 "urban raw" 2 "urban only" 3 "urban w/ controls" 4 "urban w/ market access") /// 
			title("{bf:Urban Wage Premium:}", ///
			size(tiny) pos(9) color(black))) xtitle("") l1title("Urban Wage Premium (in percent)", size(small))
			
	gr export "$gh/paper/figures/urbanpremium_IPUMS_removeown.pdf", as(pdf) replace
	restore	
	
	
	
	
	
	
	
	
	
	
*-> Robustness Check: MSA size
est clear
foreach year of numlist 1940 1950 1960 1970 1980 1990 2000 2005 2010 2015 {
	
	display "** `year' ******************************************"
	
		reg ln_weeklywage ln_msasize [aw=perwt] if year==`year', cluster(msa)
		
		reg ln_weeklywage ln_msasize ln_ma [aw=perwt] if year==`year', cluster(msa)
		
		}

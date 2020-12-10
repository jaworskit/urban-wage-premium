********************************************************************************
* results-IPUMS.do
*
* This file replicates Boustan's Urban Wage gap figure in the Urbanization in the United States paper. Then it extends this to include a market access variable. 
*
* Files to Run Before: 
*   - do/data-marketaccess.do 
********************************************************************************

cls
clear all

********************************************************************************

global project "/Users/taylorjaworski/Dropbox/Papers/EH/RegionalDevelopment/transportation/UrbanWagePremium/"
global gh "/Users/taylorjaworski/Projects/urban-wage-premium/"
if c(username) == "kylebutts" {
	global project "/Users/kylebutts/Dropbox/UrbanWagePremium"
	global gh "/Users/kylebutts/Documents/Projects/urban-wage-premium"
}

********************************************************************************

use "$project/data/dta/urban_wage_with_ma.dta", clear
	
********************************************************************************
* Results
********************************************************************************		
	
*-> Summary Table
	
	qui g wage_urban = .
	replace wage_urban = weeklywage if urban == 1
	qui g wage_nonurban = .
	replace wage_nonurban = weeklywage if urban == 0
	
	
	foreach year of numlist 1940 1950 1960 1970 1980 1990 2000 2005 2010 2015 {
		foreach var of varlist urban ln_ma wage_urban wage_nonurban {
			qui sum `var' [aw=perwt] if year == `year'
			
			if "`var'" == "urban" {
				matrix n_`year' = `r(N)'
			}
 			
			matrix mean_`var'_`year' = `r(mean)'
			matrix sd_`var'_`year' = `r(sd)'
		}
		
		matrix year_`year' = `year'
		
		matrix define row_`year' = (n_`year', mean_urban_`year', mean_ln_ma_`year', sd_ln_ma_`year', /// 
			mean_wage_urban_`year', sd_wage_urban_`year', mean_wage_nonurban_`year', sd_wage_nonurban_`year')
	}
	
	matrix define results = (row_1940 \ row_1950 \ row_1960 \ row_1970 \ row_1980 /// 
		\ row_1990 \ row_2000 \ row_2005 \ row_2010 \ row_2015) 
	
	matrix rownames results = 1940 1950 1960 1970 1980 1990 2000 2005 2010 2015
	
	
	esttab matrix(results, fmt(%10.0fc 2 %10.2fc %10.2fc %10.0fc %10.0fc %10.0fc %10.0fc ))  ///
		using "$gh/paper/results/summary_stats/summary.tex" ///
		, replace ///
		tex plain fragment /// 
		nomtitle nonumbers collabel(none) ///
		mlabels(none) eqlabels(none)
		
		
*-> Replicate Boustan et al figure's Urban Wage Premium Estimate
	
	display "** Boustan et al Replication *****************************"
	
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
	
	
*-> Regression-based Urban Wage Premium estimates
	
	*est clear
	foreach year of numlist 1940 1950 1960 1970 1980 1990 2000 2005 2010 2015 {
		
		display "** `year' ******************************************"
		
			qui reg ln_weeklywage urban [aw=perwt] if year==`year', cluster(metarea)
			qui eststo est1_`year'
			scalar urban_est1_`year' = _b[urban]
			
			qui reghdfe ln_weeklywage urban [aw=perwt] if year==`year', cluster(metarea) a(agegroup educ white)
			qui eststo est2_`year'
			scalar urban_est2_`year' = _b[urban]
			
			qui reghdfe ln_weeklywage urban ln_ma [aw=perwt] if year==`year', cluster(metarea) a(agegroup educ white)
			qui eststo est3_`year'
			scalar urban_est3_`year' = _b[urban]

		display "**************************************************"
		
	}
		
	* Display estimates and proportion of premium explained by market access
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
		
*-> Figure: Urban Premium over time

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
			legend(region(lcolor(white)) rows(1) size(tiny) order(1 "urban raw" 2 "urban only" ///
			3 "urban w/ controls" 4 "urban w/ market access") title("{bf:Urban Wage Premium:}", ///
			size(tiny) pos(9) color(black))) xtitle("") l1title("Urban Wage Premium (in percent)", size(small))
	gr export "$gh/paper/figures/urbanpremium_IPUMS.pdf",as(pdf) replace
	restore

	
	* Export results to table
	if(1 == 1){		
		estout est1* using "$gh/paper/results/results-IPUMS/premium_no_ma.tex" ///
			, replace type style(tex) collabels(none) mlabels(none) eqlabels(none) ///
			varwidth(25) cells(b(fmt(4)) se(par fmt(4))) stats(N, fmt(%12.0fc) labels("\hline N")) /// 
			keep(urban) varlabels(urban "Urban")	
		
		estout est2* using "$gh/paper/results/results-IPUMS/premium_no_ma_controls.tex" ///
			, replace type style(tex) collabels(none) mlabels(none) eqlabels(none) ///
			varwidth(25) cells(b(fmt(4)) se(par fmt(4))) stats(N, fmt(%12.0fc) labels("\hline N")) /// 
			keep(urban) varlabels(urban "Urban")	

		 estout est3* using "$gh/paper/results/results-IPUMS/premium_ma.tex" ///
			, replace type style(tex) collabels(none) mlabels(none) eqlabels(none) ///
			varwidth(10) modelwidth(8) cells(b(fmt(3)) se(par fmt(3))) stats(N, fmt(%12.0fc) labels("\hline N")) /// 
			keep(urban ln_ma) varlabels(urban "Urban" ln_ma "$\log(MA)$")	
	}
	
	
	
			
	

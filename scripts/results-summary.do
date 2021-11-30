********************************************************************************
* results-summary.do
*
* Summary table of variables
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
		
********************************************************************************
* Results
********************************************************************************

*-> Summary Table of urban indicator, log(remove own MA), wage_urban, and wage_nonurban

	qui g wage_urban = .
	replace wage_urban = weeklywage if urban == 1
	qui g wage_nonurban = .
	replace wage_nonurban = weeklywage if urban == 0


	foreach year of numlist 1940 1950 1960 1970 1980 1990 2000 2005 2010 2015 {
		foreach var of varlist urban ln_ma_remove_own wage_urban wage_nonurban {
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

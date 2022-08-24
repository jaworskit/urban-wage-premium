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

* Temp for code editing
* bys year urban: keep if _n < 5000

		
********************************************************************************
* Results
********************************************************************************

*-> Summary Table of urban indicator, log(remove own MA), wage_urban, and wage_nonurban

	qui g college_degree = (educ >= 10)

	foreach year of numlist 1940 1950 1960 1970 1980 1990 2000 2010 2020 {
  * foreach year of numlist 1940 {
    qui sum urban [aw=perwt] if year == `year'
    matrix frac_urban_`year' = `r(mean)' * 100

    qui sum weeklywage [aw=perwt] if year == `year' & urban == 1
    matrix mean_weeklywage_urban_`year' = `r(mean)'
    qui sum weeklywage [aw=perwt] if year == `year' & urban == 0
    matrix mean_weeklywage_nonurban_`year' = `r(mean)'
    matrix premia_`year' = (mean_weeklywage_urban_`year'[1,1] / mean_weeklywage_nonurban_`year'[1,1] - 1) * 100

    * Fraction of people with college degree
    qui sum college_degree [aw=perwt] if year == `year'
    matrix frac_college_`year' = `r(mean)' * 100

    * Fraction of people with college degree in Urban Areas
    qui sum college_degree [aw=perwt] if year == `year' & urban == 1
    matrix frac_college_urban_`year' = `r(mean)' * 100

    * Fraction of people with college degree in Non-urban
    qui sum college_degree [aw=perwt] if year == `year' & urban == 0
    matrix frac_college_nonurban_`year' = `r(mean)' * 100

		matrix define row_`year' = (premia_`year', frac_urban_`year', frac_college_`year', frac_college_urban_`year', frac_college_nonurban_`year')
	}

	matrix define results = (row_1940 \ row_1950 \ row_1960 \ row_1970 \ row_1980 \ row_1990 \ row_2000 \ row_2010 \ row_2020)

	matrix rownames results = 1940 1950 1960 1970 1980 1990 2000 2010 2020


	esttab matrix(results, fmt(1 1 1 1 1))  ///
		using "$gh/paper/tables/summary_stats/summary.tex" ///
		, replace ///
		tex plain fragment ///
		nomtitle nonumbers collabel(none) ///
		mlabels(none) eqlabels(none)

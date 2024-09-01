********************************************************************************
* marketaccess.do
*
* This file combines market access variables from matlab into a dataset
* Must run after metarea.do, IPUMS-compile.do
********************************************************************************
cls
clear all

global project "/Users/taylorjaworski/Dropbox/Research/Papers/UrbanWagePremium/"
global gh "/Users/taylorjaworski/Github/urban-wage-premium"
if c(username) == "kylebutts" {
	global project "/Users/kylebutts/Dropbox/UrbanWagePremium"
	global gh "/Users/kylebutts/Documents/Projects/urban-wage-premium"
}
********************************************************************************

*-> Load market access 
	
	foreach year in 1940 1950 1960 1970 1980 1990 2000 2010 2020 {
		
		if `year' == 2020 {
			qui import delimited using "$project/data/matlab/output/MA2010_cost1.csv", clear
		} 
		else {
			qui import delimited using "$project/data/matlab/output/MA`year'_cost1.csv", clear
		}
		
		qui rename v1 fips
		qui rename v2 ma 
		qui gen year = `year'
		
		qui save "$project/data/marketaccess/dta/MA`year'.dta", replace
	}
	
	clear 
	foreach year in 1940 1950 1960 1970 1980 1990 2000 2010 2020  {
		qui append using "$project/data/marketaccess/dta/MA`year'.dta"
	}
	
	qui sort fips year
	duplicates drop fips year ma, force
	
	qui gen statefip = floor(fips/1000)
	
	qui save "$project/data/marketaccess/dta/MA_allyears.dta", replace

	
	
*-> Load market access (robustness: remove own-MSA gdp)
	
	foreach year in 1940 1950 1960 1970 1980 1990 2000 2010 2020 {
		
		if `year' == 2005 {
			qui import delimited using "$project/data/matlab/output/MA2000_cost1_removeown.csv", clear
		} 
		else if `year' == 2020 {
			qui import delimited using "$project/data/matlab/output/MA2010_cost1_removeown.csv", clear
		} 
		else {
			qui import delimited using "$project/data/matlab/output/MA`year'_cost1_removeown.csv", clear
		}
		
		qui rename v1 fips
		qui rename v2 ma_removeown 
		qui gen year = `year'
		
		qui save "$project/data/marketaccess/dta/MA`year'_removeown.dta", replace
	}
	
	clear 
	foreach year in 1940 1950 1960 1970 1980 1990 2000 2010 2020 {
		qui append using "$project/data/marketaccess/dta/MA`year'_removeown.dta"
	}
	
	qui sort fips year
	duplicates drop fips year ma_removeown, force
	
	qui gen statefip = floor(fips/1000)
	
	qui save "$project/data/marketaccess/dta/MA_allyears_removeown.dta", replace
	
	
	
	
*-> Load county population

	clear

	foreach year in 1940 1950 1960 1970 1980 1990 2000 2010 2020 {
		qui append using "$project/data/population/population_`year'.dta"
	}

	label variable population "Population in year"
	
	* drop Hawaii and Alaska
	qui drop if floor(fips/1000) == 2 | floor(fips/1000) == 15
  * drop statewide numbers
  qui drop if mod(fips, 1000) == 0 & year == 2020
  
  * DC Statewide in 1960 not needed, in 1950 should be 11001 (county)
  qui drop if fips == 11000 & year == 1960
  qui replace fips = 11001 if fips == 11000 & year == 1950
  
  * Dade county becomes Miami-Dade County
  replace fips = 12086 if fips == 12025

	qui save "$project/data/temp/population_1940_2020.dta", replace
	
*-> MA county to MSA crosswalk

	* Merge with County FIPS to MSA Crosswalk
	merge 1:m fips year using "$project/data/urbanareas/metarea_final.dta"
	gen statefip = floor(fips/1000)
	
	* _merge == 2 is because there are many counties without an MSA
	drop _merge
		
	* Merge with county-level market access	
	merge m:1 fips year using "$project/data/marketaccess/dta/MA_allyears.dta"
	drop _merge
	
	* A few fips codes are missing ma (330 obs.; Virginia is messed up)
	drop if missing(ma)
	
	* Merge with county-level market access (robustness remove own)
	merge m:1 fips year using "$project/data/marketaccess/dta/MA_allyears_removeown.dta"
	drop _merge
	
	* A few fips codes are missing ma (330 obs.; Virginia is messed up)
	drop if missing(ma_removeown)
	
	* For non-MSA counties, code = statefip when we collpase by code
	replace code = statefip*100000 if missing(code)
	
	** Take mean within msa and within rural-state
	* Without weighting by population
	preserve
    qui collapse (mean) ma ma_removeown, by(code year)
    qui save "$project/data/temp/pop_unweighted.dta", replace
	restore 
	
	* Weighting by population 
	qui collapse (mean) ma ma_removeown [fw = population], by(code year)
	rename ma ma_weighted
	rename ma_removeown ma_weighted_removeown
	
	* Merge msa weighted and unweighted
	merge 1:1 code year using "$project/data/temp/pop_unweighted.dta"
	drop _merge
	rm "$project/data/temp/pop_unweighted.dta"
	
	label variable ma ///
    "Market Access (averaged across counties)"
	label variable ma_weighted ///
    "Market Access (averaged across counties, weighted by cnty population)"
	label variable ma_removeown ///
    "Market Access (averaged across counties, removed own-MSA)"
	label variable ma_weighted_removeown ///
    "Market Access (averaged across counties, weighted by cnty population, removed own-MSA)"
	
  keeporder code year ma ma_weighted ma_removeown ma_weighted_removeown
	sort code year
	qui replace code = 0 if code == .	
	
*-> Merge in lat/long Data for MSAs and State
	* Some missing because MSA shape files from IPUMS don't match perfectly
	merge m:1 year code using "$project/data/crosswalk/msa_lat_long.dta"
	drop _merge

	merge m:1 code using "$project/data/crosswalk/state_lat_long.dta"
	drop _merge
	drop if code == .

	
*-> Add 1940 and 1950 population
  * Calculate above and below- median population cities
  * Data from https://www2.census.gov/library/publications/decennial/1950/pc-03/pc-3-03.pdf
  preserve 
    insheet using "$project/data/urbanareas/metarea_population_1940_1950.csv", clear
    egen pop_1940_rank = rank(pop_1940)
    gen top20 = pop_1940_rank <= 20
    gen code = metarea
    keep code top20 pop_1940

    tempfile population
    save `population'
  restore
  merge m:1 code using `population'
  drop if _merge == 2
  drop _merge

*-> Export
  drop if year == 2015 | year == 2005 | year == 2020
	qui save "$project/data/dta/msa_market_access.dta", replace
	

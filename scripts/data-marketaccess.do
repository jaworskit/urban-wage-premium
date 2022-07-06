********************************************************************************
* data-marketaccess.do
*
* This file combines market access variables from matlab into a dataset
* Must run after data-metarea.do, data-IPUMS-compile.do
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


*-> Load market access 
	
	foreach year in 1940 1950 1960 1970 1980 1990 2000 2010 2020 {
		
		if `year' == 2005 {
			qui import delimited using "$project/data/matlab/output/MA2000_cost1.csv", clear
		} 
		else if `year' == 2020 {
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
	drop state county
	
	qui save "$project/data/dta/temp/population_1940_2020.dta", replace
	
	
	
	
*-> MA county to MSA crosswalk

	qui use "$project/data/dta/temp/population_1940_2020.dta", clear
	
	* rm "$project/dta/temp/population_1940_2020.dta"

	* Merge with County FIPS to MSA Crosswalk
	merge 1:m fips year using "$project/data/urbanareas/metarea_final.dta"
	replace statefip = floor(fips/1000) if missing(statefip)
	
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
	qui save "$project/data/dta/temp/pop_unweighted.dta", replace
	restore 
	
	* Weighting by population 
	qui collapse (mean) ma ma_removeown [fw= population], by(code year)
	rename ma ma_weighted
	rename ma_removeown ma_weighted_removeown
	
	* Merge msa weighted and unweighted
	merge 1:1 code year using "$project/data/dta/temp/pop_unweighted.dta"
	drop _merge
	rm "$project/data/dta/temp/pop_unweighted.dta"
	
	label variable ma "Market Access (averaged across counties)"
	label variable ma_weighted "Market Access (averaged across counties, weighted by cnty population)"
	label variable ma_removeown "Market Access (averaged across counties, removed own-MSA)"
	label variable ma_weighted_removeown "Market Access (averaged across counties, weighted by cnty population, removed own-MSA)"
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

  drop if year == 2005 | year == 2015
	
	* Save
	qui save "$project/data/dta/msa_market_access.dta", replace

	
	

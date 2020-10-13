********************************************************************************
* data-marketaccess.do
*
* This file combines market access variables from matlab code with IPUMS Survye Data from 1940-2015
*
* Files to Run Before: 
*   - matlab/FindMA.m
*   - do/data-metarea.do
********************************************************************************

cls
clear all

********************************************************************************

* global project "/Users/taylorjaworski/Dropbox/Papers/EH/RegionalDevelopment/transportation/UrbanWagePremium"
 global project "/Users/kylebutts/Dropbox/UrbanWagePremium"

********************************************************************************


*-> Load market access 
	
	foreach year in "1940" "1950" "1960" "1970" "1980" "1990" "2000" "2005" "2010" "2015" {
		
		if `year' == 2005 {
			qui import delimited using "$project/data/matlab/output/MA2000_cost1.csv", clear
		} 
		else if `year' == 2015 {
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
	foreach year in "1940" "1950" "1960" "1970" "1980" "1990" "2000" "2005" "2010" "2015" {
		qui append using "$project/data/marketaccess/dta/MA`year'.dta"
	}
	
	qui sort fips year
	duplicates drop fips year ma, force
	
	gen statefip = floor(fips/1000)
	
	qui save "$project/data/marketaccess/dta/MA_allyears.dta", replace
	
	
*-> Load county population

	clear

	foreach year in "1940" "1950" "1960" "1970" "1980" "1990" "2000" "2005" "2010" "2015" {
		qui append using "$project/data/population/population_`year'.dta"
	}
	
	label variable population "Population in year"
	
	* drop Hawaii and Alaska
	qui drop if floor(fips/1000) == 2 | floor(fips/1000) == 15
	
	qui save "$project/data/dta/temp/population_1940_2010.dta", replace
	
	
*-> MA county to MSA crosswalk

	qui use "$project/data/dta/temp/population_1940_2010.dta", clear
	* rm "$project/dta/temp/population_1940_2010.dta"

	* Merge with County FIPS to MSA Crosswalk
	merge 1:m fips year using "$project/data/urbanareas/metarea_final.dta"
	
	* _merge == 2 is because there are many counties without an MSA
	drop _merge
		
	* Merge with county-level market access	
	merge m:1 fips year using "$project/data/marketaccess/dta/MA_allyears.dta"
	
	* A few fips codes are missing ma (330 obs.; Virginia is messed up)
	drop if missing(ma)
	
	* For missing, code = statefip when we collpase by code
	replace code = statefip*100000 if missing(code)

	
	** Take mean within msa and within rural-state
	* Without weighting by population
	preserve
	qui collapse (mean) ma, by(code year)
	qui save "$project/data/dta/temp/pop_unweighted.dta", replace
	restore 
	
	* Weighting by population 
	qui collapse (mean) ma [fw= population], by(code year)
	rename ma ma_weighted
	
	* Merge msa weighted and unweighted
	merge 1:1 code year using "$project/data/dta/temp/pop_unweighted.dta"
	drop _merge
	rm "$project/data/dta/temp/pop_unweighted.dta"
	
	label variable ma "Market Access (averaged across counties)"
	label variable ma_weighted "Market Access (averaged across counties, weighted by cnty population)"
	keeporder code year ma ma_weighted
	sort code year
	
	qui replace code = 0 if code == .
	
	qui save "$project/data/dta/msa_market_access.dta", replace
	

*-> Load Urban Wage data 

	* Prepare CBSA to MSA crosswalk
	* import delimited "$project/data/crosswalk/crosswalk_cbsa_to_msa.csv", clear
	* save "$project/data/crosswalk/crosswalk_cbsa_to_msa.dta", replace

	qui use "$project/data/dta/urban_wage_premium_data.dta", clear 
	
	* to merge with market access, need correct MSA codes
	qui gen metarea_name = metarea 
	qui replace metarea = metarea * 10 if year <= 2010
	qui gen code = metarea
	
	* a few adjustments for MSA codes
	* Kentucky part of Hamilton is in 1640, not 3200
	qui replace code = 1640 if statefip == 21 & code == 3200
	* Kentucky part of Hamilton is in 1640, not 3200
	qui replace code = 1640 if statefip == 39 & code == 3200
	* 2000, Dutchess County is 2281, not 2280
	qui replace code = 2281 if code == 2280
	* Rocky Mount, NC is 6895, not 6890
	qui replace code = 6895 if code == 6890
	* Bridgeport, CT not in Texas 
	qui replace code = 0 if statefip == 48 & code == 1160
		
	* Non-existant
	* Mississippi 3300 non-existant
	qui replace code = 0 if code == 3300
	* Colorado 3010 non-existant
	qui replace code = 0 if code == 3010
	* 2940 non-existant
	qui replace code = 0 if year == 1940 & statefip == 25 & code == 2940 & year == 1940
	* 1960, Gary was it's own msa
	qui replace code = 1602 if year == 1960 & statefip == 18 & code == 1600
	* 1980, Longbranch was not an msa
	qui replace code = 0 if year == 1980 & code == 4410
	* 1990 only Vancouver WA had its own msa
	qui replace code = 6441 if year == 1990 & statefip == 53 & code == 6440
	
	
	replace code = statefip*100000 if code == 0
	merge m:1 year code using "$project/data/dta/msa_market_access.dta"
	
	* Drop non-merged
	drop if _merge ~= 3 
	drop _merge
	
	* Fix code 
	replace code = 0 if code >= 100000
	* CT has no missing counties after 1980, so non-msa's CT has to be dropped
	drop if statefip == 09 & code == 0 & year >= 1980

*->	Export survey data with MA variable
	save "$project/data/dta/urban_wage_with_ma.dta", replace 



	
	



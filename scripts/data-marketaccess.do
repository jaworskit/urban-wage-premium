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
			qui import delimited using "$project/matlab/output/MA2000_cost1.csv", clear
		} 
		else if `year' == 2015 {
			qui import delimited using "$project/matlab/output/MA2010_cost1.csv", clear
		} 
		else {
			qui import delimited using "$project/matlab/output/MA`year'_cost1.csv", clear
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

	foreach year in "1940" "1950" "1960" "1970" "1980" "1990" "2000" "2010" {
		qui append using "$project/data/population/population_`year'.dta"
	}
	
	label variable population "Population in year"
	
	* drop Hawaii and Alaska
	qui drop if floor(fips/1000) == 2 | floor(fips/1000) == 15
	
	qui save "$project/dta/temp/population_1940_2010.dta", replace
	
	
*-> MA county to MSA crosswalk

	qui use "$project/data/urbanareas/metarea_final.dta", clear 
	
	gen statefip = floor(fips/1000)
	
	* drop Hawaii and Alaska
	qui drop if statefip == 2 | statefip == 15
	
	* Merge with county-level market access	
	merge m:1 fips year using "$project/data/marketaccess/dta/MA_allyears.dta"
		
	* _merge == 2 is because there are many counties without an MSA
	drop _merge
	
	
	* Merge with county-level population
	merge m:1 fips year using "$project/dta/temp/population_1940_2010.dta"
	* rm "$project/dta/temp/population_1940_2010.dta"
	
	* A few fips codes are missing ma (171 obs.; Virginia is messed up)
	drop if missing(ma)
	
	* For missing, code = statefip when we collpase by code
	replace code = statefip*100000 if missing(code)

	
	** Take mean within msa and within rural-state
	* Without weighting by population
	preserve
	qui collapse (mean) ma (first) metarea, by(code year)
	qui save "$project/dta/temp/pop_unweighted.dta", replace
	restore 
	
	* Weighting by population 
	qui collapse (mean) ma (first) metarea [fw= population], by(code year)
	rename ma ma_weighted
	
	* Merge msa weighted and unweighted
	merge 1:1 code year using "$project/dta/temp/pop_unweighted.dta"
	drop _merge
	rm "$project/dta/temp/pop_unweighted.dta"
	
	label variable ma "Market Access (averaged across counties)"
	label variable ma_weighted "Market Access (averaged across counties, weighted by cnty population)"
	keeporder code year ma ma_weighted
	sort code year
	
	qui replace code = 0 if code == .
	
	qui save "$project/dta/msa_market_access.dta", replace
	

*-> Load Urban Wage data 

	* Prepare CBSA to MSA crosswalk
	* import delimited "$project/data/crosswalk/crosswalk_cbsa_to_msa.csv", clear
	* save "$project/data/crosswalk/crosswalk_cbsa_to_msa.dta", replace

	qui use "$project/dta/urban_wage_premium_data.dta", clear 
	
	* to merge with market access, need correct MSA codes
	qui gen metarea_name = metarea 
	qui replace metarea = metarea * 10 if year != 2015
	qui gen code = metarea
	
	
	* 2015 CBSA to MSA
	merge m:1 metarea using "$project/data/crosswalk/crosswalk_cbsa_to_msa.dta"
	qui drop _merge
	qui replace code = msa if year == 2015 & ~missing(msa)
	qui replace code = 0 if missing(code)
	drop csa msa 
	
	
	* A few adjustments to cbsa -> msa
	* Newark NJ, correct MSA is 5405, not 5640
	qui replace code = 5605 if year == 2015 & code == 5640
	* New York City being marked as Newark
	qui replace code = 5600 if year == 2015 & code == 5605 & statefip == 36
	* Bangore ME, correct MSA is 730, not 733
	qui replace code = 730 if year == 2015 & code == 733
	* Barnstable MA, correct MSA is 740, not 743
	qui replace code = 740 if year == 2015 & code == 743
	* Boston MA, correct MSA is 1120, not 1123
	qui replace code = 1120 if year == 2015 & code == 1123
	* Boston MA not in CT
	qui replace code = 0 if year == 2015 & code == 1120 & statefip == 9
	* Boston MA not in Rhode Island
	qui replace code = 0 if year == 2015 & code == 1120 & statefip == 44
	* Hartford CT, correct MSA is 3280, not 3283
	qui replace code = 3280 if year == 2015 & code == 3283
	* New Haven CT, correct MSA is 5480, not 5483
	qui replace code = 5480 if year == 2015 & code == 5483
	* Norwich CT, correct MSA is 5520, not 5523
	qui replace code = 5520 if year == 2015 & code == 5523
	* Bristol, CT is Hartford CT 
	qui replace code = 3280 if code == 3281
	* Elkhart IN, correct MSA is 2320, not 2330
	qui replace code = 2320 if year == 2015 & code == 2330
	* Jacksonville, FL, correct MSA is 3590, not 3600
	qui replace code = 3590 if year == 2015 & code == 3600
	* Naples, FL, correct MSA is 5340, not 5345
	qui replace code = 5340 if year == 2015 & code == 5345
	* Rocky Mount, NC, correct MSA is 6890, not 6895
	qui replace code = 6890 if year == 2015 & code == 6895
	* Salem and Gloucester not around in 2015
	qui replace code = 0 if year == 2015 & statefip == 33 & code == 1123
	
	
	* a few adjustments for MSA codes
	* Kentucky part of Hamilton is in 1640, not 3200
	qui replace code = 1640 if statefip == 21 & code == 3200
	* Kentucky part of Hamilton is in 1640, not 3200
	qui replace code = 1640 if statefip == 39 & code == 3200
	* 2000, Dutchess County is 2281, not 2280
	qui replace code = 2281 if code == 2280
	* Rocky Mount, NC is 6895, not 6890
	qui replace code = 6895 if code == 6890
	
	* 2015 Salisbury, MD not in Deleware 
	qui replace code = 0 if year == 2015 & statefip == 10 & code == 41540
	* 2015 Youngstown-Warren, OH not in PA
	qui replace code = 0 if year == 2015 & statefip == 42 & code == 9320
	* 2015 Omaha, NE not in Iowa
	qui replace code = 0 if year == 2015 & statefip == 19 & code == 5920
	* 2015 Philadelphia not in Deleware
	qui replace code = 0 if year == 2015 & statefip == 10 & code == 6160
	* 2015 Philadelphia not in Maryland
	qui replace code = 0 if year == 2015 & statefip == 24 & code == 6160
	* 2015 Myrtle Beach not in NC
	qui replace code = 0 if year == 2015 & statefip == 37 & code == 5330
	* 2015 Chicago not in Wisconsin
	qui replace code = 0 if year == 2015 & statefip == 55 & code == 1600
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
	
	replace code = statefip*100000 if code = 0
	
	
	* 2005 uses 2000 Counties MA and 2015 uses Counties 2010 MA
	merge m:1 year code using "$project/dta/msa_market_access.dta"
	
	* CT has no missing counties after 1980, so non-msa's CT has to be dropped
	drop if statefip == 09 & code == 0 & year >= 1980
	* Drop non-merged
	drop if _merge ~= 3 
	drop _merge
	
	* Fix code 
	replace code = 0 if code >= 100000

*->	Export survey data with MA variable
	save "$project/dta/urban_wage_with_ma.dta", replace 



	
	



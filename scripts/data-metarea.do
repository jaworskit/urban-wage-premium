********************************************************************************
* data-metarea.do
*
* This file creates metarea_final.dta which is a county to MSA crosswalk.
********************************************************************************

cls
clear all

********************************************************************************

* global project "/Users/taylorjaworski/Dropbox/Papers/EH/LongRunMarketAccess/"
if c(username) == "kylebutts" {
	global project "/Users/kylebutts/Dropbox/UrbanWagePremium"
	global gh "~/Documents/Projects/urban-wage-premium"
}

********************************************************************************

*-> Load metarea names

	qui import excel "$project/data/urbanareas/metarea_names.xlsx", first clear
	qui save "$project/data/urbanareas/metarea_names.dta", replace

*-> Load in ipums
	
	import delimited "$project/data/urbanareas/msa_in_ipums.csv", clear
	qui gen in_ipums = 1
	qui save "$project/data/urbanareas/msa_in_ipums.dta", replace
	
*-> Simplify data

	foreach t in "1940_1950" "1960" "1970" "1980" "1990" "2000" "2005" "2010" {
		* Edited from https://usa.ipums.org/usa/volii/county_comp2b.shtml
		import excel "$project/data/urbanareas/msa_county_fips_kyle_2020-10-08.xlsx", first clear

		keeporder code metarea county_`t' fips_`t'
		qui drop if fips_`t'==.
		sort metarea fips_`t'
		rename (fips_`t') (fips)
		keeporder code metarea fips
		qui save "$project/data/urbanareas/metarea_`t'.dta", replace
		
	}

	foreach t in "2015" {
		* From http://data.nber.org/cbsa-msa-fips-ssa-county-crosswalk/
		import delimited "$project/data/urbanareas/cbsatocountycrosswalk`t'.csv", clear
		
		qui drop if cbsa==.
		rename (fipscounty cbsa cbsaname) (fips code metarea)
		keeporder fips code metarea
		qui save "$project/data/urbanareas/metarea_`t'.dta", replace
	}
	
*-> 1940-1950
		
	clear
	append using "$project/data/urbanareas/metarea_1940_1950.dta"	
	rename fips fips1940
	qui g fips1950 = fips1940
	qui g id = _n
	reshape long fips, i(id code metarea) j(year)
	duplicates drop year code fips, force
	keeporder year code metarea fips
	sort year code fips
	qui save "$project/data/urbanareas/metarea_1940_1950.dta", replace
	
*-> 1960-2015
	
	foreach t in "1960" "1970" "1980" "1990" "2000" "2005" "2010" "2015" {
		clear 
		append using "$project/data/urbanareas/metarea_`t'.dta"	
		qui g year=`t'
		duplicates list code fips
		duplicates drop code fips, force
		keeporder year code metarea fips
		sort year code fips
		qui save "$project/data/urbanareas/metarea_`t'.dta", replace
		
		}
	
*-> Append all years
	
	clear
	foreach t in "1940_1950" "1960" "1970" "1980" "1990" "2000" "2005" "2010" "2015" {
		
		append using "$project/data/urbanareas/metarea_`t'.dta"
		
		}
	
	drop metarea
	qui merge m:1 code using "$project/data/urbanareas/metarea_names.dta"
	keeporder year fips code metarea
	sort year fips code metarea
	* duplicates drop year fips, force
	
	
*-> Add indicator for metro areas found in IPUMS data
	merge m:1 code year using "$project/data/urbanareas/msa_in_ipums.dta"
	qui keep if _merge != 2 
	qui replace in_ipums = 0 if missing(in_ipums)
	drop _merge
	
	
	gen statefip = floor(fips/1000)
	* drop Hawaii and Alaska
	qui drop if statefip == 2 | statefip == 15
	
	qui save "$project/data/urbanareas/metarea_final.dta", replace
	

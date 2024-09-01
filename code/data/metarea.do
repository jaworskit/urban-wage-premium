********************************************************************************
* metarea.do
*
* This file creates metarea_final.dta which is a county to MSA crosswalk.
********************************************************************************
cls
clear all

global project "/Users/taylorjaworski/Dropbox/Research/Papers/UrbanWagePremium/"
global gh "/Users/taylorjaworski/Github/urban-wage-premium"
if c(username) == "kylebutts" {
	global project "/Users/kylebutts/Dropbox/UrbanWagePremium"
	global gh "~/Documents/Projects/urban-wage-premium"
}
********************************************************************************

*-> Load metarea names

	qui import excel "$project/data/urbanareas/metarea_names.xlsx", first clear
	qui save "$project/data/urbanareas/metarea_names.dta", replace
	
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

	foreach t in "2020" {
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
	
*-> 1960-2020
	
	foreach t in "1960" "1970" "1980" "1990" "2000" "2010" "2020" {
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
	foreach t in "1940_1950" "1960" "1970" "1980" "1990" "2000" "2010" "2020" {
		append using "$project/data/urbanareas/metarea_`t'.dta"
  }
	drop if year == 2020

	drop metarea
	merge m:1 code using "$project/data/urbanareas/metarea_names.dta", keep(match)
	keeporder fips year code metarea
	sort fips year code metarea
	duplicates drop fips year, force
	
*-> Export

	qui save "$project/data/urbanareas/metarea_final.dta", replace
	

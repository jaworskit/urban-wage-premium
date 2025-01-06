********************************************************************************
* merge.do
*
* This merges survey data with Market Access data
* Must run after `metarea.do`, `IPUMS-compile.do`, `marketaccess.do`
********************************************************************************
cls
clear all

global project "/Users/taylorjaworski/Dropbox/Research/Papers/UrbanWagePremium/"
global gh "/Users/taylorjaworski/Github/urban-wage-premium"
if c(username) == "kylebutts" {
	global project "~/Dropbox/UrbanWagePremium"
	global gh "~/Documents/Projects/urban-wage-premium"
}
********************************************************************************

*-> Housing 1950 Data
	
	qui use "$project/data/urbanareas/metarea_final.dta", clear
	keep if year == 1950
	keep fips code
	* Fix Miami - Dade County
	replace fips = 12085 if fips == 12086
	* Fix DC
	replace fips = 11000 if fips == 11001
	qui save "$project/data/temp/metarea_1950.dta", replace
	
  * Historical, Demographic, Economic, and Social Data: 
  * The United States, 1790-2002
	qui use "$project/data/housing/02896-0072-Data.dta", clear
	rename var63 valueh_1950
  rename var65 rent_avg_1950
	* Drop statewide and 2 obs missing fips 
	drop if missing(fips) | missing(counfip)
	qui replace statefip = (fips - mod(fips, 1000)) / 1000
	
	qui merge 1:m fips using "$project/data/temp/metarea_1950.dta"
	drop _merge
	rm "$project/data/temp/metarea_1950.dta"
	qui gen year = 1950
	
	* Collapse to code
	qui replace code = statefip * 100000 if code == .
	qui collapse (mean) valueh_1950 rent_avg_1950 year, by(code)
	
	qui save "$project/data/temp/housing_1950.dta", replace

********************************************************************************
* Load Survey data and Merge with MA 
********************************************************************************

	qui use "$project/data/dta/ipums_compiled.dta", clear 
	
*-> Fix MSA codes in survey

	* to merge with market access, need correct MSA codes
	qui gen code = metarea 
	qui replace code = code * 10 if year <= 2010

  * Get metarea name 
  decode metarea, gen(metarea_name)
  drop metarea
  rename metarea_name metarea
	
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
	
*-> Merge MA

	replace code = statefip*100000 if code == 0

  * tab code if _merge == 1
  * Details:
  * 1200 = Achorage, Alaska 
  * 
  * All these's counties are in other codes as well
  * 2600 = Fitchburg-Leominster, MA PMSA
  * 5350 = Nashua, NH
  * 5400 = New Bedford, MA
  * 5480 = New Haven-Meriden, CT
  * 8040 = Stamford, CT
  * 8880 = Waterbury, CT
  * 9240 = Worcester, MA
  * 
  * 900000 = CT has no non-msa after 1980

	merge m:1 year code using "$project/data/dta/msa_market_access.dta"
	
	* Drop non-merged
	drop if _merge == 2
	drop _merge
	
	* Every county in CT is in an MSA starting in 1980, so non-msa's CT observations are dropped
	drop if statefip == 09 & code == 900000 & year >= 1980

  * Fix code back to 0 for non-msa workers
	replace code = 0 if code >= 100000

*-> Urban indicator
  qui gen urban = (code > 0)
  
********************************************************************************
* Prepare Data for Results 
********************************************************************************
	
*-> Main sample
  
  * Male non-farm workers
	qui keep if sex == 1 & ind1950 != 1

*-> order variables
  order statefip code metarea year perwt, first

*-> Add census region

  preserve 
    qui insheet using "$project/data/crosswalk/statefip_to_census_region.csv", clear
    keeporder statefip census_region
    * Drop hawaii and alaska
    drop if statefip == 2 | statefip == 15
    qui save "$project/data/temp/census_regions.dta", replace
  restore
  merge m:1 statefip using "$project/data/temp/census_regions.dta", assert(match) nogen

*-> fix income top codes

	* https://usa.ipums.org/usa-action/variables/INCWAGE#codes_section
	qui drop if incwage==0
	qui drop if incwage==999998 & year==1940
	qui replace incwage = 1.5*05001 if incwage>=05001 & year==1940
	qui replace incwage = 1.5*10000 if incwage>=10000 & year==1950
	qui replace incwage = 1.5*25000 if incwage>=25000 & year==1960
	qui replace incwage = 1.5*50000 if incwage>=50000 & year==1970
	qui replace incwage = 1.5*75000 if incwage==75000 & year==1980

*-> use CPI to adjust to 2010 dollars 

	qui replace incwage = incwage * 16.81 if year == 1940
	qui replace incwage = incwage * 9.92 if year == 1950
	qui replace incwage = incwage * 7.98 if year == 1960
	qui replace incwage = incwage * 6.18 if year == 1970
	qui replace incwage = incwage * 3.00 if year == 1980
	qui replace incwage = incwage * 1.82 if year == 1990
	qui replace incwage = incwage * 1.38 if year == 2000
	qui replace incwage = incwage * 1.00 if year == 2010

*-> fix weeks

	qui gen weeks = .
	qui replace weeks = 33 if wkswork2==3
	qui replace weeks = 44 if wkswork2==4
	qui replace weeks = 47 if wkswork2==5
	qui replace weeks = 51 if wkswork2==6
	
*-> generate "total" variables

	qui gen totalweeks = perwt*weeks
	qui gen totalincome = perwt*incwage

*-> generate log(weekly wage) variable

	qui gen weeklywage = (incwage/weeks)
	qui gen ln_weeklywage = log(weeklywage)
	
*-> Merge in 1950 median home price
	
	merge m:1 code year using "$project/data/temp/housing_1950.dta"	
	drop if _merge == 2
	replace valueh = valueh_1950 if ~missing(valueh_1950)
	drop valueh_1950
  replace rent = rent_avg_1950 if ~missing(rent_avg_1950)
	drop rent_avg_1950
	drop _merge

*-> Clean rent and housing value

  replace rent = . if (rent == 9999 | rent == 9998) & (year == 1940)
  replace rent = . if (rent == 1) & (year == 1980 | year == 1990)
  replace rent = . if (rent == 0)
  
	qui replace valueh = . if valueh==0 | valueh==9999999 | valueh==9999998

  * * NOTE: In 1940, there's oddly big numbers for 42,000 obs. out of 3.6 million
  * replace rent = . if (rent > 150) & (year == 1940)

  * Winsorize rent and housing value data (there are some extremes)
  gstats winsor rent, by(year) replace cuts(1 99) 
  gstats winsor valueh, by(year) replace cuts(1 99)

*-> Adjust housing value for inflation

	qui replace valueh = valueh * 16.81 if year==1940
  qui replace valueh = valueh * 9.92 if year==1950
	qui replace valueh = valueh * 7.98 if year==1960
	qui replace valueh = valueh * 6.18 if year==1970
	qui replace valueh = valueh * 3.00 if year==1980
	qui replace valueh = valueh * 1.82 if year==1990
	qui replace valueh = valueh * 1.38 if year==2000
	qui replace valueh = valueh * 1.00 if year==2010

  qui gen rent_unadjusted = rent
  qui replace rent = rent * 16.81 if year==1940
  qui replace rent = rent * 9.92 if year==1950
	qui replace rent = rent * 7.98 if year==1960
	qui replace rent = rent * 6.18 if year==1970
	qui replace rent = rent * 3.00 if year==1980
	qui replace rent = rent * 1.82 if year==1990
	qui replace rent = rent * 1.38 if year==2000
	qui replace rent = rent * 1.00 if year==2010

*-> "Hourly housing costs" from Hoxie, Shoag, and Veuger (JPubE, 2023)

  * Definition from Ganon and Shoag (2017)
  qui gen housing_costs = valueh * 0.05
  qui replace housing_costs = rent * 12 if valueh >= .
  
  * 48 working weeks per year
  qui gen weekly_housing_costs = housing_costs / 48
  qui gen weekly_housing_costs_rentonly = rent * 12 / 48
  
  * Weekly wage net of housing costs
  qui gen net_weeklywage = weeklywage - weekly_housing_costs
  qui gen net_weeklywage_rentonly = weeklywage - weekly_housing_costs_rentonly

*-> generate MSA and non-MSA size

	egen msasize = total(perwt), by(code)
	qui gen ln_msasize = log(msasize)
	
*-> additional variables for regressions

	qui gen white = (race==1)
  qui gen black = (race==2)
	qui gen agegroup = int(age / 5)	
	
  qui gen hs_degree = educ >= 6
  qui gen college_degree = educ >= 10
  qui gen older = age > 40
  qui xtile age_tercile = age, nq(3)

*-> Prepare polynomial of lat lon

	qui gen lat2 = lat^2
	qui gen lat3 = lat^3
	qui gen lon2 = lon^2
	qui gen lon3 = lon^3
	qui gen latlon = lat * lon
	qui gen lat2lon = lat^2 * lon 
	qui gen latlon2 = lat * lon^2

*-> Log variables

  qui gen ln_ma = log(ma)
	qui gen ln_ma_weighted = log(ma_weighted)
	qui gen ln_ma_removeown = log(ma_removeown)
	qui gen ln_ma_weighted_removeown = log(ma_weighted_removeown)

  qui gen ln_weekly_housing_costs = log(weekly_housing_costs)
  qui gen ln_net_weeklywage = log(net_weeklywage)
  
  qui gen ln_weekly_housing_costs_rentonly = log(weekly_housing_costs_rentonly)
  qui gen ln_net_weeklywage_rentonly = log(net_weeklywage_rentonly)

		
*-> Label Variables	

	label variable ma "Market Access"
	label variable ma_weighted "Market Access (Weighted)"
	label variable ma_removeown "Market Access"
	label variable ma_weighted_removeown "Market Access (Weighted)"
	label variable weeklywage "Weekly Wage, 2015 \$"
	label variable white "=1, if White"
	label variable urban "=1, if in Urban Area"

*->	Export final sample
	save "$project/data/dta/urban_wage.dta", replace 


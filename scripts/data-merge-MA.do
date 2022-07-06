********************************************************************************
* data-merge-MA.do
*
* This merges survey data with Market Access data
* Must run after data-metarea.do, data-IPUMS-compile.do, data-marketaccess.do
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
	
*-> Housing 1950 Data
	
	qui use "$project/data/urbanareas/metarea_final.dta", clear
	keep if year == 1950
	keep fips code
	* Fix Miami - Dade County
	replace fips = 12085 if fips == 12086
	* Fix DC
	replace fips = 11000 if fips == 11001
	qui save "$project/data/temp/metarea_1950.dta", replace
	
	qui use "$project/data/housing/02896-0072-Data.dta", clear
	rename var63 valueh_1950
	* 2 obs missing fips 
	drop if missing(fips)
	* Drop statewide
	drop if missing(counfip)
	* fix state fips
	qui replace statefip = (fips - mod(fips, 1000))/1000
	
	qui merge 1:m fips using "$project/data/temp/metarea_1950.dta"
	drop _merge
	rm "$project/data/temp/metarea_1950.dta"
	
	qui g year = 1950
	
	* Collapse to code
	qui replace code = statefip * 100000 if code == .
	qui collapse (mean) valueh year, by(code)
	
	qui save "$project/data/temp/housing_1950.dta", replace


********************************************************************************
* Load Survey data and Merge with MA 
********************************************************************************


	qui use "$project/data/dta/urban_wage_premium_data.dta", clear 
	
	
*-> Fix MSA codes in survey

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
	
	
*-> Merge MA

	replace code = statefip*100000 if code == 0
	merge m:1 year code using "$project/data/dta/msa_market_access.dta"
	
	* Drop non-merged
	drop if _merge ~= 3 
	drop _merge
	
	* CT has no missing counties after 1980, so non-msa's CT has to be dropped
	drop if statefip == 09 & code == 900000 & year >= 1980

	
********************************************************************************
* Prepare Data for Results 
********************************************************************************
	
*-> fix income top codes
	* https://usa.ipums.org/usa-action/variables/INCWAGE#codes_section
	qui drop if incwage==0
	qui drop if incwage==999998 & year==1940
	qui replace incwage = 1.5*05001 if incwage>=05001 & year==1940
	qui replace incwage = 1.5*10000 if incwage>=10000 & year==1950
	qui replace incwage = 1.5*25000 if incwage>=25000 & year==1960
	qui replace incwage = 1.5*50000 if incwage>=50000 & year==1970
	qui replace incwage = 1.5*75000 if incwage==75000 & year==1980
	
*-> fix weeks
	qui g weeks = .
	qui replace weeks = 33 if wkswork2==3
	qui replace weeks = 44 if wkswork2==4
	qui replace weeks = 47 if wkswork2==5
	qui replace weeks = 51 if wkswork2==6
	
*-> generate "total" variables

	qui g totalweeks = perwt*weeks
	qui g totalincome = perwt*incwage

*-> generate log(weekly wage) variable

	qui g weeklywage = (incwage/weeks)
	qui g ln_weeklywage = log(incwage/weeks)
	qui by year, sort: sum weeklywage
	
*-> Merge in 1950 median home price
	
	merge m:1 code year using "$project/data/temp/housing_1950.dta"	
	drop if _merge == 2
	replace valueh = valueh_1950 if ~missing(valueh_1950)
	drop valueh_1950
	drop _merge

*-> generate log(average housing price) variable by city

	qui replace valueh = . if valueh==0 | valueh==9999999 | valueh==9999998
	qui g houses = perwt*(valueh!=.)
	qui g housingvalue = perwt*valueh
	egen total_houses = total(houses), by(year code)
	egen total_housingvalue = total(housingvalue), by(year code)
	qui g average_price = total_housingvalue/total_houses
	qui g ln_price = log(average_price)
	drop total_houses total_housingvalue average_price housingvalue houses valueh
	*qui replace houseprice = houseprice*16.81 if year==1940
	*qui replace houseprice = houseprice*7.98 if year==1960
	*qui replace houseprice = houseprice*6.18 if year==1970
	*qui replace houseprice = houseprice*3.00 if year==1980
	*qui replace houseprice = houseprice*1.82 if year==1990
	*qui replace houseprice = houseprice*1.38 if year==2000
	*qui replace houseprice = houseprice*1.00 if year==2010
	
*-> use CPI to adjust to 2015 dollars 

	qui replace totalincome = totalincome*16.81 if year == 1940
	qui replace totalincome = totalincome*9.92 if year == 1950
	qui replace totalincome = totalincome*7.98 if year == 1960
	qui replace totalincome = totalincome*6.18 if year == 1970
	qui replace totalincome = totalincome*3.00 if year == 1980
	qui replace totalincome = totalincome*1.82 if year == 1990
	qui replace totalincome = totalincome*1.38 if year == 2000
	qui replace totalincome = totalincome*1.00 if year == 2010
  qui replace totalincome = totalincome*1.00 if year == 2020
	
	qui g urban = (metarea > 0)
	qui keep if sex == 1
	qui drop if ind1950 == 1

*-> generate MSA and non-MSA size

	egen msasize = total(perwt), by(code)
	qui g ln_msasize = log(msasize)
	
*-> additional variables for regressions

	qui g white = (race==1)
	qui g agegroup = int(age/5)	
	qui g ln_ma = log(ma)
	qui g ln_ma_weighted = log(ma_weighted)
	qui g ln_ma_removeown = log(ma_removeown)
	qui g ln_ma_weighted_removeown = log(ma_weighted_removeown)
	
*-> Prepare polynomial of lat lon

	qui g lat2 = lat^2
	qui g lat3 = lat^3
	qui g lon2 = lon^2
	qui g lon3 = lon^3
	qui g latlon = lat * lon
	qui g lat2lon = lat^2 * lon 
	qui g latlon2 = lat * lon^2
		
*-> Label Variables	

	label variable ma "Market Access"
	label variable ma_weighted "Market Access (Weighted)"
	label variable ma_removeown "Market Access"
	label variable ma_weighted_removeown "Market Access (Weighted)"
	label variable weeklywage "Weekly Wage, 2015 \$"
	label variable white "=1, if White"
	label variable urban "=1, if in Urban Area"
	
	
*->	Export survey data with MA variable
	
	* Fix code 
	replace statefip = code/100000 if code >= 100000
	replace code = 0 if code >= 100000

	save "$project/data/dta/urban_wage_with_ma.dta", replace 


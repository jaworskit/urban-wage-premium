********************************************************************************
* data-group-averages.do
*
* Creates group-averages of variables for use with Altonji and Mansfield (2018)
* Must run after data-IPUMS-compile.do, data-marketaccess.do, and 
* data-merge-MA.do
********************************************************************************

cls
clear all
* ssc install gtools

********************************************************************************

global project "/Users/taylorjaworski/Dropbox/Research/Papers/UrbanWagePremium/"
global gh "/Users/taylorjaworski/Github/urban-wage-premium"
if c(username) == "kylebutts" {
	global project "/Users/kylebutts/Dropbox/UrbanWagePremium"
	global gh "/Users/kylebutts/Documents/Projects/urban-wage-premium"
}

********************************************************************************

use "$project/data/dta/urban_wage_with_ma.dta", clear 

qui g hs_degree = (educ >= 6)
qui g college_degree = (educ >= 10)

* msacode is MSA code or 10000*statefips for non-urban areas
sort year msacode

********************************************************************************
* Generate Group Averages
********************************************************************************
*-> Total Pop.

	gegen perwt_total = total(perwt), by(year msacode)

*-> By Race Group
	
	levelsof white, local(rlevels)
	foreach r of local rlevels {
		qui g race`r'_level = perwt*(white==`r')
		qui gegen race`r'_total = total(race`r'_level), by(year msacode)
		qui g share_race`r' = race`r'_total/perwt_total
		drop race`r'_level race`r'_total
  }
	
*-> By Age Group
	
	levelsof agegroup, local(alevels)
	foreach a of local alevels {
		qui g age`a'_level = perwt*(agegroup==`a')
		qui gegen age`a'_total = total(age`a'_level), by(year msacode)
		qui g share_age`a' = age`a'_total/perwt_total
		drop age`a'_level age`a'_total
  }
	
*-> By Education Group
	
	levelsof educ, local(elevels)
	foreach e of local elevels {
		qui g educ`e'_level = perwt*(educ==`e')
		qui gegen educ`e'_total = total(educ`e'_level), by(year msacode)
		qui g share_educ`e' = educ`e'_total/perwt_total
		drop educ`e'_level educ`e'_total
  }
		
*-> By Marital Status

	levelsof marst, local(mlevels)
	foreach m of local mlevels {
		qui g marst`m'_level = perwt*(marst==`m')
		qui gegen marst`m'_total = total(marst`m'_level), by(year msacode)
		qui g share_marst`m' = marst`m'_total/perwt_total
		drop marst`m'_level marst`m'_total
  }

*-> By Veteran Status

	levelsof vetstat, local(vlevels)
	foreach v of local vlevels {
		qui g vetstat`v'_level = perwt*(vetstat==`v')
		qui gegen vetstat`v'_total = total(vetstat`v'_level), by(year msacode)
		qui g share_vetstat`v' = vetstat`v'_total/perwt_total
		drop vetstat`v'_level vetstat`v'_total
  }
		
*-> Average rent

	qui gegen rent_total = total(rent), by(year msacode)
	qui g rent_avg = rent_total/perwt_total
	drop rent_total

*-> Average housing price

	qui g houses = perwt*(valueh!=.)
	qui g housingvalue = perwt*valueh
	qui gegen total_houses = total(houses), by(year msacode)
	qui gegen total_housingvalue = total(housingvalue), by(year msacode)
	qui g average_price = total_housingvalue/total_houses
	drop total_houses total_housingvalue housingvalue houses
		
********************************************************************************
* Generate Group Averages by Highschool / No Highschool
********************************************************************************
/*
*-> Total Pop.

	egen by_hs_perwt_total = total(perwt), by(year msacode hs_degree)

*-> By Race Group
	
	levelsof white, local(rlevels)
	foreach r of local rlevels {
		qui g race`r'_level = perwt*(white==`r')
		qui egen race`r'_total = total(race`r'_level), by(year msacode hs_degree)
		qui g by_hs_share_race`r' = race`r'_total/perwt_total
		drop race`r'_level race`r'_total
  }
	
*-> By Age Group
	
	levelsof agegroup, local(alevels)
	foreach a of local alevels {
		qui g age`a'_level = perwt*(agegroup==`a')
		qui egen age`a'_total = total(age`a'_level), by(year msacode hs_degree)
		qui g by_hs_share_age`a' = age`a'_total/perwt_total
		drop age`a'_level age`a'_total
  }
	
*-> By Education Group
	
	levelsof educ, local(elevels)
	foreach e of local elevels {
		qui g educ`e'_level = perwt*(educ==`e')
		qui egen educ`e'_total = total(educ`e'_level), by(year msacode hs_degree)
		qui g by_hs_share_educ`e' = educ`e'_total/perwt_total
		drop educ`e'_level educ`e'_total
  }
		
*-> By Marital Status

	levelsof marst, local(mlevels)
	foreach m of local mlevels {
		qui g marst`m'_level = perwt*(marst==`m')
		qui egen marst`m'_total = total(marst`m'_level), by(year msacode hs_degree)
		qui g by_hs_share_marst`m' = marst`m'_total/perwt_total
		drop marst`m'_level marst`m'_total
  }

*-> By Veteran Status

	levelsof vetstat, local(vlevels)
	foreach v of local vlevels {
		qui g vetstat`v'_level = perwt*(vetstat==`v')
		qui egen vetstat`v'_total = total(vetstat`v'_level), by(year msacode hs_degree)
		qui g by_hs_share_vetstat`v' = vetstat`v'_total/perwt_total
		drop vetstat`v'_level vetstat`v'_total
  }
		
*-> Average rent

	qui egen rent_total = total(rent), by(year msacode hs_degree)
	qui g by_hs_rent_avg = rent_total/perwt_total
	drop rent_total

*-> Average housing price

	qui g houses = perwt*(valueh!=.)
	qui g housingvalue = perwt*valueh
	qui gegen total_houses = total(houses), by(year msacode hs_degree)
	qui gegen total_housingvalue = total(housingvalue), by(year msacode hs_degree)
	qui g by_hs_average_price = total_housingvalue/total_houses
	drop total_houses total_housingvalue housingvalue houses 
*/

********************************************************************************
* Generate Group Averages by College / No College
********************************************************************************
*-> Total Pop.

	egen by_college_perwt_total = total(perwt), by(year msacode college_degree)

*-> By Race Group
	
	levelsof white, local(rlevels)
	foreach r of local rlevels {
		qui g race`r'_level = perwt*(white==`r')
		qui egen race`r'_total = total(race`r'_level), by(year msacode college_degree)
		qui g by_college_share_race`r' = race`r'_total/perwt_total
		drop race`r'_level race`r'_total
  }
	
*-> By Age Group
	
	levelsof agegroup, local(alevels)
	foreach a of local alevels {
		qui g age`a'_level = perwt*(agegroup==`a')
		qui egen age`a'_total = total(age`a'_level), by(year msacode college_degree)
		qui g by_college_share_age`a' = age`a'_total/perwt_total
		drop age`a'_level age`a'_total
  }
	
*-> By Education Group
	
	levelsof educ, local(elevels)
	foreach e of local elevels {
		qui g educ`e'_level = perwt*(educ==`e')
		qui egen educ`e'_total = total(educ`e'_level), by(year msacode college_degree)
		qui g by_college_share_educ`e' = educ`e'_total/perwt_total
		drop educ`e'_level educ`e'_total
  }
		
*-> By Marital Status

	levelsof marst, local(mlevels)
	foreach m of local mlevels {
		qui g marst`m'_level = perwt*(marst==`m')
		qui egen marst`m'_total = total(marst`m'_level), by(year msacode college_degree)
		qui g by_college_share_marst`m' = marst`m'_total/perwt_total
		drop marst`m'_level marst`m'_total
  }

*-> By Veteran Status

	levelsof vetstat, local(vlevels)
	foreach v of local vlevels {
		qui g vetstat`v'_level = perwt*(vetstat==`v')
		qui egen vetstat`v'_total = total(vetstat`v'_level), by(year msacode college_degree)
		qui g by_college_share_vetstat`v' = vetstat`v'_total/perwt_total
		drop vetstat`v'_level vetstat`v'_total
  }
		
*-> Average rent

	qui egen rent_total = total(rent), by(year msacode college_degree)
	qui g by_college_rent_avg = rent_total/perwt_total
	drop rent_total

*-> Average housing price

	qui g houses = perwt*(valueh!=.)
	qui g housingvalue = perwt*valueh
	qui gegen total_houses = total(houses), by(year msacode college_degree)
	qui gegen total_housingvalue = total(housingvalue), by(year msacode college_degree)
	qui g by_college_average_price = total_housingvalue/total_houses
	drop total_houses total_housingvalue housingvalue houses


*-> Export
  save "$project/data/dta/urban_wage_final.dta", replace 

********************************************************************************
* data-group-averages.do
*
* Creates group-averages of variables for use with Altonji and Mansfield (2018)
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

use "$project/data/dta/urban_wage_with_ma.dta", clear

********************************************************************************
* Generate Group Averages
********************************************************************************
	
	qui g msacode = metarea
	qui replace msacode = statefip if msacode==.

*-> Total Pop.

	egen perwt_total = total(perwt), by(year msacode)

*-> By Race Group
	
	levelsof white, local(rlevels)
	foreach r of local rlevels {
		qui g race`r'_level = perwt*(white==`r')
		qui egen race`r'_total = total(race`r'_level), by(year msacode)
		qui g share_race`r' = race`r'_total/perwt_total
		drop race`r'_level race`r'_total
		}
	
*-> By Age Group
	
	levelsof agegroup, local(alevels)
	foreach a of local alevels {
		qui g age`a'_level = perwt*(agegroup==`a')
		qui egen age`a'_total = total(age`a'_level), by(year msacode)
		qui g share_age`a' = age`a'_total/perwt_total
		drop age`a'_level age`a'_total
		}
	
*-> By Education Group
	
	levelsof educ, local(elevels)
	foreach e of local elevels {
		qui g educ`e'_level = perwt*(educ==`e')
		qui egen educ`e'_total = total(educ`e'_level), by(year msacode)
		qui g share_educ`e' = educ`e'_total/perwt_total
		drop educ`e'_level educ`e'_total
		}
		
*-> By Marital Status

	levelsof marst, local(mlevels)
	foreach m of local mlevels {
		qui g marst`m'_level = perwt*(marst==`m')
		qui egen marst`m'_total = total(marst`m'_level), by(year msacode)
		qui g share_marst`m' = marst`m'_total/perwt_total
		drop marst`m'_level marst`m'_total
		}

*-> By Veteran Status

	levelsof vetstat, local(vlevels)
	foreach v of local vlevels {
		qui g vetstat`v'_level = perwt*(vetstat==`v')
		qui egen vetstat`v'_total = total(vetstat`v'_level), by(year msacode)
		qui g share_vetstat`v' = vetstat`v'_total/perwt_total
		drop vetstat`v'_level vetstat`v'_total
		}
		
*-> Average rent

	qui egen rent_total = total(rent), by(year msacode)
	qui g rent_avg = rent_total/perwt_total
	drop rent_total

		
*-> Export
save "$project/data/dta/urban_wage_final.dta", replace 

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

*-> Total

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
		
*-> Export
save "$project/data/dta/urban_wage.dta", replace 

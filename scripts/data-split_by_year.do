* Splits data by year so easier to work with on RAM

global project "/Users/taylorjaworski/Dropbox/Research/Papers/UrbanWagePremium/"
global gh "/Users/taylorjaworski/Github/urban-wage-premium"
if c(username) == "kylebutts" {
	global project "/Users/kylebutts/Dropbox/UrbanWagePremium"
	global gh "/Users/kylebutts/Documents/Projects/urban-wage-premium"
}


use ${project}/data/dta/urban_wage_final.dta, clear

foreach y in 1940 1950 1960 1970 1980 1990 2000 2010 { 
	disp "On year: `y'"
  
  preserve
	
	keep if year == `y'
	
  save "${project}/data/dta/urban_wage_final_`y'.dta", replace
	
	restore
}



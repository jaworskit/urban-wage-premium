* Splits data by year so easier to work with on RAM

global dropbox "/Users/kylebutts/Dropbox/UrbanWagePremium"

use ${dropbox}/data/dta/urban_wage_final.dta, clear

foreach y in 1940 1950 1960 1970 1980 1990 2000 2010 2020 { 
	preserve
	
	keep if year == `y'
	
  save "${dropbox}/data/dta/urban_wage_final_`y'.dta", replace
	
	restore
}



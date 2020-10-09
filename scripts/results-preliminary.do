cls
clear all

global ipums "/Users/taylorjaworski/Dropbox/Data/Census/IPUMS"
* global project "/Users/taylorjaworski/Dropbox/Papers/EH/LongRunMarketAccess/"
global project "/Users/kylebutts/Dropbox/UrbanWagePremium"


	qui use "$project/data/population/2010CountyPopulation.dta"
	qui keep if year>1940
	
	qui merge m:m year fips using "$project/data/income/2010CountyIncome.dta"
	qui drop if _merge==2
	qui drop _merge
	
	qui egen count = count(year), by(fips)
	qui keep if count==6
	qui drop count
	
	qui merge m:m year fips using "$project/data/urbanareas/metarea_final.dta"
	qui g urban = (_merge==3)
	qui drop if _merge==2
	qui drop _merge
	
	qui replace medianincome = medianincome*16.81 if year==1940
	qui replace medianincome = medianincome*7.98 if year==1960
	qui replace medianincome = medianincome*6.18 if year==1970
	qui replace medianincome = medianincome*3.00 if year==1980
	qui replace medianincome = medianincome*1.82 if year==1990
	qui replace medianincome = medianincome*1.38 if year==2000
	qui replace medianincome = medianincome*1.00 if year==2010
	
	qui egen total_population = total(population), by(year)
	qui g income_population_weighted = (population/total_population) * percapitaincome
	
	preserve 
	collapse (mean) medianincome percapitaincome [aw=population], by(year urban)
	
	reshape wide medianincome percapitaincome, i(year) j(urban)
		
	qui g urbanpremium = log(medianincome1/medianincome0)
		
	gr tw (scatter urbanpremium year, c(l) lc(black) mfc(white) mc(black))
	* gr export "$project/paper/figures/urbanpremium_COUNTY.pdf",as(pdf) replace
	restore
	
	
	
	
	

********************************************************************************
* ICPSR-compile.do
*
* This creates a basic replication of the urban wage premium using county aggregate data. 
* No longer used
********************************************************************************

cls
clear all

********************************************************************************

* global project "/Users/taylorjaworski/Dropbox/Papers/EH/LongRunMarketAccess/"
global project "~/Dropbox/Projects/UrbanWagePremium"

********************************************************************************

	use "$project/data/income/2010CountyIncome.dta", clear
	
	qui merge 1:1 year fips using "$project/data/urbanareas/metarea_final.dta"
	qui drop if _merge==2
	qui g urban = (_merge==3)
	drop _merge
	
	qui replace percapitaincome = 0 if percapitaincome==.
	qui g ln_percapitaincome = log(percapitaincome+1)
	
	qui replace medianincome = 0 if medianincome==.
	qui g ln_medianincome = log(medianincome+1)
	
********************************************************************************

*-> estimate urban wage premium by decade, ln per capita income
	est clear
	forvalues t = 1940(10)2010 {
	
		qui reg ln_percapitaincome urban if year==`t', cluster(statenam_2010)
		qui eststo
	
		}
		
		preserve
		qui estout * using "$project/data/temp/ln_percapitaincome.csv", ///
			replace style(tab) collabels(none) mlabels(none) eqlabels(none) ///
			cells("b") keep(urban)
		qui import delim using "$project/data/temp/ln_percapitaincome.csv", clear varnames(nonames)  
		rm "$project/data/temp/ln_percapitaincome.csv"
		drop v1
		qui g id = 1
		reshape long v, i(id) j(year)
		qui replace year = 1920 + 10*year
		
		gr tw (scatter v year if year!=1950, c(l) mc(black) mfc(white) lc(black) ms(o)) ///
			, graphregion(color(white)) ylabe(0(.2)1,nogrid format(%12.1fc))
		
		restore
		
	estout * , replace style(tex) cells(b(fmt(2)) se(par fmt(2))) ///
		collabels(none) mlabels(none) eqlabels(none) varwidth(15) ///
		modelwidth(7) keep(urban)
	
*-> estimate urban wage premium by decade, ln median income
	est clear
	forvalues t = 1940(10)2010 {
	
		qui reg ln_medianincome urban if year==`t', cluster(statenam_2010)
		qui eststo
	
		}

		preserve
		qui estout * using "$project/data/temp/ln_medianincome.csv", ///
			replace style(tab) collabels(none) mlabels(none) eqlabels(none) ///
			cells("b") keep(urban)
		qui import delim using "$project/data/temp/ln_medianincome.csv", clear varnames(nonames)  
		rm "$project/data/temp/ln_medianincome.csv"
		drop v1
		qui g id = 1
		reshape long v, i(id) j(year)
		qui replace year = 1920 + 10*year
		
		gr tw (scatter v year, c(l) mc(black) mfc(white) lc(black) ms(o)) ///
			, graphregion(color(white)) ylabe(0(.4)2.8,nogrid format(%12.1fc))
		
		restore
	
	estout * , replace style(tex) cells(b(fmt(2)) se(par fmt(2))) ///
		collabels(none) mlabels(none) eqlabels(none) varwidth(15)  ///
		modelwidth(7) keep(urban)
		
********************************************************************************
	
	xi i.statenam_2010
	
	est clear
	forvalues t = 1940(10)2010 {
	
		qui reg ln_percapitaincome urban _I* if year==`t', cluster(statenam_2010)
		qui eststo
	
		}
		
		* preserve
		qui estout * using "$project/data/temp/ln_percapitaincome.csv", ///
			replace style(tab) collabels(none) mlabels(none) eqlabels(none) ///
			cells("b") keep(urban)
		qui import delim using "$project/data/temp/ln_percapitaincome.csv", clear varnames(nonames)  
		rm "$project/data/temp/ln_percapitaincome.csv"
		drop v1
		qui g id = 1
		reshape long v, i(id) j(year)
		qui replace year = 1920 + 10*year
		
		gr tw (scatter v year if year!=1950, c(l) mc(black) mfc(white) lc(black) ms(o)) ///
			, graphregion(color(white)) ylabe(0(.2)1,nogrid format(%12.1fc))
		
		restore
		
	estout * , replace style(tex) cells(b(fmt(2)) se(par fmt(2))) ///
		collabels(none) mlabels(none) eqlabels(none) varwidth(15) ///
		modelwidth(7) keep(urban)
	
	est clear
	forvalues t = 1940(10)2010 {
	
		qui reg ln_medianincome urban  _I* if year==`t', cluster(statenam_2010)
		qui eststo
	
		}
		
		preserve
		qui estout * using "$project/data/temp/ln_medianincome.csv", ///
			replace style(tab) collabels(none) mlabels(none) eqlabels(none) ///
			cells("b") keep(urban)
		qui import delim using "$project/data/temp/ln_medianincome.csv", clear varnames(nonames)  
		rm "$project/data/temp/ln_medianincome.csv"
		drop v1
		qui g id = 1
		reshape long v, i(id) j(year)
		qui replace year = 1920 + 10*year
		
		gr tw (scatter v year, c(l) mc(black) mfc(white) lc(black) ms(o)) ///
			, graphregion(color(white)) ylabe(0(.4)2,nogrid format(%12.1fc))
		
		restore
	
	estout * , replace style(tex) cells(b(fmt(2)) se(par fmt(2))) ///
		collabels(none) mlabels(none) eqlabels(none) varwidth(15)  ///
		modelwidth(7) keep(urban)

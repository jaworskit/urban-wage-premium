********************************************************************************
* IPUMS-compile.do
*
* From IPUMS files, creates individual survey dataset  
********************************************************************************

cls
clear all

* global ipums "/Users/taylorjaworski/Dropbox/Research/Data/IPUMS"
* global project "/Users/taylorjaworski/Dropbox/Research/Papers/UrbanWagePremium"
global ipums "/Users/kylebutts/Dropbox/IPUMS"
global project "/Users/kylebutts/Dropbox/UrbanWagePremium"

*-> Clean each year's data

  foreach year of numlist 1940 1950 1960 1970 1980 1990 2000 2010 { 
		display "`year'"
		*-> 1940
			/* if `year' == 1940 {
				set seed 339487731
				forvalues r = 1/4 {

					qui use "$ipums/1940/1940_reg`r'.dta", clear
          * 1940 is too large, so using only 15% sample
          keep if uniform() < .15

					**keep age 25 - 65
					
						qui keep if age >= 25 & age <= 65
						
					**keep employed and working
					
						*qui keep if empstatd == 10 
						
						qui save "$project/data/temp/1940_reg`r'_temp.dta", replace
					
					}
				
				clear 	
				forvalues r = 1/4 {
				
					qui append using "$project/data/temp/1940_reg`r'_temp.dta"
					rm "$project/data/temp/1940_reg`r'_temp.dta"
					
        }	
				
				**fix occupation variable
				
					*0) merge occ1950 -> occ2010
					
						qui merge n:1 occ1950 using "$project/data/dta/occ1950_occ2010_crosswalk.dta"
						qui gen occ2010 = occ2010_temp 
						qui drop if _m == 2
						qui drop occ2010_temp _m
									
					*1) merge in soc2010 crosswalk 
					
						qui gen occ2010_st = string(occ2010,"%04.0f") //gen occupation code stringed
						qui drop occ2010
						rename occ2010_st occ2010
						qui merge n:1 occ2010 using "$project/data/dta/occ_soc_crosswalk.dta" //merge in soc crosswalk
						qui drop if _m == 2
						qui drop _m
						qui drop if soc2010 == "."

					*2) generate 2-digit SOC 
					
						qui gen occupation = substr(soc2010, 1,2) 
				
				**define "three-digit" industry

					qui replace ind1950 = int(ind1950/10)
					qui drop if ind1950==0 | ind1950>=97
					qui replace ind1950 = int(ind1950/10)
		
				**drop Alaska and Hawaii 
				
					qui drop if statefip == 15 | statefip == 2 | statefip>56 
				
				**keep if worked at least 26 weeks
				
					qui keep if wkswork2 >= 3
				
				*save 1940 data
					
					keeporder year statefip metarea occupation ind1950 age sex race educ perwt incwage wkswork2 valueh bpl marst vetstat rent
					qui save "$project/data/temp/census_1940_temp.dta", replace	
      } */

		*-> 1950
			
			if `year' == 1950 {
					
				qui use "$ipums/`year'/`year'.dta", clear
				drop perwt
				qui drop if slwt==0
				rename slwt perwt
	
				**keep age 25 - 65
				
					qui keep if age >= 25 & age <= 65
				
				**keep employed and working
				
					*qui keep if empstatd == 10 
				
				**fix occupation variable: occ1950 -> occ2010
				
					*1) merge in soc2010 crosswalk 
					
						qui gen occ2010_st = string(occ2010,"%04.0f") //gen occupation code stringed
						drop occ2010
						rename occ2010_st occ2010
						qui merge n:1 occ2010 using "$project/data/dta/occ_soc_crosswalk.dta" //merge in soc crosswalk
						qui drop if _m == 2
						drop _m
						qui drop if soc2010 == "."

					*2) generate 2-digit SOC 
					
						qui gen occupation = substr(soc2010, 1,2) 
				
				**define "three-digit" industry

					qui replace ind1950 = int(ind1950/10)
					qui drop if ind1950==0 | ind1950>=97
					qui replace ind1950 = int(ind1950/10)
			
				**drop Alaska and Hawaii 
				
					qui drop if statefip == 15 | statefip == 2 | statefip>56 
				
				**keep if worked at least 26 weeks
				
					qui keep if wkswork2 >= 3
				
				*save 1950 data
				
					keeporder year statefip metarea occupation ind1950 age sex race educ perwt incwage wkswork2 bpl marst vetstat
					qui save "$project/data/temp/census_1950_temp.dta", replace
						
      }
					
		*-> 1960-2000
		
			if `year' > 1950 & `year' <= 2000 {
			
				if `year' == 1970 {
					qui use "$ipums/`year'/1970_wo_migrate.dta", clear
        }
				if `year' != 1970 {
					qui use "$ipums/`year'/`year'.dta", clear
        }
				
				**keep age 25 - 65
				
					qui keep if age >= 25 & age <= 65
					
				**keep employed and working
				
					*qui keep if empstatd == 10 
		
				**fix occupation variable: occ1950 -> occ2010
				
					*1) merge in soc2010 crosswalk 
					
						qui gen occ2010_st = string(occ2010,"%04.0f") //gen occupation code stringed
						qui drop occ2010
						rename occ2010_st occ2010
						qui merge n:1 occ2010 using "$project/data/dta/occ_soc_crosswalk.dta" //merge in soc crosswalk
						qui drop if _m == 2
						qui drop _m
						qui drop if soc2010 == "."

					*2) generate 2-digit SOC 
					
						qui gen occupation = substr(soc2010, 1,2) 
				
				**define "three-digit" industry

					qui replace ind1950 = int(ind1950/10)
					qui drop if ind1950==0 | ind1950>=97
					qui replace ind1950 = int(ind1950/10)
			
				**drop Alaska and Hawaii 

					qui drop if statefip == 15 | statefip == 2| statefip>56 
				
				**keep if worked at least 26 weeks
				
					qui keep if wkswork2 >= 3
				
				*save 1960-2010 data
					
					if `year' == 1970 {
					  keeporder year statefip metarea occupation ind1950 age sex race educ perwt incwage wkswork2 valueh bpl marst rent
					}
					if `year' != 1970 {
					  keeporder year statefip metarea occupation ind1950 age sex race educ perwt incwage wkswork2 valueh bpl marst vetstat rent
					}
					qui save "$project/data/temp/census_`year'_temp.dta", replace
					
      }

		*-> 2010
			
			if `year' == 2010 {
				qui use "$ipums/`year'/`year'_with_rent.dta", clear
				
				**keep age 25 - 65
				
					qui keep if age >= 25 & age <= 65
					
				**keep employed and working
				
					*qui keep if empstatd == 10 
		
				**fix occupation variable
				
				*0) merge occ1950 -> occ2010
					
          qui merge n:1 occ1950 using "$project/data/dta/occ1950_occ2010_crosswalk.dta"
          qui gen occ2010 = occ2010_temp 
          qui drop if _m == 2
          drop occ2010_temp _m
				
				*1) merge in soc2010 crosswalk 
					qui gen occ2010_st = string(occ2010,"%04.0f") //gen occupation code stringed
					drop occ2010
					rename occ2010_st occ2010
					qui merge n:1 occ2010 using "$project/data/dta/occ_soc_crosswalk.dta" //merge in soc crosswalk
					qui drop if _m == 2
					drop _m
					qui drop if soc2010 == "."

				*2) generate 2-digit SOC 
					qui gen occupation = substr(soc2010, 1,2) 
				
				**define "three-digit" industry

					qui replace ind1950 = int(ind1950/10)
					qui drop if ind1950==0 | ind1950>=97
					qui replace ind1950 = int(ind1950/10)
			
				**drop Alaska and Hawaii 

					qui drop if statefip == 15 | statefip == 2| statefip>56 
				
				**keep if worked at least 26 weeks
				
					qui keep if wkswork2 >= 3
				
				**save 1960-2010 data
					
					if `year'==2015 {
						rename met2013 metarea
          }
					keeporder year statefip metarea occupation ind1950 age sex race educ perwt incwage wkswork2 bpl marst vetstat rent valueh
					qui save "$project/data/temp/census_`year'_temp.dta", replace
      }
  } /* end year */

*-> append all years
  clear
  foreach year of numlist 1940 1950 1960 1970 1980 1990 2000 2010 {	
    qui append using "$project/data/temp/census_`year'_temp.dta"
    * rm "$project/data/temp/census_`year'_temp.dta"
  }
  
*-> Clean occupation
  qui replace occupation = "17" if occupation=="15" 
  qui replace occupation = "13" if occupation=="23" 
  
*-> Export
  qui save "$project/data/dta/ipums_compiled.dta", replace

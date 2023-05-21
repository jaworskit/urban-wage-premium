********************************************************************************
* data-IPUMS-compile.do
*
* From IPUMS files, creates individual survey dataset  
********************************************************************************

cls
clear all

* global ipums "/Users/taylorjaworski/Dropbox/Research/Data/IPUMS"
* global project "/Users/taylorjaworski/Dropbox/Research/Papers/UrbanWagePremium"
global ipums "/Users/kylebutts/Dropbox/IPUMS"
global project "/Users/kylebutts/Dropbox/UrbanWagePremium"


********************************************************************************

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
						
						qui save "$project/data/dta/temp/1940_reg`r'_temp.dta", replace
					
					}
				
				clear 	
				forvalues r = 1/4 {
				
					qui append using "$project/data/dta/temp/1940_reg`r'_temp.dta"
					rm "$project/data/dta/temp/1940_reg`r'_temp.dta"
					
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
					qui save "$project/data/dta/temp/census_1940_temp.dta", replace	
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
					qui save "$project/data/dta/temp/census_1950_temp.dta", replace
						
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
					qui save "$project/data/dta/temp/census_`year'_temp.dta", replace
					
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
					qui save "$project/data/dta/temp/census_`year'_temp.dta", replace
      }
  } /* end for loop */
				
********************************************************************************

	*-> append all years
		
		clear
		foreach year of numlist 1940 1950 1960 1970 1980 1990 2000 2010 {	
			qui append using "$project/data/dta/temp/census_`year'_temp.dta"
			* rm "$project/data/dta/temp/census_`year'_temp.dta"
		}
		
		qui replace occupation = "17" if occupation=="15" 
		qui replace occupation = "13" if occupation=="23" 
		
		qui save "$project/data/dta/ipums_compiled.dta", replace
		
********************************************************************************

	/* 
  *-> Basic Urban Wage Premium Plot

		use "$project/data/dta/ipums_compiled.dta", clear
		
	*->fix income top codes
		
		qui drop if incwage==0
		qui drop if incwage==999998 & year==1940
		qui replace incwage = 1.5*05001 if incwage>=05001 & year==1940
		qui replace incwage = 1.5*25000 if incwage==25000 & year==1960
		qui replace incwage = 1.5*50000 if incwage==50000 & year==1970
		qui replace incwage = 1.5*75000 if incwage==75000 & year==1980
		
	*->fix weeks
		
		qui g weeks = .
		qui replace weeks = 33 if wkswork2==3
		qui replace weeks = 44 if wkswork2==4
		qui replace weeks = 47 if wkswork2==5
		qui replace weeks = 51 if wkswork2==6
		
	*->generate "total" variables
	
		qui g totalweeks = perwt*weeks
		qui g totalincome = perwt*incwage
		
	*->use CPI to adjust to 2015 dollars 
	
		qui replace totalincome = totalincome*16.81 if year==1940
		qui replace totalincome = totalincome*7.98 if year==1960
		qui replace totalincome = totalincome*6.18 if year==1970
		qui replace totalincome = totalincome*3.00 if year==1980
		qui replace totalincome = totalincome*1.82 if year==1990
		qui replace totalincome = totalincome*1.38 if year==2000
		qui replace totalincome = totalincome*1.00 if year==2010
		
	*->housing
		
		qui replace valueh = . if valueh==0 | valueh==9999999 | valueh==9999998
		qui g houses = perwt*(valueh!=.)
		qui g housingvalue = perwt*valueh
		egen total_houses = total(houses), by(year metarea)
		egen total_housingvalue = total(housingvalue), by(year metarea)
		qui g average_houseprice = total_housingvalue/total_houses
		qui replace houseprice = houseprice*16.81 if year==1940
		qui replace houseprice = houseprice*7.98 if year==1960
		qui replace houseprice = houseprice*6.18 if year==1970
		qui replace houseprice = houseprice*3.00 if year==1980
		qui replace houseprice = houseprice*1.82 if year==1990
		qui replace houseprice = houseprice*1.38 if year==2000
		qui replace houseprice = houseprice*1.00 if year==2010 

	*-> 	
    qui g urban = (metarea!=0)
		qui keep if sex==1
		qui drop if ind1950==1
		
	  collapse (sum) totalweeks totalincome perwt houses housingvalue, by(year urban)
	
		qui g weeklywage = totalincome/perwt
		qui g houseprice = housingvalue/houses
		
		reshape wide weeklywage houseprice totalweeks totalincome perwt, i(year) j(urban)
		
		qui g urbanpremium = log(weeklywage1/weeklywage0)
		
		gr tw (scatter urbanpremium year, c(l) lc(black) mfc(white) mc(black))
		gr export "$project/paper/figures/urbanpremium_IPUMS.pdf", as(pdf) replace	
  */
		

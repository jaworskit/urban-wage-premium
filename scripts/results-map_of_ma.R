## -----------------------------------------------------------------------------
## results-map_of_ma.R
## Kyle Butts, CU Boulder Economics 
## 
## Creates maps of Market Access over time (Facet Wrap) and a % change in MA from 1940 to 2010
## -----------------------------------------------------------------------------

library(tidyverse)
library(tidylog)
library(haven)
library(sf) 
library(rmapshaper)
library(tigris)

setwd("~/Dropbox/UrbanWagePremium/")

# Custom Theme 
source("https://gist.githubusercontent.com/kylebutts/7dc66a01ec7e499faa90b4f1fd46ef9f/raw/ea949343b6f486d7453b12465b8f375b8b67093f/theme_kyle.R")



## Load MA data ----------------------------------------------------------------

ma <- haven::read_dta("dta/msa_market_access.dta") %>% 
	mutate(
		state_fips = case_when(
			code >= 100000 ~ code / 100000
		),
		code = sprintf("%04d", code),
		log_ma = log(ma)
	) %>% 
	select(code, year, everything()) %>% 
	arrange(code, year) 

ma_nonurban <- ma %>% filter(!is.na(state_fips))

ma <- ma %>% 
	filter(is.na(state_fips)) %>% 
	select(code, year, log_ma)


## Prepare MSAs ----------------------------------------------------------------

msa_1950 <- sf::read_sf("data/shapefiles/MSA_1950/US_smsa_1950.shp") %>% 
	rmapshaper::ms_simplify(keep = 0.01) %>% 
	select(code = SMAA, metarea = SMA, geometry) %>%
	# Not in Ipums
	filter(!(code %in% c(
		# Bay City, MI SMA
		"0800",
		# Lowell, MA SMA
		"4560",
		# Springfield, OH SMA
		"7960",
		# Wilkes-Barre--Hazleton, PA SMA
		"9120",
		# New Britain-Bristol, CT SMA
		"5440",
		# Ogden, UT SMA
		"5840",
		# Winston-Salem, NC SMA
		"9220",
		# Lawrence, MA SMA
		"4160",
		# Waterbury CT
		"8880",
		# Durham, NC SMA
		"2280",
		# New Bedford, MA SMA
		"5400",
		# Stamford-Norwalk, CT SMA
		"8040"
	))) %>% 
	mutate(
		code = case_when(
			# Fall River, MA-RI SMA
			code == "2480" ~ "6481",
			# San Bernardino
			code == "7280" ~ "6780",
			# El Paso
			code == "2320" ~ "2310",
			# Durham
			code == "2281" ~ "2280",
			# Fort Worth
			code == "2800" ~ "1921",
			# Jacksonville, FL
			code == "3600" ~ "3590",
			TRUE ~ code
		)
	)


ma_1940 <- full_join(
		msa_1950, 
		ma %>% 
			filter(year == 1940) %>% 
			filter(!(code %in% c("2001", "2200", "7161"))), 
		by = "code"
	)

ma_1950 <- full_join(
		msa_1950, 
		ma %>% 
			filter(year == 1950) %>% 
			filter(!(code %in% c("2001", "2200", "7161"))), 
		by = "code"
	)


msa_1960 <- sf::read_sf("data/shapefiles/MSA_1960/") %>% 
	rmapshaper::ms_simplify(keep = 0.01) %>% 
	select(code = SMSAA, metarea = SMSA, geometry) %>%
	# Not in Ipums
	filter(!(code %in% c(
		# Meriden, CT SMSA
		"4960",
		# Honolulu, HI SMSA
		"3320"
	))) %>% 
	mutate(
		code = case_when(
			# Jacksonville, FL
			code == "3600" ~ "3590",
			# Fall River, MA-RI SMA
			code == "2480" ~ "6481",
			# San Bernardino
			code == "7280" ~ "6780",
			# Fort Worth
			code == "2800" ~ "1921",
			# Santa Barbara
			code == "7480" ~ "7470",
			# Durham, NC
			code == "2280" ~ "6640",
			# El Paso
			code == "2320" ~ "2310",
			# Winston-Salem, NC
			code == "9220" ~ "3120",
			# Wilkes-Barre--Hazleton, PA SMSA
			code == "9120" ~ "7560",
			# Jersey City, NJ SMSA
			code == "3640" ~ "5603",
			# Lowell, MA SMSA
			code == "4560" ~ "1122",
			# Bay City, MI SMSA
			code == "0800" ~ "6960",
			# Paterson-Clifton-Passaic, NJ SMSA
			code == "6040" ~ "5602",
			# Ogden, UT SMSA
			code == "5840" ~ "7161",
			# Springfield, OH SMSA
			code == "7960" ~ "2001",
			# Newport News-Hampton, VA SMSA
			code == "5680" ~ "5721",
			# Gary-Hammond-East Chicago, IN SMSA
			code == "2960" ~ "1602",
			# New Britain, CT SMSA
			code == "5440" ~ "3283",
			# Lawrence-Haverhill, MA-NH SMSA
			code == "4160" ~ "1121",
			# Newark NJ
			code == "5640" ~ "5605",
			TRUE ~ code
		)
	)

ma_1960 <- full_join(
		msa_1960, 
		ma %>% 
			filter(year == 1960) %>% 
			filter(!(code %in% c("1260", "5990"))), 
		by = "code"
	) 


msa_1970 <- sf::read_sf("data/shapefiles/MSA_1970/") %>% 
	rmapshaper::ms_simplify(keep = 0.01) %>% 
	select(code = SMSAA, metarea = SMSA, geometry) %>%
	# Not in Ipums
	filter(!(code %in% c(
		# Meriden, CT SMSA
		"4960",
		# Ogden, UT SMSA
		"5840",
		# Durham, NC
		"2280",
		# Sherman-Denison, TX SMSA
		"7640",
		# Pine Bluff, AR SMSA
		"6240",
		# Honolulu, HI SMSA
		"3320"
	))) %>% 
	mutate(
		code = case_when(
			# Jacksonville, FL
			code == "3600" ~ "3590",
			# El Paso
			code == "2320" ~ "2310",
			# Newark, NJ
			code == "5640" ~ "5605",
			# Santa Barbara
			code == "7480" ~ "7470",
			# Springfield, OH SMSA
			code == "7960" ~ "2001",
			# San Bernardino
			code == "7280" ~ "6780",
			# Fort Worth
			code == "2800" ~ "1921",
			# Lowell, MA SMSA
			code == "4560" ~ "1122",
			# New Britain, CT SMSA
			code == "5440" ~ "3283",
			# Bay City, MI SMSA
			code == "0800" ~ "6960",
			# Gary-Hammond-East Chicago, IN SMSA
			code == "2960" ~ "1602",
			# Paterson-Clifton-Passaic, NJ SMSA
			code == "6040" ~ "5602",
			# Jersey City, NJ SMSA
			code == "3640" ~ "5603",
			# Fall River, MA-RI SMA
			code == "2480" ~ "6481",
			# Wilkes-Barre--Hazleton, PA SMSA
			code == "9120" ~ "7560",
			# Ogden, UT SMSA
			code == "5840" ~ "7161",
			# Newport News-Hampton, VA SMSA
			code == "5680" ~ "5721",
			# Lawrence-Haverhill, MA-NH SMSA
			code == "4160" ~ "1121",
			# Petersburg-Colonial Heights, VA SMSA
			code == "6140" ~ "6761",
			# Vallejo-napa, CA SMSA
			code == "8720" ~ "7362",
			# Anaheim-Santa Ana-Garden Grove, CA SMSA
			code == "0360" ~ "4481",
			# Oxnard-Ventura, CA SMSA
			code == "6000" ~ "8730",
			# Bristol, CT SMSA
			code == "1170" ~ "3281",
			TRUE ~ code
		)
	)


ma_1970 <- full_join(
		msa_1970, 
		ma %>% 
			filter(year == 1970) %>% 
			filter(!(code %in% c("2020", "2700", "3980", "4410", "6240", "7161", "7480", "9140", "9260"))), 
		by = "code"
	)

msa_1980 <- sf::read_sf("data/shapefiles/MSA_1980/") %>% 
	rmapshaper::ms_simplify(keep = 0.01) %>% 
	select(code = SMSAA, metarea = SMSA, geometry) %>%
	# Not in Ipums
	filter(!(code %in% c(
		# Meriden, CT SMSA 
		"4960",
		# NORTHEAST PENNSYLVANIA 
		"5745",
		# Pine Bluff, AR SMSA 
		"6240",
		# Anchorage, Alaska
		"0380",
		# Honolulu, HI SMSA
		"3320"
	))) %>% 
	mutate(
		code = case_when(
			# Jacksonville, FL 
			code == "3600" ~ "3590",
			# El Paso 
			code == "2320" ~ "2310",
			# Newark, NJ 
			code == "5640" ~ "5605",
			# Bristol, CT SMSA 
			code == "1170" ~ "3281",
			# Fall River, MA-RI SMA 
			code == "2480" ~ "6481",
			# Santa Barbara 
			code == "7480" ~ "7470",
			# Santa Cruz 
			code == "7485" ~ "7480", 
			# Springfield, OH SMSA 
			code == "7960" ~ "2001",
			# Oxnard-Ventura, CA SMSA 
			code == "6000" ~ "8730",
			# Jersey City, NJ SMSA 
			code == "3640" ~ "5603",
			# Paterson-Clifton-Passaic, NJ SMSA 
			code == "6040" ~ "5602",
			# Anaheim-Santa Ana-Garden Grove, CA SMSA 
			code == "0360" ~ "4481",
			# Vallejo-napa, CA SMSA 
			code == "8720" ~ "7362",
			# Gary-Hammond-East Chicago, IN SMSA 
			code == "2960" ~ "1602",
			# Newport News-Hampton, VA SMSA 
			code == "5680" ~ "5721",
			# New Britain, CT SMSA 
			code == "5440" ~ "3283",
			# Bay City, MI SMSA 
			code == "0800" ~ "6960",
			# Lawrence-Haverhill, MA-NH SMSA 
			code == "4160" ~ "1121",
			# Lowell, MA SMSA 
			code == "4560" ~ "1122",
			# Petersburg-Colonial Heights, VA SMSA 
			code == "6140" ~ "6761",
			# Newark, OH 
			code == "5645" ~ "5640",
			# Grand Forks, ND-MN 
			code == "2985" ~ "2990",
			# Jacksonville, NC 
			code == "3605" ~ "3600",
			# Nassau-Suffolk, NY 
			code == "5380" ~ "5601",
			# Oxnard-Simi Valley-Ventura, CA
			code == "6000" ~ "8730",
			# Elkhart 
			code == "2330" ~ "2320",
			# Elmira, NY
			code == "2335" ~ "2330",
			# Pascagoula-Moss Point, MS
			code == "6025" ~ "6030",
			# Anderson, SC
			code == "0405" ~ "3161",
			# Florence, SC
			code == "2655" ~ "2660",
			# Santa Cruz, CA
			code == "7485" ~ "7480",
			# Panama City, FL
			code == "6015" ~ "6010",
			# Rock Hill, SC
			code == "6885" ~ "1521",
			# Glenn Falls, NY
			code == "2975" ~ "2970",
			# Burlington, VT 
			code == "1305" ~ "1310",
			TRUE ~ code
		)
	)

ma_1980 <- full_join(
		msa_1980, 
		ma %>% 
			filter(year == 1980) %>% 
			filter(!(code %in% c("0460", "1921", "2320", "4760", "6240", "7560"))), 
		by = "code"
	) %>% view()
	
	

msa_1990 <- sf::read_sf("data/shapefiles/MSA_1990/") %>% 
	rmapshaper::ms_simplify(keep = 0.01) %>% 
	select(code = MSACMSA, geometry) %>% 
	# Not in Ipums
	filter(!(code %in% c(
		# Meriden, CT SMSA 
		"4960",
		# Pine Bluff, AR SMSA 
		"6240",
		# Anchorage, Alaska
		"0380",
		# Honolulu, HI SMSA
		"3320",
		# Not sure what these 3 are
		"0405", "1282", "6025"
	))) %>% 
		mutate(
			code = case_when(
				# Buffalo, NY
				code == "1282" ~ "1280",
				# Burlington, VT
				code == "1305" ~ "1310",
				# Cincinnati-Hamilton
				code == "1642" ~ "1640",
				# Panama City
				code == "6015" ~ "6010",
				# Grand Forks, ND
				msa == "2985" ~ "2990",
				# Elmira, NY
				code == "2335" ~ "2330",
				# Florence, SC
				code == "2655" ~ "2660",
				# Naples, FL
				code == "5345" ~ "5340",
				# Glens Falls, NY
				code == "2975" ~ "2970",
				# Jacksonville, NC
				code == "3605" ~ "3600",
				# Pittsburg
				code == "6282" ~ "6280",
				# Parkersburg/Marietta, WV/OH
				code == "6025" ~ "6020", 
				# Panama City, FL
				code == "6015" ~ "6010",
				TRUE ~ code
			)
		)

# Collapse to Combined Statistical Areas
ma_1990 <- ma %>% 
	filter(year == 1990) %>% 
	filter(!(code %in% c("0400"))) %>% 
	mutate(
		csa = case_when(
			# Detroit 
			code %in% c("0440", "2160", "2640") ~ "2162",
			# Seattle-Tacoma-Bremerton, WA
			code %in% c("1150", "5910", "7600", "8200") ~ "7602",
			# Dallas-Fort Worth, TX
			code %in% c("1920", "2800") ~ "1922",
			# Cleveland-Akron, OH
			code %in% c("0080", "1680") ~ "1692",
			# Miami-Fort Lauderdale, FL
			code %in% c("2680", "5000") ~ "4992",
			# Denver-Boulder-Greeley, CO
			code %in% c("2080", "2081", "3060") ~ "2082",
			# Milwaukee-Racine, WI
			code %in% c("5080", "6600") ~ "5082",
			# Philadelphia-Wilmington-Atlantic City, PA-NJ-DE-MD
			code %in% c("0560", "6160", "8760", "9160") ~ "6162",
			# Los Angeles-Riverside-Orange County, CA
			code %in% c("4480", "4481", "4482", "6780", "8730") ~ "4472",
			# Boston
			code %in% c("1120", "1200", "2600", "1121", "1122", "4760", "5350", "6450", "9240") ~ "1122",
			# Portland
			code %in% c("6640", "7080") ~ "6442", 
			
		
			TRUE ~ code 
		)
	) %>% 
	group_by(year, csa) %>% 
	summarize(log_ma = mean(log_ma)) %>%
	mutate(year = 1990) %>% 
	select(year, code = csa, log_ma)


full_join(
	msa_1990, 
	ma_1990,
	by = "code"
) %>% view()

view(anti_join(
	msa_1990, 
	ma_1990,
	by = "code"
))

view(anti_join(
	ma_1990,
	msa_1990, 
	by = "code"
))

plot(st_geometry(temp))

#

msa_2000 <- sf::read_sf("data/shapefiles/MSA_2000/")



#




ma_1940 <- ma %>% filter(year == 1940)


anti <- anti_join(msa_1950, ma_1940, by = c("msa" = "code"))

ggplot() +
	geom_sf(data = anti %>% st_as_sf())
	

anti_join(ma_1940, msa_1950, by = c("code" = "msa")) %>% view()






## Prepare geometries ----------------------------------------------------------

msa_shp <- sf::read_sf("data/shapefiles/msa_2000/US_msacmsa_2000.shp") %>% 
	rmapshaper::ms_simplify(keep = 0.01) %>% 
	select(msa = MSACMSA, geometry)


states <- tigris::states(class = "sf") %>% 
	rmapshaper::ms_simplify(keep = 0.025) %>% 
	filter(as.numeric(STATEFP) < 58 & NAME != "Alaska" & NAME != "Hawaii") %>% 
	st_transform(st_crs(msa_shp)) %>% 
	select(state_fips = STATEFP, geometry)

us <- states %>% summarize()

msa_by_state <- st_intersection(msa_shp, states)

nonurban <- st_difference(states, msa_shp %>% summarize()) %>% 
	mutate(msa = "0000")

msa_and_nonurban <- bind_rows(msa_by_state, nonurban)











anti <- anti_join(ma, msa_and_nonurban, by = c("state_fips", "code" = "msa"))


# 


ma_shp <- left_join(ma, msa_and_nonurban, by = c("state_fips", "code" = "msa")) %>% 
	st_as_sf()


## Plot: Market Access over Time -----------------------------------------------

ggplot() + 
	geom_sf(data = ma_shp %>% filter(year != 2005 & year != 2015), 
			aes(fill = log_ma), color = NA) +
	geom_sf()
	geom_sf(data= us, fill = NA, color= "grey30", size= 0.2) + 
	facet_wrap(~ year, ncol = 4) +
	# Remove Coordinates, leaving just the map
	coord_sf(datum = NA) +
	labs(
		fill = "Log of Market Access"
	) +
	theme_kyle() + 
	# Put Legend on Bottom
	scale_fill_viridis_c() + 
	guides(fill = guide_colorbar(title.position = "top", nrow = 1)) +
	theme(legend.position = "bottom")


## Plot: Change in Log MA 1940-2010 --------------------------------------------





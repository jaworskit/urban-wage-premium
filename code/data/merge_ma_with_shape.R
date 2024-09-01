## -----------------------------------------------------------------------------
## results-map_of_ma.R
## Kyle Butts, CU Boulder Economics
##
## Merge market access data at the MSA level with MSA shapefiles for 1940-2015. Also extracts lat-long from MSAs
##
## Files to run before:
## - marketaccess.do
## -----------------------------------------------------------------------------

library(tidyverse)
library(tidylog)
library(haven)
library(sf)
library(rmapshaper)
library(tigris)
library(glue)

gh <- "~/Documents/Projects/urban-wage-premium"
dropbox <- "~/Dropbox/UrbanWagePremium"
setwd(dropbox)

## Load MA data ----------------------------------------------------------------

ma <- haven::read_dta("data/dta/msa_market_access.dta") %>%
  mutate(
    state_fips = case_when(
      code >= 100000 ~ code / 100000
    ),
    code = sprintf("%04d", code),
    log_ma = log(ma),
    log_ma_removeown = log(ma_removeown)
  ) %>%
  select(code, year, everything()) %>%
  arrange(code, year)

ma_nonurban <- ma %>%
  filter(!is.na(state_fips))

ma <- ma %>%
  filter(is.na(state_fips)) %>%
  select(code, year, log_ma, log_ma_removeown)


## 1950 ------------------------------------------------------------------------

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
) %>%
  select(code, metarea, year, log_ma, log_ma_removeown, geometry)

ma_1950 <- full_join(
  msa_1950,
  ma %>%
    filter(year == 1950) %>%
    filter(!(code %in% c("2001", "2200", "7161"))),
  by = "code"
) %>%
  select(code, metarea, year, log_ma, log_ma_removeown, geometry)

## 1960 ------------------------------------------------------------------------

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
) %>%
  st_transform(st_crs(msa_1950)) %>%
  select(code, metarea, year, log_ma, log_ma_removeown, geometry)

## 1970 ------------------------------------------------------------------------

msa_1970 <- sf::read_sf("data/shapefiles/MSA_1970/") %>%
  rmapshaper::ms_simplify(keep = 0.01) %>%
  st_transform(st_crs(msa_1950)) %>%
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
) %>%
  select(code, metarea, year, log_ma, log_ma_removeown, geometry)


## 1980 ------------------------------------------------------------------------

msa_1980 <- sf::read_sf("data/shapefiles/MSA_1980/") %>%
  rmapshaper::ms_simplify(keep = 0.01) %>%
  st_transform(st_crs(msa_1950)) %>%
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
    filter(!(code %in% c("0460", "1921", "4760", "6240", "7560"))),
  by = "code"
) %>%
  select(code, year, log_ma, log_ma_removeown, geometry)


## 1990 ------------------------------------------------------------------------

msa_1990 <- sf::read_sf("data/shapefiles/MSA_1990/") %>%
  rmapshaper::ms_simplify(keep = 0.01) %>%
  st_transform(st_crs(msa_1950)) %>%
  select(code = MSACMSA, geometry, area = SHAPE_AREA) %>%
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

    # Next to Anderson, SC
    "0405"
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
      code == "2985" ~ "2990",
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
      # Florence, SC
      code == "2655" ~ "2660",
      # Glenn Falls, NY
      code == "2975" ~ "2970",
      # Grand Forks, ND
      code == "2985" ~ "2990",
      # El Paso
      code == "2320" ~ "2310",
      # Elkhart, IN
      code == "2330" ~ "2320",
      # Jacksonville Florida
      area == 7274545618.47 ~ "3590",
      # Santa Barbara, CA
      code == "7480" ~ "7470",
      TRUE ~ code
    )
  ) %>%
  select(-area)

# Collapse to Combined Statistical Areas
ma_1990 <- ma %>%
  filter(year == 1990) %>%
  filter(!(code %in% c("0400"))) %>%
  mutate(
    csa = case_when(
      # Detroit
      code %in% c("0440", "2160") ~ "2162",
      # Seattle-Tacoma-Bremerton, WA
      code %in% c("7600", "8200") ~ "7602",
      # Dallas-Fort Worth, TX
      code %in% c("1920", "2800") ~ "1922",
      # Cleveland-Akron, OH
      code %in% c("0080", "1680") ~ "1692",
      # Miami-Fort Lauderdale, FL
      code %in% c("2680", "5000") ~ "4992",
      # Denver-Boulder-Greeley, CO
      code %in% c("2080", "2081") ~ "2082",
      # Milwaukee-Racine, WI
      code %in% c("5080", "6600") ~ "5082",
      # Philadelphia-Wilmington-Atlantic City, PA-NJ-DE-MD
      code %in% c("6160", "8760", "9160") ~ "6162",
      # Los Angeles-Riverside-Orange County, CA
      code %in% c("4480", "4481", "4482", "6780", "8730") ~ "4472",
      # Boston
      code %in% c("1120", "1200", "1121", "1122", "1123", "5350") ~ "1122",
      # Portland
      code %in% c("6440") ~ "6442",
      # Houston-Galveston-Brazoria, TX
      code %in% c("3360", "2920", "1145") ~ "3362",
      # New Haven
      code %in% c("1160", "5480") ~ "5480",
      # Buffalo, NY
      code %in% c("1280", "1281") ~ "1280",
      # Chicago
      code %in% c("1600", "1601", "1602", "1603", "1604", "3800") ~ "1602",
      # Dallas, TX
      code %in% c("1920", "1921") ~ "1922",
      # Anderson, SC
      code %in% c("3160", "3161") ~ "3160",
      # Cincinnati-Hamilton, OH
      code %in% c("1640", "3200") ~ "1640",
      # Hartford, CT
      code %in% c("3280", "3281", "3282", "3282", "3283") ~ "3282",
      # Houstan, TX
      code %in% c("3360", "3361") ~ "3362",
      # Cleveland, OH
      code %in% c("1680", "0080", "4440") ~ "1692",
      # New York, NY
      code %in% c("5190", "5600", "5601", "5602", "5603", "5604", "5605", "5760", "5950", "8040", "8480", "1930") ~ "5602",
      # Pittsburgh, PA
      code %in% c("6280", "6281") ~ "6280",
      # Portland, OR
      code %in% c("6440", "6441") ~ "6442",
      # Providence, RI
      code %in% c("6480", "6481", "6482") ~ "6482",
      # San Francisco, CA
      code %in% c("7360", "7361", "7362", "7400", "7480", "7500") ~ "7362",
      TRUE ~ code
    )
  ) %>%
  group_by(year, csa) %>%
  summarize(log_ma = mean(log_ma), log_ma_removeown = mean(log_ma_removeown)) %>%
  mutate(year = 1990) %>%
  select(year, code = csa, log_ma, log_ma_removeown) %>%
  filter(!(code %in% c(
    "5790", # Ocala FL
    "6240" # Pine Bluff FL
  )))


ma_1990 <- full_join(
  msa_1990,
  ma_1990,
  by = "code"
) %>%
  select(code, year, log_ma, log_ma_removeown, geometry)


## 2000 ------------------------------------------------------------------------


msa_2000 <- sf::read_sf("data/shapefiles/MSA_2000/") %>%
  rmapshaper::ms_simplify(keep = 0.01) %>%
  st_transform(st_crs(msa_1950)) %>%
  select(code = MSACMSA, geometry, area = SHAPE_AREA) %>%
  # Not in Ipums
  filter(!(code %in% c(
    # Anchorage, Alaska
    "0380",
    # Honolulu, HI SMSA
    "3320"
  ))) %>%
  mutate(
    code = case_when(
      # Burlington, VT
      code == "1305" ~ "1310",
      # Panama City, FL
      code == "6015" ~ "6010",
      # Elmira, NY
      code == "2335" ~ "2330",
      # Florence, SC
      code == "2655" ~ "2660",
      # Glenn Falls, NY
      code == "2975" ~ "2970",
      # Grand Forks, ND
      code == "2985" ~ "2990",
      # Jacksonville, NC
      code == "3605" ~ "3600",
      # Naples, FL
      code == "5345" ~ "5340",
      # Panama City
      code == "6015" ~ "6010",
      # El Paso
      code == "2320" ~ "2310",
      # Jacksonville Florida
      area == 7274545618.47 ~ "3590",
      # Santa Barbara, CA
      code == "7480" ~ "7470",
      # Elkhart, IN
      code == "2330" ~ "2320",
      TRUE ~ code
    )
  ) %>%
  select(-area)

# Collapse to Combined Statistical Areas
ma_2000 <- ma %>%
  filter(year == 2000) %>%
  mutate(
    csa = case_when(
      # Houston-Galveston-Brazoria, TX
      code %in% c("3360", "2920", "1145") ~ "3362",
      # Cincinnati-Hamilton, OH
      code %in% c("1640", "3200") ~ "1642",
      # Cleveland-Akron, OH
      code %in% c("0080", "1680") ~ "1692",
      # Dallas-Fort Worth, TX
      code %in% c("1920", "2800") ~ "1922",
      # Denver-Boulder-Greeley, CO
      code %in% c("2080", "2081", "3060") ~ "2082",
      # Detroit
      code %in% c("0440", "2160", "2640") ~ "2162",
      # Philadelphia-Wilmington-Atlantic City, PA-NJ-DE-MD
      code %in% c("6160", "8760", "9160", "0560") ~ "6162",
      # Los Angeles-Riverside-Orange County, CA
      code %in% c("4480", "4481", "4482", "6780", "8730") ~ "4472",
      # Miami-Fort Lauderdale, FL
      code %in% c("2680", "5000") ~ "4992",
      # Milwaukee-Racine, WI
      code %in% c("5080", "6600") ~ "5082",
      # Portland
      code %in% c("6440", "7080") ~ "6442",
      # Sacramento
      code %in% c("9270", "6920") ~ "6922",
      # Seattle-Tacoma-Bremerton, WA
      code %in% c("7600", "8200", "1150", "5910") ~ "7602",
      # Washington-Baltimore
      code %in% c("0720", "3180", "8840") ~ "8872",
      # Boston
      code %in% c("1120", "1200", "1121", "1122", "1123", "5350", "2600", "4760", "5400", "6450", "9240") ~ "1122",
      # Chicago
      code %in% c("1600", "1601", "1602", "1603", "1604", "3800", "3740") ~ "1602",
      # Dallas, TX
      code %in% c("1920", "1921") ~ "1922",
      # New York, NY
      code %in% c("5190", "5600", "5601", "5602", "5603", "5604", "5605", "5760", "5950", "8040", "8480", "2281", "5660", "8880", "1930", "1160", "5480") ~ "5602",
      # Houstan, TX
      code %in% c("3360", "3361") ~ "3362",
      # Norfolk-Newsport Beach
      code %in% c("5720", "5721") ~ "5720",
      # San Francisco, CA
      code %in% c("7360", "7361", "7362", "7400", "7480", "7500") ~ "7362",
      TRUE ~ code
    )
  ) %>%
  group_by(year, csa) %>%
  summarize(log_ma = mean(log_ma), log_ma_removeown = mean(log_ma_removeown)) %>%
  mutate(year = 2000) %>%
  select(year, code = csa, log_ma, log_ma_removeown)


ma_2000 <- full_join(
  msa_2000,
  ma_2000,
  by = "code"
) %>%
  select(code, year, log_ma, log_ma_removeown, geometry)

## 2005 ------------------------------------------------------------------------

# Collapse to Combined Statistical Areas
ma_2005 <- ma %>%
  filter(year == 2005) %>%
  mutate(
    csa = case_when(
      # Houston-Galveston-Brazoria, TX
      code %in% c("3360", "2920", "1145") ~ "3362",
      # Cincinnati-Hamilton, OH
      code %in% c("1640", "3200") ~ "1642",
      # Cleveland-Akron, OH
      code %in% c("0080", "1680") ~ "1692",
      # Dallas-Fort Worth, TX
      code %in% c("1920", "2800") ~ "1922",
      # Denver-Boulder-Greeley, CO
      code %in% c("2080", "2081", "3060") ~ "2082",
      # Detroit
      code %in% c("0440", "2160", "2640") ~ "2162",
      # Philadelphia-Wilmington-Atlantic City, PA-NJ-DE-MD
      code %in% c("6160", "8760", "9160", "0560") ~ "6162",
      # Los Angeles-Riverside-Orange County, CA
      code %in% c("4480", "4481", "4482", "6780", "8730") ~ "4472",
      # Miami-Fort Lauderdale, FL
      code %in% c("2680", "5000") ~ "4992",
      # Milwaukee-Racine, WI
      code %in% c("5080", "6600") ~ "5082",
      # Portland
      code %in% c("6440", "7080") ~ "6442",
      # Sacramento
      code %in% c("9270", "6920") ~ "6922",
      # Seattle-Tacoma-Bremerton, WA
      code %in% c("7600", "8200", "1150", "5910") ~ "7602",
      # Washington-Baltimore
      code %in% c("0720", "3180", "8840") ~ "8872",
      # Boston
      code %in% c("1120", "1200", "1121", "1122", "1123", "5350", "2600", "4760", "5400", "6450", "9240") ~ "1122",
      # Chicago
      code %in% c("1600", "1601", "1602", "1603", "1604", "3800", "3740") ~ "1602",
      # Dallas, TX
      code %in% c("1920", "1921") ~ "1922",
      # New York, NY
      code %in% c("5190", "5600", "5601", "5602", "5603", "5604", "5605", "5760", "5950", "8040", "8480", "2281", "5660", "8880", "1930", "1160", "5480") ~ "5602",
      # Houstan, TX
      code %in% c("3360", "3361") ~ "3362",
      # Norfolk-Newsport Beach
      code %in% c("5720", "5721") ~ "5720",
      # San Francisco, CA
      code %in% c("7360", "7361", "7362", "7400", "7480", "7500") ~ "7362",
      TRUE ~ code
    )
  ) %>%
  group_by(year, csa) %>%
  summarize(log_ma = mean(log_ma), log_ma_removeown = mean(log_ma_removeown)) %>%
  mutate(year = 2005) %>%
  select(year, code = csa, log_ma, log_ma_removeown)


ma_2005 <- full_join(
  msa_2000,
  ma_2005,
  by = "code"
) %>%
  select(code, year, log_ma, log_ma_removeown, geometry)


## 2010 ------------------------------------------------------------------------

# Collapse to Combined Statistical Areas
ma_2010 <- ma %>%
  filter(year == 2010) %>%
  mutate(
    csa = case_when(
      # Houston-Galveston-Brazoria, TX
      code %in% c("3360", "2920", "1145") ~ "3362",
      # Cincinnati-Hamilton, OH
      code %in% c("1640", "3200") ~ "1642",
      # Cleveland-Akron, OH
      code %in% c("0080", "1680") ~ "1692",
      # Dallas-Fort Worth, TX
      code %in% c("1920", "2800") ~ "1922",
      # Denver-Boulder-Greeley, CO
      code %in% c("2080", "2081", "3060") ~ "2082",
      # Detroit
      code %in% c("0440", "2160", "2640") ~ "2162",
      # Philadelphia-Wilmington-Atlantic City, PA-NJ-DE-MD
      code %in% c("6160", "8760", "9160", "0560") ~ "6162",
      # Los Angeles-Riverside-Orange County, CA
      code %in% c("4480", "4481", "4482", "6780", "8730") ~ "4472",
      # Miami-Fort Lauderdale, FL
      code %in% c("2680", "5000") ~ "4992",
      # Milwaukee-Racine, WI
      code %in% c("5080", "6600") ~ "5082",
      # Portland
      code %in% c("6440", "7080") ~ "6442",
      # Sacramento
      code %in% c("9270", "6920") ~ "6922",
      # Seattle-Tacoma-Bremerton, WA
      code %in% c("7600", "8200", "1150", "5910") ~ "7602",
      # Washington-Baltimore
      code %in% c("0720", "3180", "8840") ~ "8872",
      # Boston
      code %in% c("1120", "1200", "1121", "1122", "1123", "5350", "2600", "4760", "5400", "6450", "9240") ~ "1122",
      # Chicago
      code %in% c("1600", "1601", "1602", "1603", "1604", "3800", "3740") ~ "1602",
      # Dallas, TX
      code %in% c("1920", "1921") ~ "1922",
      # New York, NY
      code %in% c("5190", "5600", "5601", "5602", "5603", "5604", "5605", "5760", "5950", "8040", "8480", "2281", "5660", "8880", "1930", "1160", "5480") ~ "5602",
      # Houstan, TX
      code %in% c("3360", "3361") ~ "3362",
      # Norfolk-Newsport Beach
      code %in% c("5720", "5721") ~ "5720",
      # San Francisco, CA
      code %in% c("7360", "7361", "7362", "7400", "7480", "7500") ~ "7362",
      TRUE ~ code
    )
  ) %>%
  group_by(year, csa) %>%
  summarize(log_ma = mean(log_ma), log_ma_removeown = mean(log_ma_removeown)) %>%
  mutate(year = 2010) %>%
  select(year, code = csa, log_ma, log_ma_removeown)


ma_2010 <- full_join(
  msa_2000,
  ma_2010,
  by = "code"
) %>%
  select(code, year, log_ma, log_ma_removeown, geometry)


## 2015 ------------------------------------------------------------------------

msa_2015 <- sf::read_sf("data/shapefiles/CBSA_2015/") %>%
  rmapshaper::ms_simplify(keep = 0.01) %>%
  st_transform(st_crs(msa_1950)) %>%
  select(code = CBSAFP, geometry)

ma_2015 <- ma %>%
  filter(year == 2015) %>%
  mutate(
    # Combined Statistical Area
    csa = case_when(
      # LA and Irvine
      code %in% c("11244", "31080") ~ "31080",
      # Boston, Cambridge, and Rockingham County NH
      code %in% c("15764", "40484", "14460") ~ "14460",
      # Philly, Camden, NJ, and Wilmington
      code %in% c("15804", "33874", "48864", "37980") ~ "37980",
      # Chicago, Elgin, Kenosha County, WI, and Gary, IN
      code %in% c("20994", "29404", "23844", "16980") ~ "16980",
      # Miami Fort Lauderdale, and West Palm, FL
      code %in% c("22744", "48424", "33100") ~ "33100",
      # Dallas and Fort Worth
      code %in% c("23104", "19100") ~ "19100",
      # NYC, Newark, and Jersey City
      code %in% c("35084", "35004", "20524", "35620") ~ "35620",
      # SF, Oakland, and San Rafael, CA
      code %in% c("36084", "41860", "42034") ~ "41860",
      # DC and Silver Spring, MD
      code %in% c("43524", "47900") ~ "47900",
      # Seattle and Tacoma, WA
      code %in% c("45104", "42660") ~ "42660",
      # Detroit and Warren
      code %in% c("47664", "19820") ~ "19820",
      TRUE ~ code
    )
  ) %>%
  group_by(year, csa) %>%
  summarize(log_ma = mean(log_ma), log_ma_removeown = mean(log_ma_removeown)) %>%
  ungroup() %>%
  mutate(year = 2015) %>%
  select(year, code = csa, log_ma, log_ma_removeown)

ma_2015 <- left_join(ma_2015, msa_2015, by = "code") %>%
  st_as_sf() %>%
  st_transform(st_crs(msa_1950))


## Prepare geometries ----------------------------------------------------------


states <- tigris::states(cb = TRUE, class = "sf") %>%
  rmapshaper::ms_simplify(keep = 0.025) %>%
  filter(as.numeric(STATEFP) < 58 & NAME != "Alaska" & NAME != "Hawaii" & STATEFP != 11) %>%
  st_transform(st_crs(msa_1950)) %>%
  select(state_fips = STATEFP, geometry)

us <- states %>% summarise()

## Non-urban
nonurban_1940 <- st_difference(states, msa_1950 %>% summarize()) %>%
  mutate(state_fips = as.numeric(state_fips)) %>%
  left_join(., ma_nonurban %>% filter(year == 1940), by = "state_fips") %>%
  select(code, year, log_ma, log_ma_removeown, geometry)

nonurban_1950 <- st_difference(states, msa_1950 %>% summarize()) %>%
  mutate(state_fips = as.numeric(state_fips)) %>%
  left_join(., ma_nonurban %>% filter(year == 1950), by = "state_fips") %>%
  select(code, year, log_ma, log_ma_removeown, geometry)

nonurban_1960 <- st_difference(states, msa_1960 %>% summarize()) %>%
  mutate(state_fips = as.numeric(state_fips)) %>%
  left_join(., ma_nonurban %>% filter(year == 1960), by = "state_fips") %>%
  select(code, year, log_ma, log_ma_removeown, geometry)

nonurban_1970 <- st_difference(states, msa_1970 %>% summarize()) %>%
  mutate(state_fips = as.numeric(state_fips)) %>%
  left_join(., ma_nonurban %>% filter(year == 1970), by = "state_fips") %>%
  select(code, year, log_ma, log_ma_removeown, geometry)

nonurban_1980 <- st_difference(states, msa_1980 %>% summarize()) %>%
  mutate(state_fips = as.numeric(state_fips)) %>%
  left_join(., ma_nonurban %>% filter(year == 1980), by = "state_fips") %>%
  select(code, year, log_ma, log_ma_removeown, geometry)

nonurban_1990 <- st_difference(states, msa_1990 %>% summarize()) %>%
  mutate(state_fips = as.numeric(state_fips)) %>%
  left_join(., ma_nonurban %>% filter(year == 1990), by = "state_fips") %>%
  select(code, year, log_ma, log_ma_removeown, geometry)

nonurban_2000 <- st_difference(states, msa_2000 %>% summarize()) %>%
  mutate(state_fips = as.numeric(state_fips)) %>%
  left_join(., ma_nonurban %>% filter(year == 2000), by = "state_fips") %>%
  select(code, year, log_ma, log_ma_removeown, geometry)

nonurban_2005 <- st_difference(states, msa_2000 %>% summarize()) %>%
  mutate(state_fips = as.numeric(state_fips)) %>%
  left_join(., ma_nonurban %>% filter(year == 2005), by = "state_fips") %>%
  select(code, year, log_ma, log_ma_removeown, geometry)

nonurban_2010 <- st_difference(states, msa_2000 %>% summarize()) %>%
  mutate(state_fips = as.numeric(state_fips)) %>%
  left_join(., ma_nonurban %>% filter(year == 2010), by = "state_fips") %>%
  select(code, year, log_ma, log_ma_removeown, geometry)

nonurban_2015 <- st_difference(states, ma_2015 %>% st_make_valid() %>% st_buffer(0) %>% summarize()) %>%
  mutate(state_fips = as.numeric(state_fips)) %>%
  left_join(., ma_nonurban %>% filter(year == 2015), by = "state_fips") %>%
  select(code, year, log_ma, log_ma_removeown, geometry)

ma_1940_all <- bind_rows(ma_1940, nonurban_1940) %>% st_as_sf()
ma_1950_all <- bind_rows(ma_1950, nonurban_1950) %>% st_as_sf()
ma_1960_all <- bind_rows(ma_1960, nonurban_1960) %>% st_as_sf()
ma_1970_all <- bind_rows(ma_1970, nonurban_1970) %>% st_as_sf()
ma_1980_all <- bind_rows(ma_1980, nonurban_1980) %>% st_as_sf()
ma_1990_all <- bind_rows(ma_1990, nonurban_1990) %>% st_as_sf()
ma_2000_all <- bind_rows(ma_2000, nonurban_2000) %>% st_as_sf()
ma_2005_all <- bind_rows(ma_2005, nonurban_2005) %>% st_as_sf()
ma_2010_all <- bind_rows(ma_2010, nonurban_2010) %>% st_as_sf()
ma_2015_all <- bind_rows(ma_2015, nonurban_2015) %>% st_as_sf()

ma_allyrs <- bind_rows(ma_1940_all, ma_1950_all, ma_1960_all, ma_1970_all, ma_1980_all, ma_1990_all, ma_2000_all, ma_2005_all, ma_2010_all, ma_2015_all)

save(list = c("ma_allyrs", "us"), file = glue("{dropbox}/data/msa_with_shape/msa_with_shape.RData"))


## Extract lat-long

ma_allyrs %>%
  st_transform(st_crs(4136)) %>%
  mutate(
    centroid = st_point_on_surface(geometry),
    lon = st_coordinates(centroid)[, 1],
    lat = st_coordinates(centroid)[, 2],
    code = as.numeric(code)
  ) %>%
  as_tibble() %>%
  select(code, year, lon, lat) %>%
  # Remove few duplicates
  group_by(code, year) %>%
  filter(row_number() == 1) %>%
  ungroup() %>%
  haven::write_dta(., path = glue("{dropbox}/data/crosswalk/msa_lat_long.dta"))


states %>%
  st_transform(st_crs(4136)) %>%
  mutate(
    centroid = st_point_on_surface(geometry),
    lon = st_coordinates(centroid)[, 1],
    lat = st_coordinates(centroid)[, 2],
    code = as.numeric(state_fips) * 100000
  ) %>%
  as_tibble() %>%
  select(code, lon, lat) %>%
  haven::write_dta(., path = glue("{dropbox}/data/crosswalk/state_lat_long.dta"))

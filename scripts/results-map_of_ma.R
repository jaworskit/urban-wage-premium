## -----------------------------------------------------------------------------
## results-map_of_ma.R
## Kyle Butts, CU Boulder Economics 
## 
## Creates maps of Market Access over time (Facet Wrap) and a % change in MA from 1940 to 2010
##
## Files to run before:
## - data-marketaccess.do
## - data-merge_ma_with_shape.R
## -----------------------------------------------------------------------------

library(tidyverse)
library(tidylog)
library(glue)
library(haven)
library(sf) 
library(rmapshaper)
library(tigris)
library(patchwork)

gh <- "~/Documents/Projects/urban-wage-premium"
dropbox <- "~/Dropbox/UrbanWagePremium/" 

setwd(dropbox)

# Custom Theme 
source("https://gist.githubusercontent.com/kylebutts/7dc66a01ec7e499faa90b4f1fd46ef9f/raw/ea949343b6f486d7453b12465b8f375b8b67093f/theme_kyle.R")


## Load MA with Shapefile from `data-merge_ma_with_shape.R`

load(glue("{dropbox}/data/msa_with_shape/msa_with_shape.RData"))


## Plot: Market Access over Time -----------------------------------------------

(ma_over_time_plot <- ggplot() + 
 	geom_sf(data = ma_allyrs %>% filter(!is.na(year)), aes(fill = log_ma), color = NA) +
 	geom_sf(data= us, fill = NA, color= "grey30", size= 0.2) + 
 	facet_wrap(~ year, ncol = 5) +
 	# Remove Coordinates, leaving just the map
 	coord_sf(datum = NA) +
 	labs(
 		fill = "Mean of Log of Market Access"
 	) +
 	theme_kyle() + 
 	# Put Legend on Bottom
 	scale_fill_distiller(type = "div", palette = "Spectral") +
 	# scale_fill_viridis_c() + 
 	# scale_fill_gradientn(colors = sf.colors()) +
 	guides(fill = guide_colorbar(title.position = "top", nrow = 1)) +
 	theme(
 		legend.position = "bottom",
 		legend.key.width = unit(1, "cm")
 	))

ggsave(
	glue("{gh}/paper/figures/ma_over_time.jpg"), ma_over_time_plot, 
	dpi = 300, width = 4800/300, height = 2400/300
)

## Plot: Market Access Deviation over Time -------------------------------------
ma_norm <- ma_allyrs %>% 
	group_by(year) %>% 
	mutate(log_ma = log_ma - mean(log_ma)) %>% 
	ungroup() %>% 
	filter(!is.na(year))

(ma_deviations_over_time_plot <- ggplot() + 
	geom_sf(data = ma_norm, aes(fill = log_ma), color = NA) +
	geom_sf(data= us, fill = NA, color= "grey30", size= 0.2) + 
	facet_wrap(~ year, ncol = 5) +
	# Remove Coordinates, leaving just the map
	coord_sf(datum = NA) +
	labs(
		fill = "Deviations from Yearly\nMean of Log of Market Access"
	) +
	theme_kyle() + 
	# Put Legend on Bottom
	scale_fill_distiller(type = "div", palette = "Spectral", limits = c(-4, 4)) +
	# scale_fill_viridis_c() + 
	# scale_fill_gradientn(colors = sf.colors()) +
	guides(fill = guide_colorbar(title.position = "top", nrow = 1)) +
	theme(
		legend.position = "bottom",
		legend.key.width = unit(1, "cm")
	))

ggsave(
	glue("{gh}/paper/figures/ma_deviations_over_time.jpg"), ma_deviations_over_time_plot, 
	dpi = 300, width = 4800/300, height = 2400/300
)

## Plot: Change in Log MA 1940-2010 --------------------------------------------

ma_change <- ma_allyrs %>%
	filter(year == 1940 | year == 2010) %>% 
	mutate(
		code = case_when(
			# Houston-Galveston-Brazoria, TX
			code == "3362" ~ "3360",
			# Cincinnati-Hamilton, OH
			code == "1642" ~ "1640",
			# Cleveland-Akron, OH
			code == "1692" ~ "1680",
			# Dallas-Fort Worth, TX
			code == "1922" ~ "1920",
			# Denver-Boulder-Greeley, CO
			code == "2082" ~ "2080",
			# Detroit 
			code == "2162" ~ "2160",
			# Philadelphia-Wilmington-Atlantic City, PA-NJ-DE-MD
			code == "6162" ~ "6160",
			# Los Angeles-Riverside-Orange County, CA
			code == "4472" ~ "4480",
			# Miami-Fort Lauderdale, FL
			code == "4992" ~ "5000",
			# Milwaukee-Racine, WI
			code == "5082" ~ "5080",
			# Portland
			code == "6442" ~ "6440", 
			# Sacramento
			code == "6922" ~ "6920",
			# Seattle-Tacoma-Bremerton, WA
			code == "7602" ~ "7600",
			# Washington-Baltimore
			code == "8872" ~ "8840",
			# Boston
			code == "1122" ~ "1120",
			# Chicago
			code == "1602" ~ "1600",
			# Dallas, TX
			code == "1922" ~ "1920",
			# New York, NY
			code == "5602" ~ "5600",
			# Houstan, TX
			code == "3362" ~ "3360",
			# Norfolk-Newsport Beach
			code == "5722" ~ "5720",
			# San Francisco, CA
			code == "7362" ~ "7360",
			TRUE ~ code
		)
	) %>% 
	complete(code, year) %>% 
	group_by(code) %>% 
	mutate(log_ma = log_ma - first(log_ma)) %>% 
	filter(year == 2010) %>%
	ungroup() %>%
	st_as_sf()

(ma_change_plot <- ggplot() + 
	geom_sf(data = ma_change, aes(fill = log_ma), color = NA) +
	geom_sf(data= us, fill = NA, color= "grey30", size= 0.2) + 
	# Remove Coordinates, leaving just the map
	coord_sf(datum = NA) +
	labs(
		fill = "Change in Log of\nMarket Access, 1940-2010"
	) +
	theme_kyle() + 
	# Put Legend on Bottom
	scale_fill_distiller(palette = "PuBu", direction = -1) +
	# scale_fill_viridis_c() + 
	# scale_fill_gradientn(colors = sf.colors()) +
	guides(fill = guide_colorbar(title.position = "top", nrow = 1)) +
	theme(
		legend.position = "bottom",
		legend.key.width = unit(1, "cm")
	))

ggsave(
	glue("{gh}/paper/figures/ma_1940_to_2010.jpg"), ma_change_plot, 
	dpi = 300, width = 4800/300, height = 2400/300
)

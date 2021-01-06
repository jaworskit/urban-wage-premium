## -----------------------------------------------------------------------------
## data-gen_ma_instrument.R
## Kyle Butts, CU Boulder Economics 
## 
## Implementation of Bursyak and Hull (2020). Generates various highway systems and calculates market access with the trade costs produced following Jaworski and Kitchens (2018).
##
## Files to run before:
## - data-marketaccess.do
## -----------------------------------------------------------------------------

library(tidyverse)
library(haven)
library(sf) 
library(rmapshaper)
library(glue)
library(stringr)

library(Rcpp)
library(RcppArmadillo)


gh <- "~/Documents/Projects/urban-wage-premium"
dropbox <- "~/Dropbox/UrbanWagePremium" 
shape <- "~/Dropbox/UrbanWagePremium/data/shapefiles/shapefiles_dissolved_network" 

setwd(dropbox)


## Load 2010 Counties ----------------------------------------------------------

# 1609.344 m = 1 mi 
M_to_MI <- 1/1609.344

matlab_fips <- read_csv(glue("{dropbox}/data/matlab/input/FIPS.csv"), col_names = "fips")

counties <- read_sf(glue("{dropbox}/data/shapefiles/us_county_2010/counties_simplified.json")) %>%
	select(state_fips = STATE, county_fips = COUNTY, geometry) %>% 
	# Remove Puerto Rico, Hawaii, and Alaska
	filter(!(state_fips %in% c("02", "15", "72"))) %>% 
	st_make_valid() %>%
	st_transform(2163) %>%
	mutate(
		fips = as.numeric(paste0(state_fips, county_fips)),
		nodeID = 1000000 + fips
	) %>% 
	tidylog::filter(fips %in% matlab_fips$fips) %>% 
	arrange(nodeID)
 
# Get centroids
centroids <- counties %>% 
	mutate(centroids = st_point_on_surface(geometry)) %>% 
	st_drop_geometry() %>% st_as_sf()

# plot(st_geometry(centroids))


## Load Roads Shape ------------------------------------------------------------ 
 
roads_raw <- read_sf(glue("{shape}/roads1930.shp")) %>% 
	st_make_valid() %>% 
	st_transform(2163) %>%
	filter(!st_is_empty(.$geometry))

roads <- roads_raw %>% 
	filter(!is.na(roadClass)) %>% 
	select(speed1, speed2, segmentID, roadClass, distance, cost1, cost2, geometry) %>% 
	mutate(edgeID = 1:n())


## Network from Roads ----------------------------------------------------------
# https://www.r-spatial.org/r/2019/09/26/spatial-networks.html

# Get start and end points of lines
nodes <- roads %>%
	st_coordinates() %>%
	as_tibble() %>%
	rename(edgeID = L1) %>%
	group_by(edgeID) %>%
	slice(c(1, n())) %>%
	ungroup() %>%
	mutate(start_end = rep(c('start', 'end'), times = n()/2))

# Simplify to remove duplicates
nodes <- nodes %>%
	group_by(X, Y) %>% 
	mutate(
		nodeID = cur_group_id()
	) %>% 
	ungroup()

source_nodes <- nodes %>%
	filter(start_end == 'start') %>%
	pull(nodeID)

target_nodes <- nodes %>%
	filter(start_end == 'end') %>%
	pull(nodeID)


edges <- roads %>%
	mutate(from = source_nodes, to = target_nodes)

nodes <- nodes %>%
	distinct(nodeID, .keep_all = TRUE) %>%
	select(-c(edgeID, start_end)) %>%
	st_as_sf(coords = c('X', 'Y')) %>%
	st_set_crs(st_crs(edges))


## Create County Center to endpoints of segments within county -----------------


within <- st_within(nodes, counties)
which_county <- purrr::map_dbl(within, magrittr::extract, 1)


# find county centroid for each node
counties_to_roads <- nodes %>% 
	mutate(
		centroid = centroids[which_county, "centroids"],
		from = centroids[which_county, ] %>% st_drop_geometry() %>% pull(nodeID),
		to = nodeID
	) %>%
	filter(!is.na(from)) %>%
	# For now, assuming costless to get to any highway segment in county
	# This could be due to firms endogenously choosing location to be near highway.
	mutate(
		roadClass = "access",
		cost1 = 0, 
		cost2 = 0
	) %>% 
	as_tibble() %>% select(-centroid, -geometry)

# Counties can also travel via access road to their k-nearest neighbors. k = 10

nn <- nngeo::st_nn(centroids, centroids, k = 11, parallel = 14)

neighbors <- purrr::map(nn, 
						~ centroids[.,] %>% 
							dplyr::rename(to = nodeID, centroids_to = centroids) %>% 
							select(to, centroids_to)
						)


access <- centroids %>% 
	select(from = nodeID, centroids) %>% 
	mutate(
		neighbors = neighbors
	) %>% 
	# much faster than dplyr::unnest
	data.table::as.data.table() %>% 
	tidyfast::dt_unnest(neighbors) %>% 
	# get county centroids
	left_join(., centroids %>% select(nodeID), by = c("from" = "nodeID")) %>% 
	as_tibble() %>% 
	# remove self-county %>% 
	filter(from != to) %>% 
	# Calculate cost
	mutate(
		dist = st_distance(centroids, centroids_to, by_element = TRUE) %>% 
			units::set_units("mi") %>% 
			units::drop_units(),
		# Travel Cost, assuming fuel costs and driver wage
		cost1 = (dist * 10 * 18) + (dist * 1),
		roadClass = "dirt"
	) %>% 
	select(-centroids, -centroids_to)
	

edges_all <- bind_rows(edges, counties_to_roads, access)


## Export Road Network ---------------------------------------------------------

save(edges_all, file = glue("{dropbox}/data/road_network/network.Rdata"))

load(file = glue("{dropbox}/data/road_network/network.Rdata"))



## Matrix of shortest distances ------------------------------------------------

county_id <- counties$nodeID
Graph <- edges_all %>% 
	st_drop_geometry() %>% as_tibble() %>% 
	select(from, to, cost = cost1) %>% 
	cppRouting::makegraph(directed = FALSE)

# Dijkstra's Algorithm
tau <- cppRouting::get_distance_matrix(Graph, from = county_id, to = county_id, allcores = TRUE)
diag(tau) <- 1

## Market Access ---------------------------------------------------------------

Y <- read_csv(glue("{dropbox}/data/matlab/input/Y1940.csv"), col_names = FALSE)
Y <- as.matrix(Y)
Counties <- length(Y)
th <- 8

# temp
tau1940 <- read_csv(glue("{dropbox}/data/matlab/input/tau1940cost1.csv"), col_names = FALSE)
tau1940 <- as.matrix(tau)

# Load SolveMA function
Rcpp::sourceCpp(glue("{gh}/scripts/solveMA.cpp"))

# Solve for MA
ma <- tibble(fips = counties$fips, ma = SolveMA(Y, tau, th, Counties))





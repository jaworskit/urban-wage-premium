# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
library(here)
library(tictoc)
run_script <- function(file) {
  cat(sprintf("\nRunning: %s\n\n", here(file)))
  tictoc::tic(file)
  source(here(file))
  cat("\n")
  tictoc::toc()
  cat("\n")
}

# STATA CODE MUST BE RUN FIRST ----
## Creates MSA crosswalk from 1940-2020
# do code/data/metarea.do

## From IPUMS files, creates individual survey dataset
# do code/data/IPUMS-compile.do

## Creates market access dataset and housing prices in 1950
# do code/data/marketaccess.do

## Merges individual survey data with additional variables
# do code/data/merge.do

# Data ----
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

## Convert to parquet dataset (much faster loading and filtering)
run_script("code/data/convert_to_parquet.R")

## Create shape file of MSAs matched with data
# run_script("code/data/merge_ma_with_shape.R")

# Analysis ----
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

## Summary statistics
run_script("code/analysis/top_paying_cities.R")
run_script("code/analysis/summary.R")
run_script("code/analysis/msa_map.R")
# run_script("code/analysis/map_of_ma.R")

## Analysis checking Altonji and Mansfield assumptions
run_script("code/analysis/PCA_group_averages.R")
run_script("code/analysis/group_averages_on_diamonds_amenities.R")

## Estimate wage premium
run_script("code/analysis/urban_premium.R")
run_script("code/analysis/female_urban_premium.R")

## Robustness of main estimates
run_script("code/analysis/robustness_full_time_employees.R")
run_script("code/analysis/importance_of_market_access.R")
run_script("code/analysis/leave_one_out_group_averages.R")

## Net-of-housing-costs
run_script("code/analysis/housing_costs.R")

## Heterogeneity
run_script("code/analysis/heterogeneity.R")

## Distributional estimates
run_script("code/analysis/distribution.R")
## shell Rscript code/analysis/convergence.R

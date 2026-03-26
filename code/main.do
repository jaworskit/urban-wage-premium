********************************************************************************
* master.do
*
* This file runs scripts from start to finish
********************************************************************************

cls
clear all

********************************************************************************

global project "/Users/taylorjaworski/Dropbox/Research/Papers/UrbanWagePremium/"
global gh "/Users/taylorjaworski/Github/urban-wage-premium/"
if c(username) == "kbutts" {
	global project "~/Dropbox/Projects/UrbanWagePremium"
	global gh "~/Documents/Projects/urban-wage-premium"
}

cd $gh


********************************************************************************
* Data Cleaning
********************************************************************************

* Matlab script to create market access and remove-own market access
* run findMA.m

* Creates MSA crosswalk from 1940-2020
do code/data/metarea.do

* From IPUMS files, creates individual survey dataset 
do code/data/IPUMS-compile.do

* Creates market access dataset and housing prices in 1950
do code/data/marketaccess.do

* Merges individual survey data with additional variables
do code/data/merge.do

* Convert to parquet dataset (much faster loading and filtering)
shell Rscript code/data/convert_to_parquet.R

* Create shape file of MSAs matched with data
shell Rscript code/data/merge_ma_with_shape.R


********************************************************************************
* Analysis
********************************************************************************

* Summary statistics
shell Rscript code/analysis/top_paying_cities.R
shell Rscript code/analysis/summary.R
shell Rscript code/analysis/msa_map.R
* shell Rscript code/analysis/map_of_ma.R

* Analysis checking Altonji and Mansfield assumptions
shell Rscript code/analysis/PCA_group_averages.R
shell Rscript code/analysis/group_averages_on_diamonds_amenities.R

* Estimate wage premium
shell Rscript code/analysis/urban_premium.R
shell Rscript code/analysis/female_urban_premium.R

* Robustness checks on main estimates
shell Rscript code/analysis/robustness_full_time_employees.R
shell Rscript code/analysis/importance_of_market_access.R
shell Rscript code/analysis/leave_one_out_group_averages.R

* Net-of-housing-costs
shell Rscript code/analysis/housing_costs.R

* Heterogeneity
shell Rscript code/analysis/heterogeneity.R

* Distributional estimates
shell Rscript code/analysis/distribution.R
* shell Rscript code/analysis/convergence.R


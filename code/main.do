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
if c(username) == "kylebutts" {
	global project "~/Dropbox/UrbanWagePremium"
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

* Creates group averages 
do code/data/group-averages.R

* Convert to parquet dataset (much faster loading and filtering)
shell Rscript code/data/convert_to_parquet.R

* Create shape file of MSAs matched with data
shell Rscript code/data/merge_ma_with_shape.R


********************************************************************************
* Analysis
********************************************************************************

* Summary table
shell Rscript code/analysis/summary.R

* Replicates Boustan's Urban Wage gap figure in the Urbanization in the United States paper and extends it to include MA and then Controls
* Uses urban indicator
shell Rscript code/analysis/results-urban_indicator.R

* Using log urban_size
shell Rscript code/analysis/results-urban_size.R

* Replicates Boustan's Urban Wage gap figure in the Urbanization in the United States paper and extends it to include MA and then Controls
* do code/analysis/results-IPUMS.do

* A set of robustness checks on main result including a remove-own GDP MA variable
* do code/analysis/results-IPUMS-robust.do


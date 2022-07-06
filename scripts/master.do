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
	global project "/Users/kylebutts/Dropbox/UrbanWagePremium"
	global gh "/Users/kylebutts/Documents/Projects/urban-wage-premium"
}

cd $gh


********************************************************************************
* Data Cleaning
********************************************************************************

* Matlab script to create market access and remove-own market access
* run findMA.m

* Creates MSA crosswalk from 1940-2020
do scripts/data-metarea.do

* From IPUMS files, creates individual survey dataset 
do scripts/data-IPUMS-compile.do

* Creates market access dataset and housing prices in 1950
do scripts/data-marketaccess.do

* Merges MA with individual survey and creates new variables    
do scripts/data-merge-MA.do

* Creates group averages 
do scripts/data-group-averages.do

* Create shape file of MSAs matched with data
* R scripts/data-merge_ma_with_shape.R


********************************************************************************
* Analysis
********************************************************************************

* Summary table
do scripts/results-summary.do

* Replicates Boustan's Urban Wage gap figure in the Urbanization in the United States paper and extends it to include MA and then Controls
* Uses urban indicator
* R scripts/results-urban_indicator.R

* Using log urban_size
* R scripts/results-urban_size.R

* Replicates Boustan's Urban Wage gap figure in the Urbanization in the United States paper and extends it to include MA and then Controls
* do scripts/results-IPUMS.do

* A set of robustness checks on main result including a remove-own GDP MA variable
* do scripts/results-IPUMS-robust.do


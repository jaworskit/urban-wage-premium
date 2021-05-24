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


********************************************************************************
* Data Cleaning
********************************************************************************

* Creates MSA crosswalk from 1940-2015
do scripts/data-metarea.do

* From IPUMS files, creates individual survey dataset 
do scripts/data-IPUMS-compile.do

* Matlab script to create market access
* run findMA.m

* Creates market access dataset and housing prices in 1950
do scripts/data-marketaccess.do

* Merges MA with individual survey and creates new variables    
do scripts/data-data-merge-MA.do

* Creates group averages 
do scripts/data-group-averages.do


********************************************************************************
* Analysis
********************************************************************************

* Replicates Boustan's Urban Wage gap figure in the Urbanization in the United States paper and extends it to include MA and then Controls
do scripts/results-IPUMS.do

* A set of robustness checks on main result including a remove-own GDP MA variable
do scripts/results-IPUMS-robust.do

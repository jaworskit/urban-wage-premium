## Data Cleaning

- data-metarea.do
  - Creates MSA crosswalk from 1940-2015

- data-IPUMS-compile.do
  - From IPUMS files, creates individual survey dataset

- findMA.m
  - Creates market access variable for each year

- data-marketaccess.do
  - Creates market access dataset and housing prices in 1950

- data-merge-MA.do
  - Merges MA with individual survey dataset
  - Edits survey dataset to create new variables

- data-group-averages.do
  - Creates group averages 

## Analysis

- results-summary.do
  - Creates summary table of urban indicator, log(remove own MA), wage_urban, and wage_nonurban

- results-urban_indicator.R
  - Calculates raw urban wage premium observed in data.
  - Include covariates and group averages to see how selection drives the observed raw wage premium over time.

- results-heterogeneity.R
  - Calculates urban wage premium for top 20 cities (in terms of 1940 population) and other cities separately
  - Calculates urban wage premium for census regions (North, South, Midwest, and West) separately.
  


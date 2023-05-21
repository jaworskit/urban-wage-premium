## Data Cleaning

- `data-metarea.do`
  - Creates MSA crosswalk from 1940-2020

- `data-IPUMS-compile.do`
  - From IPUMS files, creates individual survey dataset

- `findMA.m`
  - Creates market access variable for each year

- `data-marketaccess.do`
  - Creates market access dataset

- `data-merge-MA.do`
  - Merges MA with individual survey dataset
  - Edits survey dataset to create new variables

- `data-group-averages.do`
  - Creates group averages 

- `data-split_by_year.do`
  - Splits dataset by year for quicker loading

- `data-convert_to_parquet.R`
  - Makes loading data quicker in R by converting to arrow parquet format

## Analysis

### Summary Statistics

- `results-summary.do`
  - Creates summary table of Year, Wage Gap (%), Urban Overall (%), % With College Degree (Overall, in Urban Areas, and in Nonurban Areas)

- `results-top_paying_cities.R`
  - Calculates the top-10 highest paying cities (raw) in 1940 and 2010

- `results-PCA_group_averages.R`
  - A simple script to calculate the PCA of the used group averages

### Regression Results

- `results-urban_premium.R`
  - Calculates raw urban wage premium observed in data as the average difference of log weekly wages
  - Include covariates and group averages to see how selection drives the observed raw wage premium over time.

- `results-rent_premium.R`
  - Calculates raw urban rent premium observed in data as the average difference of log monthly rent 

- `results-real_wage_premium.R`
  - Define "Real Wage" as the annual wages minus the annual average rent payment
  - Calculates raw urban real wage premium observed in data as the average difference of log real annual wages
  - Include covariates and group averages to see how selection drives the observed raw real wage premium over time.

- `results-heterogeneity.R`
  - Performs four heterogeneity results
  - 1. Estimates seperately for the top 20 largest populated cities (in 1940) and the other MSAs
  - 2. Estimates seperately for the four census regions (North, South, Midwest, and West)
  - 3. Estimates seperately for college and non-college workers
  - 4. Estimates using 1940 MSA definitions

- `results-distribution.R`
  - Plots the distribution over time of residualized log weekly wages after controlling for individual controls and group averages
  - Plots seperately for urban/non-urban workers



# The Urban Wage Premium in Historical Perspective

[Kyle Butts](https://www.kylebutts.com/)<sup>1</sup>, [Taylor Jaworski](https://jaworskit.github.io/)<sup>1</sup>, and [Carl Kitchens](https://sites.google.com/site/kitchct/)<sup>2</sup>
<br>
<sup>1</sup>University of Colorado: Boulder and <sup>2</sup>Florida State University

#### [Paper](https://github.com/jaworskit/urban-wage-premium/blob/master/paper/urban_wage.pdf) 


## Abstract

We estimate the urban wage premium in the United States from 1940 to 2010. 
Drawing on recent advances in the literature on selection on unobservables, 
we show how to control for heterogeneity in the characteristics of individuals 
that choose to live in cities to address endogenous sorting. Estimates from 
naive comparisons of individuals living in urban versus rural areas 
substantially overstate the urban wage premium. We find that the premium is 
highest in the middle of the twentieth century (about 12 percent in 1940 and 
1950) relative to the early in twenty-first century (declining to a few percent 
by mid-2020). Overall, the urban wage premium is decreasing and sorting 
explains a larger fraction of the difference in urban versus rural earnings 
across our sample period.

## Replication

### Data Cleaning

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

### Analysis 

#### Summary Statistics

- `results-summary.do`
  - Creates summary table of Year, Wage Gap (%), Urban Overall (%), % With College Degree (Overall, in Urban Areas, and in Nonurban Areas)

- `results-top_paying_cities.R`
  - Calculates the top-10 highest paying cities (raw) in 1940 and 2010

- `results-PCA_group_averages.R`
  - A simple script to calculate the PCA of the used group averages

#### Regression Results

- `results-urban_premium.R`
  - Calculates raw urban wage premium observed in data as the average difference of log weekly wages
  - Include covariates and group averages to see how selection drives the observed raw wage premium over time.

- `results-rent_premium.R`
  - Calculates raw urban rent premium observed in data as the average difference of log monthly rent 

- `results-net_of_housing_premium.R`
  - Define "Real Wage" as the annual wages minus the annual average rent payment
  - Calculates raw urban real wage premium observed in data as the average difference of log real annual wages
  - Include covariates and group averages to see how selection drives the observed raw real wage premium over time.

- `results-heterogeneity.R`
  - Performs four heterogeneity results
  - 1. Estimates seperately for the top 20 largest populated cities (in 1940) and the other MSAs
  - 2. Estimates seperately for the four census regions (North, South, Midwest, and West)
  - 3. Estimates seperately for college and non-college workers
  
- `results-1940_msas.R`
  - Robustness check dropping workers in MSAs that were created after 1940

- `results-distribution.R`
  - Plots the distribution over time of residualized log weekly wages after controlling for individual controls and group averages
  - Plots seperately for urban/non-urban workers



## Citation

```
@article{butts2023urban,
  title={The Urban Wage Premium in Historical Perspective},
  author={Butts, Kyle and Jaworski, Taylor and Kitchens, Carl},
  journal={Working Paper},
  year={2023}
}
```



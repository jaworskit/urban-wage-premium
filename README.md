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

- `code/data/metarea.do`
  - Creates MSA crosswalk from 1940-2020

- `code/data/IPUMS-compile.do`
  - From IPUMS files, creates individual survey dataset

- `code/solve-ma/FindMA.m`
  - Creates market access variable for each year

- `code/data/marketaccess.do`
  - Creates market access dataset

- `code/data/merge.do`
  - Merges MA with individual survey dataset
  - Edits survey dataset to create new variables

- `code/data/convert_to_parquet.R`
  - Makes loading data quicker in R by converting to arrow parquet format

### Analysis 

#### Summary Statistics

- `code/analysis/summary.R`
  - Creates summary table of Year, Wage Gap (%), Urban Overall (%), % With College Degree (Overall, in Urban Areas, and in Nonurban Areas)

- `code/analysis/top_paying_cities.R`
  - Calculates the top-10 highest paying cities (raw) in 1940 and 2010

- `code/analysis/PCA_group_averages.R`
  - A simple script to calculate the PCA of the used group averages

#### Regression Results

- `code/analysis/urban_premium.R`
  - Calculates raw urban wage premium observed in data as the average difference of log weekly wages
  - Include covariates and group averages to see how selection drives the observed raw wage premium over time.

- `code/analysis/housing_costs.R`
  - Calculates raw urban rent premium observed in data as the average difference of log monthly rent 
  - Define "Real Wage" as wages minus housing costs (Ganong and Shoag, 2017)
  - Calculates raw urban real wage premium observed in data as the average difference of log real annual wages
  - Include covariates and group averages to estimate the causal urban (net) wage premium over time.

- `code/analysis/heterogeneity.R`
  - Heterogeneity results
  - 1. Estimates separately for the top 20 largest populated cities (in 1940) and the other MSAs
  - 2. Estimates separately for the four census regions (North, South, Midwest, and West)
  - 3. Estimates separately for college and non-college workers
  - 4. Estimates separately for White and Black workers
  - 5. Estimates separately for older and younger workers to assess the possibility of urban wage-growth premium
  
- `code/analysis/1940_msas.R`
  - Robustness check dropping workers in MSAs that were created after 1940

- `code/analysis/distribution.R`
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



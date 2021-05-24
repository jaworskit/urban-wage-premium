Data Creation Order:


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


Analysis Order:

- results-IPUMS.do
  - Replicates Boustan's Urban Wage gap figure in the Urbanization in the United States paper
  - Extends it to include MA and then Controls 

- results-IPUMS-robust.do
  - A set of robustness checks on main result
  - Including a remove-own MA 


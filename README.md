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
  - Merges MA with individual survye dataset and computes group averages 




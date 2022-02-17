## results-city_specific_premium.R ---------------------------------------------
## Kyle Butts, CU Boulder Economics 
## 
## This file estimates individual city premiums for 1940, 1980, & 2015 
## and creates a rank chart

library(tidyverse)
library(glue)
library(broom)
library(vroom)
library(fixest)
library(collapse)
# Shrinkage Estimator
library(FEShR) 

# Local
project <- "/Users/kylebutts/Dropbox/UrbanWagePremium"
gh <- "/Users/kylebutts/Documents/Projects/urban-wage-premium"

# data <- data.table::fread(glue("{project}/data/dta/urban_wage_final_sample.csv"))

# Research Computing
# project <- "/projects/kybu6659/urban-wage-premium"
# data <- vroom(glue("{project}/data/urban_wage_final.csv"))

# Results ----------------------------------------------------------------------

results <- NULL

# Loop through year
for (y in c(1940, 1950, 1960, 1970, 1980, 1990, 2000, 2010)) {
    cli::cli_alert_info("Starting on year {y}")
    
    # Sample has every observation except 15% sample of 1940 full count
    data <- data.table::fread(glue("{project}/data/dta/urban_wage_final_{y}.csv"))
    
    ## Urban only --------------------------------------------------------------
    
    fmla <- as.formula(glue('ln_weeklywage ~ i(metarea, ref = "Not identifiable or not in an MSA")'))
    
    est <- feols(fmla,
                 data = data, cluster = ~metarea, weights = ~perwt, lean = TRUE
    )
    
    results <- bind_rows(
        results, 
        tibble(code = names(coef(est)), premium = coef(est), year = y)
    )
}

# MSA Names
names <- haven::read_dta(
    glue::glue("{project}/data/urbanareas/metarea_names.dta")
) |>
    mutate(code = as.character(code))

city_premia <- results |>
    filter(stringr::str_starts(code, "metarea")) |> 
    group_by(code) |>
    mutate(num_years_in_sample = n()) |>
    ungroup() |> 
    # Join with names
    mutate(
        code = str_extract(code, "(?<=::)(.*)$")
    ) |> 
    left_join(names, by = "code") |> 
    mutate(
        # Replace NAs with name
        metarea = if_else(is.na(metarea), code, metarea)
    ) 

setDT(city_premia)

city_premia[
    num_years_in_sample == 8,
    rank_balanced := frank(premium, ties.method="min"),
    by = year
][,
  rank_unbalanced := frank(premium, ties.method="min"),
  by = year
]

save(city_premia, file = glue::glue("{gh}/data/estimates-city_premia.RData"))

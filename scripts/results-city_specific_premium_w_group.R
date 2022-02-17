## results-city_specific_premium.R ---------------------------------------------
## Kyle Butts, CU Boulder Economics 
## 
## This file estimates individual city premiums for 1940, 1980, & 2015 
## and creates a rank chart

library(tidyverse)
library(data.table)
library(glue)
library(broom)
library(vroom)
library(fixest)
library(collapse)

# Local
project <- "/Users/kylebutts/Dropbox/UrbanWagePremium"
gh <- "/Users/kylebutts/Documents/Projects/urban-wage-premium"

# Results ----------------------------------------------------------------------

results <- NULL

# Loop through year
for (y in c(1940, 1950, 1960, 1970, 1980, 1990, 2000, 2010)) {
    cli::cli_alert_info("Starting on year {y}")
    
    # Sample has every observation except 15% sample of 1940 full count
    data <- data.table::fread(glue("{project}/data/dta/urban_wage_final_{y}.csv"))
    
    ## Urban, Individual Controls, Market Access, & Group Averages -------------
    
    # Group_vars formula
    group_vars <- c("ln_ma_removeown", "share_race0", "share_race1", "share_age5", "share_age6", "share_age7", "share_age8", "share_age9", "share_age10", "share_age11", "share_age12", "share_age13", "share_educ0", "share_educ1", "share_educ2", "share_educ3", "share_educ4", "share_educ5", "share_educ6", "share_educ7", "share_educ8", "share_educ9", "share_educ10", "share_educ11", "share_educ99", "share_marst1", "share_marst2", "share_marst3", "share_marst4", "share_marst5", "share_marst6", "share_vetstat0", "share_vetstat1", "share_vetstat2", "share_vetstat9")
    
    group_formula <- paste(group_vars, collapse = " + ")
    
    fmla <- as.formula(glue('ln_weeklywage ~ i(metarea, ref = "Not identifiable or not in an MSA") + {group_formula} | agegroup + educ + white'))
    
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

save(city_premia, file = glue::glue("{gh}/data/estimates-city_premia_w_group.RData"))


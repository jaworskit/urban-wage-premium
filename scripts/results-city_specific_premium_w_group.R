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
for (y in c(1940, 1970, 2000)) {
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

# Only MSAs present throughout
city_premia <- results |>
    filter(stringr::str_starts(code, "metarea")) |> 
    group_by(code) |>
    # Keep only present in 1940, 1970, and 2000
    filter(n() == 3) |>
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

# save(city_premia, file = glue::glue("{gh}/data/estimates-city_premia.RData"))

# ---- Shrinkage Estimator -----------------------------------------------------

load(glue::glue("{gh}/data/estimates-city_premia.RData"))


# premia_wide <- pivot_wider(
#     city_premia |> select(-rank),
#     names_from = "year",
#     names_prefix = "premia_",
#     values_from = "premium",
# )
# 
# FE_mat <- as.matrix(premia_wide[, c("premia_1940", "premia_1970", "premia_2000")])
# 
# @TODO: Figure out M
# FEShR::fe_shrink(FE_mat)

# ---- Visualize Results -------------------------------------------------------

ggplot(city_premia, 
       aes(x = year, y = rank, color = metarea)
) +
    geom_point(size = 7) +
    # geom_text(data = city_premia |> filter(year == max(year)),
    #     aes(x = year + 2, label = metarea), size = 5, hjust = 0) +
    ggbump::geom_bump(size = 2, smooth = 8) + 
    scale_x_continuous(limits = c(1935, 2020),
                       breaks = c(1940, 1970, 2000)) +
    cowplot::theme_minimal_grid(font_size = 14, line_size = 0) +
    theme(legend.position = "none",
          panel.grid.major = element_blank()) +
    labs(y = "Rank", x = NULL) 


rank_1940 <- city_premia[city_premia$year == 1940, ][["rank"]]
rank_2000 <- city_premia[city_premia$year == 2000, ][["rank"]]
cor.test(rank_1940, rank_2000, method = "spearman")

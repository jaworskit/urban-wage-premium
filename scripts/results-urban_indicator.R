## results-IPUMS.R -------------------------------------------------------------
## Kyle Butts, CU Boulder Economics
##
## This file replicates Boustan's Urban Wage gap figure in the Urbanization in the United States paper. Then it extends this to include individual-level controls, market access controls, and MSA-averages.
##

library(tidyverse)
library(glue)
library(broom)
library(vroom)
library(fixest)
library(collapse)

# Local
project <- "/Users/kylebutts/Dropbox/UrbanWagePremium"
gh <- "/Users/kylebutts/Documents/Projects/urban-wage-premium"


# Research Computing
# project <- "/projects/kybu6659/urban-wage-premium"
# data <- vroom(glue("{project}/data/urban_wage_final.csv"))

data <- data.table::fread(glue("{project}/data/dta/urban_wage_final_sample.csv"))

# Results ----------------------------------------------------------------------

## Boustan et. al --------------------------------------------------------------

df0 <- data %>%
    collapse::collap(totalincome + perwt ~ year + urban, fsum) %>%
    mutate(weeklywage = totalincome / perwt) %>%
    pivot_wider(id_cols = c("year"), values_from = weeklywage, names_from = urban, names_prefix = "urban_") %>%
    mutate(est = log(urban_1 / urban_0), group = "Raw") %>%
    select(year, est, group)

# Store results
# results <- df0
results <- NULL


## Regression Results ----------------------------------------------------------

# Loop through year
# Removed 2005 and 2015 for data quality issue in 2005
for (i in c(1940, 1950, 1960, 1970, 1980, 1990, 2000, 2010)) {
    cli::cli_alert_info("Starting on year {i}")

    # Sample has every observation except 15% sample of 1940 full count
    rm(data)
    data <- data.table::fread(glue("{project}/data/dta/urban_wage_final_sample.csv"))
    data <- data[year == i, ]

    ## Urban -------------------------------------------------------------------

    est1 <- feols(ln_weeklywage ~ urban,
        data = data, cluster = ~metarea, weights = ~perwt, lean = TRUE
    )

    ## Urban & Individual Controls ---------------------------------------------

    est2 <- feols(ln_weeklywage ~ urban | agegroup + educ + white,
        data = data, cluster = ~metarea, weights = ~perwt, lean = TRUE
    )
    ## Urban, Individual Controls, Market Access, & Group Averages -------------

    # Group_vars formula
    group_vars <- c("ln_ma_removeown", "share_race0", "share_race1", "share_age5", "share_age6", "share_age7", "share_age8", "share_age9", "share_age10", "share_age11", "share_age12", "share_age13", "share_educ0", "share_educ1", "share_educ2", "share_educ3", "share_educ4", "share_educ5", "share_educ6", "share_educ7", "share_educ8", "share_educ9", "share_educ10", "share_educ11", "share_educ99", "share_marst1", "share_marst2", "share_marst3", "share_marst4", "share_marst5", "share_marst6", "share_vetstat0", "share_vetstat1", "share_vetstat2", "share_vetstat9")

    group_formula <- paste(group_vars, collapse = " + ")

    fmla <- as.formula(glue("ln_weeklywage ~ urban + {group_formula} | agegroup + educ + white"))

    est3 <- feols(fmla,
        data = data, cluster = ~metarea, weights = ~perwt, lean = TRUE
    )


    results <- bind_rows(results, tibble(
        year = rep(i, times = 3),
        est = unlist(lapply(list(est1, est2, est3), function(x) {
            coef(x)[["urban"]]
        })),
        group = c("Urban Only", "Controls", "Group Averages")
    ))
}


# Export Results ---------------------------------------------------------------

save(results, file = glue("{gh}/data/estimates-urban_indicator.RData"))


# Plot Point Estimates ---------------------------------------------------------

load(file = glue("{gh}/data/estimates-urban_indicator.RData"))

# theme_kyle
# devtools::install_github("kylebutts/kfbmisc")
library(kfbmisc)

# exponentiate log differences
results <- results %>%
    mutate(
        exp_est = exp(est) - 1,
        group = factor(group, levels = c("Urban Only", "Controls", "Group Averages"))
    )

## Boustan et. al --------------------------------------------------------------

boustan <- ggplot(results %>% filter(group == "Raw"), aes(x = year, y = exp_est, color = group)) +
    geom_line(aes(x = year, y = exp_est), color = "grey10", size = 2) +
    geom_point(aes(x = year, y = exp_est), color = "grey10", shape = 15, size = 5) +
    labs(x = "Year", y = "Urban Wage Premium") +
    scale_y_continuous(labels = scales::percent, limits = c(0, NA)) +
    theme_kyle(base_size = 24) +
    guides(colour = guide_legend(title.position = "top", nrow = 2)) +
    theme(legend.position = "bottom")

# kfbmisc::ggpreview(boustan, dpi = 300, width = 4800/300, height = 2400/300, cairo = FALSE)

ggsave(glue("{gh}/paper/figures/boustan.pdf"), boustan, width = 4800 / 300, height = 2400 / 300)


## Raw -------------------------------------------------------------------------


(urban <- ggplot(
        results[results$group == "Urban Only", ], 
        aes(x = year, y = exp_est)
    ) +
    geom_line(size = 2, linetype = 1) +
    geom_point(size = 5, shape = 15) +
    labs(
        x = "Year", y = "Urban Wage Premium"
    ) +
    scale_y_continuous(labels = scales::percent, limits = c(-0.05, 0.375)) + 
    theme_kyle(base_size = 24) +
    guides(colour = guide_legend(title.position = "top", nrow = 2)) +
    theme(legend.position = "bottom"))

# kfbmisc::ggpreview(controls, dpi = 300, width = 4800/300, height = 3000/300, cairo = FALSE, device ="pdf")

ggsave(glue("{gh}/paper/figures/urbanpremium_urban.pdf"), controls, width = 16, height = 10)


## Regression Results ----------------------------------------------------------

(controls <- ggplot(
        results[results$group != "Raw", ], 
        aes(x = year, y = exp_est, group = group, color = group)
    ) +
    geom_line(aes(linetype = group), size = 2) +
    geom_point(aes(shape = group), size = 5) +
    labs(
        x = "Year", y = "Urban Wage Premium", group = "Specification", 
        shape = "Specification", color = "Specification", 
        linetype = "Specification"
    ) +
    scale_y_continuous(labels = scales::percent, limits = c(-0.05, 0.375)) + 
    # ggsci::scale_color_jama() +
    scale_color_manual(values = c(
        # "Raw" = "grey40",
        "Urban Only" = "grey10",
        "Controls" = "grey10",
        "Market Access" = "grey10",
        "Group Averages" = "grey10"
        # "Rent" = "grey10"
    )) +
    scale_linetype_manual(values = c(
        # "Raw" = 2,
        "Urban Only" = 1,
        "Controls" = 1,
        "Market Access" = 1,
        "Group Averages" = 1
        # "Rent" = 1
    )) +
    scale_shape_manual(values = c(
        # "Raw" = 15,
        "Urban Only" = 15,
        "Controls" = 16,
        "Market Access" = 17,
        "Group Averages" = 18
        # "Rent" = 4
    )) +
    theme_kyle(base_size = 24) +
    guides(colour = guide_legend(title.position = "top", nrow = 2)) +
    theme(legend.position = "bottom"))

# kfbmisc::ggpreview(controls, dpi = 300, width = 4800/300, height = 3000/300, cairo = FALSE, device ="pdf")

ggsave(glue("{gh}/paper/figures/urbanpremium_controls.pdf"), controls, width = 16, height = 10)

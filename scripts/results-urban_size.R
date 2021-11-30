## results-urban_size.R -------------------------------------------------------------
## Kyle Butts, CU Boulder Economics
##
## This file replicates Boustan's Urban Wage gap figure in the Urbanization in the United States paper, but uses log(population) instead of an urban indicator. Then it extends this to include individual-level controls, market access controls, and MSA-averages.
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

# data_index <- vroom(glue("{project}/data/dta/urban_wage_final.csv"))data <- filter(data_index, year == 1940)

# Research Computing

# project <- "/projects/kybu6659/urban-wage-premium"
# data <- vroom(glue("{project}/data/urban_wage_final.csv"))


# Results ----------------------------------------------------------------------

# Store results
results <- NULL

# Loop through year
for (i in c(1940, 1950, 1960, 1970, 1980, 1990, 2000, 2005, 2010, 2015)) {
    
    cli::cli_alert_info("Starting on year {i}")
    
    # Sample has every observation except 15% sample of 1940 full count
    data <- data.table::fread(glue("{project}/data/dta/urban_wage_final_sample.csv"))
    data <- data[year == i]
    
    ## Urban -------------------------------------------------------------------
    
    est1 <- feols(ln_weeklywage ~ ln_msasize,
                  data = data, cluster = ~metarea, weights = ~perwt, lean = TRUE
    )
    
    ## Urban & Individual Controls ---------------------------------------------
    
    est2 <- feols(ln_weeklywage ~ ln_msasize | agegroup + educ + white,
                  data = data, cluster = ~metarea, weights = ~perwt, lean = TRUE
    )
    
    ## Urban, Individual Controls, & Market Access -----------------------------
    
    est3 <- feols(ln_weeklywage ~ ln_msasize + ln_ma_removeown | agegroup + educ + white,
                  data = data, cluster = ~metarea, weights = ~perwt, lean = TRUE
    )
    
    ## Urban, Individual Controls, Market Access, & Group Averages -------------
    
    # Group_vars formula
    group_vars <- c("share_race0", "share_race1", "share_age5", "share_age6", "share_age7", "share_age8", "share_age9", "share_age10", "share_age11", "share_age12", "share_age13", "share_educ0", "share_educ1", "share_educ2", "share_educ3", "share_educ4", "share_educ5", "share_educ6", "share_educ7", "share_educ8", "share_educ9", "share_educ10", "share_educ11", "share_educ99", "share_marst1", "share_marst2", "share_marst3", "share_marst4", "share_marst5", "share_marst6", "share_vetstat0", "share_vetstat1", "share_vetstat2", "share_vetstat9")
    
    group_formula <- paste(group_vars, collapse = " + ")
    
    fmla <- as.formula(glue("ln_weeklywage ~ ln_msasize + ln_ma_removeown + {group_formula} | agegroup + educ + white"))
    
    est4 <- feols(fmla,
                  data = data, cluster = ~metarea, weights = ~perwt, lean = TRUE
    )
    
    
    results <- bind_rows(results, tibble(
        year = rep(i, times = 4),
        est = unlist(lapply(list(est1, est2, est3, est4), function(x) {
            coef(x)[["ln_msasize"]]
        })),
        group = c("Urban Only", "Controls", "Market Access", "Group Averages")
    ))
}


# Export Results ---------------------------------------------------------------

save(results, file = glue("{gh}/data/estimates-urban_size.RData"))
# load(file = glue("{gh}/data/estimates-urban_size.RData"))

# save(results, file = glue("{project}/data/results-urban_size.RData"))


# Plot Point Estimates ---------------------------------------------------------

# theme_kyle
# devtools::install_github("kylebutts/kfbmisc")
library(kfbmisc)


## Regression Results ----------------------------------------------------------

(controls <- ggplot(results, aes(x = year, y = est / 100, group = group, linetype = group, shape = group, color = group)) +
     geom_line(size = 2) +
     geom_point(size = 5) +
     labs(x = "Year", y = "Coefficient on Log(MSA Population)", group = "Specification", shape = "Specification", color = "Specification", linetype = "Specification") +
     scale_y_continuous(labels = function(x) scales::percent(x, accuracy = 0.01)) +
     # ggsci::scale_color_jama() +
     scale_color_manual(values = c(
         "Urban Only" = "grey10",
         "Controls" = "grey10",
         "Market Access" = "grey10",
         "Group Averages" = "grey10"
     )) +
     scale_linetype_manual(values = c(
         "Urban Only" = 1,
         "Controls" = 1,
         "Market Access" = 1,
         "Group Averages" = 1
     )) +
     scale_shape_manual(values = c(
         "Urban Only" = 15,
         "Controls" = 16,
         "Market Access" = 17,
         "Group Averages" = 18
     )) +
     theme_kyle(base_size = 24) +
     guides(colour = guide_legend(title.position = "top", nrow = 2)) +
     theme(legend.position = "bottom"))

# kfbmisc::ggpreview(controls, dpi = 300, width = 4800/300, height = 3000/300, cairo = FALSE)

ggsave(glue("{gh}/paper/figures/urbanpremium_logpop.pdf"), controls, width = 4800/300, height = 3000/300)

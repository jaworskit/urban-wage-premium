## results-heterogeneity.R -----------------------------------------------------
## Kyle Butts, CU Boulder Economics 
## 
## This file estimates the premium seperately for large and small urban areas

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


# Data from https://www2.census.gov/library/publications/decennial/1950/pc-03/pc-3-03.pdf
# Calculate above and below- median population cities
pop1940 <- glue(
    "{project}/data/urbanareas/metarea_population_1940_1950.csv"
  ) |> 
  fread()

pop1940 <- pop1940[, pop_1940 := as.integer(pop_1940)]
pop1940 <- pop1940[!is.na(pop_1940),]

pop1940[, pop_1940_rank := .N + 1 - frank(pop_1940)]
pop1940[, top20 := (pop_1940_rank <= 20)]
pop1940 <- pop1940[order(pop_1940_rank), ]
pop1940[, state := NULL]

# Results ----------------------------------------------------------------------

results <- NULL
results_top20 <- NULL
results_region <- NULL

# Loop through year
for (y in c(1940, 1950, 1960, 1970, 1980, 1990, 2000, 2010)) {
    cli::cli_alert_info("Starting on year {y}")
    
    # Sample has every observation except 15% sample of 1940 full count
    data <- glue("{project}/data/dta/urban_wage_final_{y}.dta") |> 
      haven::read_dta() |> 
      data.table::setDT()
    
    # Merge with 1940 above/below median population
    data <- merge(data, pop1940, by="metarea", all.x = T)
    data <- data[!is.na(pop_1940) | metarea == 0, ]
    data[, urban_top20 := fcase(
        top20, "Top 20",
        !top20, "Urban",
        default = "Non-urban"
    )]
    data[, region := fcase(
        region == "North", "North",
        region == "South", "South",
        region == "Midwest", "Midwest",
        region == "West", "West",
        default = "Non-urban"
    )]
    
    cli::cli_alert_warning("Year {y} has {nrow(data)} observations")    
    
    ## Urban, Individual Controls, Market Access, & Group Averages -------------
    
    # Group_vars formula
    group_vars <- c("ln_ma_removeown", "share_race0", "share_race1", "share_age5", "share_age6", "share_age7", "share_age8", "share_age9", "share_age10", "share_age11", "share_age12", "share_age13", "share_educ0", "share_educ1", "share_educ2", "share_educ3", "share_educ4", "share_educ5", "share_educ6", "share_educ7", "share_educ8", "share_educ9", "share_educ10", "share_educ11", "share_educ99", "share_marst1", "share_marst2", "share_marst3", "share_marst4", "share_marst5", "share_marst6", "share_vetstat0", "share_vetstat1", "share_vetstat2", "share_vetstat9")
    
    group_formula <- paste(group_vars, collapse = " + ")
    
    
    ## Urban on subsample ------------------------------------------------------
    
    fmla <- as.formula(glue('ln_weeklywage ~ i(urban) + {group_formula} | agegroup + educ + white'))
    
    est <- feols(fmla,
                 data = data, cluster = ~metarea, weights = ~perwt, lean = TRUE
    )
    
    
    results <- bind_rows(
        results, 
        tibble(code = names(coef(est)), premium = coef(est), year = y)
    )
    
    ## Top 20 Populous Urban Areas ---------------------------------------------
    
    fmla <- as.formula(glue('ln_weeklywage ~ i(urban_top20, ref="Non-urban") + {group_formula} | agegroup + educ + white'))
    
    est <- feols(fmla,
                 data = data, cluster = ~metarea, weights = ~perwt, lean = TRUE
    )
    
    
    results_top20 <- bind_rows(
        results_top20, 
        tibble(code = names(coef(est)), premium = coef(est), year = y)
    )
    
    ## Regions of US -----------------------------------------------------------
    
    fmla <- as.formula(glue('ln_weeklywage ~ i(region, ref="Non-urban") + {group_formula} | agegroup + educ + white'))
    
    est <- feols(fmla,
                 data = data, cluster = ~metarea, weights = ~perwt, lean = TRUE
    )
    
    
    results_region <- bind_rows(
        results_region, 
        tibble(code = names(coef(est)), premium = coef(est), year = y)
    )
}


results <- results |>
    filter(stringr::str_starts(code, "urban")) |> 
    mutate(
        group = "Urban",
        group = factor(group, levels = "Urban"),
        code = NULL,
        exp_est = exp(premium) - 1
    )

results_top20 <- results_top20 |>
    filter(stringr::str_starts(code, "urban_top20")) |> 
    mutate(
        group = stringr::str_remove(code, "urban_top20::"),
        group = factor(group, levels = c("Top 20", "Urban")),
        code = NULL,
        exp_est = exp(premium) - 1
    ) 

results_region <- results_region |>
    filter(stringr::str_starts(code, "region")) |> 
    mutate(
        group = stringr::str_remove(code, "region::"),
        group = factor(group, levels = c("North", "West", "South", "Midwest")),
        code = NULL,
        exp_est = exp(premium) - 1
    ) 



(urban_1940_msas <- ggplot(results, aes(x = year, y = exp_est, group = group, color = group)) +
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
        "Urban" = "grey10"
    )) +
    scale_linetype_manual(values = c(
        "Urban" = 1
    )) +
    scale_shape_manual(values = c(
        "Urban" = 16
    )) +
    kfbmisc::theme_kyle(base_size = 24) +
    guides(colour = guide_legend(title.position = "top", nrow = 2)) +
    theme(legend.position = "bottom"))


(top20 <- ggplot(results_top20, aes(x = year, y = exp_est, group = group, color = group)) +
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
        "Top 20" = "grey10",
        "Urban" = "grey10"
    )) +
    scale_linetype_manual(values = c(
        "Top 20" = 1,
        "Urban" = 1
    )) +
    scale_shape_manual(values = c(
        "Top 20" = 15,
        "Urban" = 16
    )) +
    kfbmisc::theme_kyle(base_size = 24) +
    guides(colour = guide_legend(title.position = "top", nrow = 2)) +
    theme(legend.position = "bottom"))


(region <- ggplot(results_region, aes(x = year, y = exp_est, group = group, color = group)) +
    geom_line(aes(linetype = group), size = 2) +
    geom_point(aes(shape = group), size = 5) +
    labs(
        x = "Year", y = "Urban Wage Premium", group = "Specification", 
        shape = "Specification", color = "Specification", 
        linetype = "Specification"
    ) +
    scale_y_continuous(labels = scales::percent, limits = c(-0.05, 0.375)) + 
    scale_color_manual(values = c(
        "North" = ggsci::pal_jama("default")(4)[1],
        "South" = ggsci::pal_jama("default")(4)[2],
        "West" = ggsci::pal_jama("default")(4)[3],
        "Midwest" = ggsci::pal_jama("default")(4)[4]
    )) +
    scale_linetype_manual(values = c(
        "North" = 1,
        "South" = 1,
        "West" = 1,
        "Midwest" = 1
    )) +
    scale_shape_manual(values = c(
        "North" = 15,
        "South" = 16,
        "West" = 17,
        "Midwest" = 18
    )) +
    kfbmisc::theme_kyle(base_size = 24) +
    guides(colour = guide_legend(title.position = "top", nrow = 2)) +
    theme(legend.position = "bottom"))



ggsave(glue("{gh}/paper/figures/urbanpremium_1940_msas.pdf"), urban_1940_msas, width = 16, height = 10)
ggsave(glue("{gh}/paper/figures/urbanpremium_top20.pdf"), top20, width = 16, height = 10)
ggsave(glue("{gh}/paper/figures/urbanpremium_region.pdf"), region, width = 16, height = 10)




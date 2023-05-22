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
library(arrow)
# devtools::install_github("kylebutts/kfbmisc")
library(kfbmisc)

# Local
project <- "/Users/kylebutts/Dropbox/UrbanWagePremium"
gh <- "/Users/kylebutts/Documents/Projects/urban-wage-premium"


# Data from https://www2.census.gov/library/publications/decennial/1950/pc-03/pc-3-03.pdf
# Calculate above and below- median population cities
pop1940 <- glue(
  "{project}/data/urbanareas/metarea_population_1940_1950.csv"
) |>
  fread()

cbsa2020_to_msa1950 <- glue(
  "{project}/data/urbanareas/cbsa2015_to_msa1950.csv"
) |>
  fread()

pop1940[, pop_1940_rank := .N + 1 - frank(pop_1940)]
pop1940[, top20 := (pop_1940_rank <= 20)]
pop1940[, metarea_name := NULL]
pop1940 <- pop1940[order(pop_1940_rank), ]

pop1940_2020 <- merge(pop1940, cbsa2020_to_msa1950, by = "metarea")
pop1940_2020[, metarea := NULL]

regions <- tibble::tribble(
  ~region, ~state, ~statefip,
  "North", "Connecticut", 09,
  "North", "Maine", 23,
  "North", "Massachusetts", 25,
  "North", "New Hampshire", 33,
  "North", "Rhode Island", 44,
  "North", "Vermont", 50,
  "North", "New Jersey", 34,
  "North", "New York", 36,
  "North", "Pennsylvania", 42,
  "Midwest", "Indiana", 18,
  "Midwest", "Illinois", 17,
  "Midwest", "Michigan", 26,
  "Midwest", "Ohio", 39,
  "Midwest", "Wisconsin", 55,
  "South", "Iowa", 19,
  "South", "Kansas", 20,
  "South", "Minnesota", 27,
  "South", "Missouri", 29,
  "South", "Nebraska", 31,
  "South", "North Dakota", 38,
  "South", "South Dakota", 46,
  "South", "Delaware", 10,
  "South", "District of Columbia", 11,
  "South", "Florida", 12,
  "South", "Georgia", 13,
  "South", "Maryland", 24,
  "South", "North Carolina", 37,
  "South", "South Carolina", 45,
  "South", "Virginia", 51,
  "South", "West Virginia", 54,
  "South", "Alabama", 01,
  "South", "Kentucky", 21,
  "South", "Mississippi", 28,
  "South", "Tennessee", 47,
  "South", "Arkansas", 05,
  "South", "Louisiana", 22,
  "South", "Oklahoma", 40,
  "South", "Texas", 48,
  "West", "Arizona", 04,
  "West", "Colorado", 08,
  "West", "Idaho", 16,
  "West", "New Mexico", 35,
  "West", "Montana", 30,
  "West", "Utah", 49,
  "West", "Nevada", 32,
  "West", "Wyoming", 56,
  "West", "Alaska", 02,
  "West", "California", 06,
  "West", "Hawaii", 15,
  "West", "Oregon", 41,
  "West", "Washington", 53
)


# Results ----------------------------------------------------------------------

results_1940_msas <- NULL
results_top20 <- NULL
results_region <- NULL
results_college <- NULL

setFixest_fml(
  ..group_averages = ~
    # Share white 
    share_race0 + # share_race1 +
    # Share age groups 
    share_age5 + share_age6 + share_age7 + share_age8 + 
    share_age9 + share_age10 + share_age11 + share_age12 + # share_age13 + 
    # Share veteran
    share_vetstat1 + # share_vetstat2 +
    share_marst1 + share_marst6 + 
    share_marst2 + 
    # < HS
    I(share_educ1 + share_educ2 + share_educ3 + share_educ4 + share_educ5) + 
    # HS
    share_educ6 +
    # Some College
    I(share_educ7 + share_educ8 + share_educ9) +  
    # BA and >BA 
    share_educ10 + share_educ11,
  ..by_college_group_averages = ~
    # Share white 
    by_college_share_race0 + # by_college_share_race1 +
    # Share age groups 
    by_college_share_age5 + by_college_share_age6 + by_college_share_age7 + by_college_share_age8 + 
    by_college_share_age9 + by_college_share_age10 + by_college_share_age11 + by_college_share_age12 + # by_college_share_age13 + 
    # Share veteran
    by_college_share_vetstat1 + # by_college_share_vetstat2 +
    by_college_share_marst1 + by_college_share_marst6 + 
    by_college_share_marst2 + 
    # < HS
    I(by_college_share_educ1 + by_college_share_educ2 + by_college_share_educ3 + by_college_share_educ4 + by_college_share_educ5) + 
    # HS
    by_college_share_educ6 +
    # Some College
    I(by_college_share_educ7 + by_college_share_educ8 + by_college_share_educ9) +  
    # BA and >BA 
    by_college_share_educ10 + by_college_share_educ11
)


# Loop through year
for (y in c(1940, 1950, 1960, 1970, 1980, 1990, 2000, 2010)) {

  # Sample has every observation except 15% sample of 1940 full count
  cli::cli_alert_info("Starting on year {y}")
  
  data <- glue("{project}/data/dta/urban_wage_final_{y}.parquet") |>
    arrow::read_parquet() |>
    collect()

  data <- as.data.table(data)

  # Merge with 1940 above/below median population
  if (y == 2020) {
    data[, metarea := code]
    data <- dplyr::left_join(
      data, pop1940_2020,
      by = c("metarea" = "cbsa")
    )
  } else {
    data <- dplyr::left_join(data, pop1940, by = "metarea")
  }

  data <- as.data.table(data)

  # data <- data[!is.na(pop_1940) | metarea == 0, ]
  data[, urban_top20 := fcase(
    top20, "Top 20",
    !top20, "Other Urban",
    default = "Non-urban"
  )]

  # get regions
  data <- dplyr::left_join(data, regions, by = "statefip")
  cli::cli_alert_warning("Year {y} has {nrow(data)} observations")


  ## Urban on 1940 MSA subsample -----------------------------------------------

  est_1940_msas <- feols(
    ln_weeklywage ~ 
      i(urban) + ln_ma_removeown + ..group_averages | 
      educ + white + agegroup,
    data = data[!is.na(pop_1940) | metarea == 0, ], 
    cluster = ~metarea, weights = ~perwt, lean = TRUE
  )

  coef_1940_msas <- est_1940_msas |> coef()
  se_1940_msas <- est_1940_msas |> se()

  results_1940_msas <- bind_rows(
    results_1940_msas,
    tibble(
      code = coef_1940_msas |> names(), 
      est = coef_1940_msas, 
      se = se_1940_msas,  
      year = y
    )
  )

  ## Top 20 Populous Urban Areas -----------------------------------------------

  est_top20 <- feols(
    ln_weeklywage ~ 
      i(urban_top20, ref = "Non-urban") + ln_ma_removeown + 
      ..group_averages |
      educ + white + agegroup,
    data = data, 
    cluster = ~metarea, weights = ~perwt, lean = TRUE
  )

  results_top20 <- bind_rows(
    results_top20,
    tibble(
      code = est_top20 |> coef() |> names(), 
      est = est_top20 |> coef(), 
      year = y
    )
  )

  ## Regions of US -------------------------------------------------------------

  est_region <- feols(
    ln_weeklywage ~ 
      i(urban, i.region, ref = "0") + ln_ma_removeown +
      ..group_averages |
      educ + white + agegroup,
    data = data, cluster = ~metarea, weights = ~perwt, lean = TRUE
  )

  results_region <- bind_rows(
    results_region,
    tibble(
      code = est_region |> coef() |> names(), 
      est = est_region |> coef(),
      year = y
    )
  )

  ## College vs. No College ----------------------------------------------------

  est_college <- feols(
    ln_weeklywage ~ 
      i(urban, ref = 0) + ln_ma_removeown + ..by_college_group_averages | 
      agegroup + educ + white,
    data = data[college_degree == 1, ], 
    cluster = ~metarea, weights = ~perwt, lean = TRUE
  )

  est_nocollege <- feols(
    ln_weeklywage ~ 
      i(urban, ref = 0) + ln_ma_removeown + ..by_college_group_averages | 
      agegroup + educ + white,
    data = data[college_degree == 0, ], 
    cluster = ~metarea, weights = ~perwt, lean = TRUE
  )

  results_college <- bind_rows(results_college, tibble(
    year = rep(y, times = 2),
    est = list(est_college, est_nocollege) |>
      lapply(function(x) {
        coef(x)[["urban::1"]]
      }) |>
      unlist(),
    se = list(est_college, est_nocollege) |>
      lapply(function(x) {
        se(x)[["urban::1"]]
      }) |>
      unlist(),
    group = c("College", "No College")
  ))

}


# Export Results ---------------------------------------------------------------
# save(results_1940_msas, results_top20, results_region, results_college, file = glue("{gh}/data/estimates-heterogeneity.RData"))

# Plot Point Estimates ---------------------------------------------------------
# load(file = glue("{gh}/data/estimates-heterogeneity.RData"))

## Top 20 Populous Urban Areas -------------------------------------------------

results_top20_clean <- results_top20 |>
  filter(stringr::str_starts(code, "urban_top20")) |>
  mutate(
    group = stringr::str_remove(code, "urban_top20::"),
    group = factor(group, levels = c("Top 20", "Other Urban")),
    code = NULL,
    exp_est = exp(est) - 1
  )

(top20 <- ggplot(
    results_top20_clean, 
    aes(x = year, y = exp_est, group = group, color = group)
  ) +
  geom_line(aes(linetype = group), size = 2) +
  geom_point(aes(shape = group), size = 5) +
  labs(
    x = NULL, y = "Urban Wage Premium", group = "Specification",
    shape = "Specification", color = "Specification",
    linetype = "Specification"
  ) +
  scale_y_continuous(labels = scales::percent, limits = c(-0.02, 0.42)) +
  scale_x_continuous(breaks = seq(1940, 2020, by = 10)) +
  # ggsci::scale_color_jama() +
  scale_color_manual(values = c(
    "Top 20" = "grey10",
    "Other Urban" = "grey40"
  )) +
  scale_linetype_manual(values = c(
    "Top 20" = 1,
    "Other Urban" = 1
  )) +
  scale_shape_manual(values = c(
    "Top 20" = 15,
    "Other Urban" = 16
  )) +
  kfbmisc::theme_kyle(base_size = 18) +
  guides(
    colour = guide_legend(
      title.position = "top", nrow = 1,
      override.aes = list(linetype = 0)
    )
  ) +
  theme(
    legend.position = c(0.5, 0.88),
    panel.grid.minor.x = element_blank(),
    legend.background = element_rect(fill = "white", color = "gray20"),
    axis.line.y = element_blank(), axis.ticks.y = element_blank(),
    axis.line.x = element_blank(), axis.ticks.x = element_blank()
  ))

ggsave(glue("{gh}/paper/figures/urbanpremium_top20.pdf"), top20, width = 14, height = 4)


## Census Regions --------------------------------------------------------------

results_region_clean <- results_region |>
  filter(stringr::str_starts(code, "urban")) |>
  mutate(
    group = stringr::str_remove(code, "urban::1:region::"),
    group = factor(group, levels = c("North", "West", "South", "Midwest")),
    code = NULL,
    exp_est = exp(est) - 1
  )

(region <- ggplot(
    results_region_clean, 
    aes(x = year, y = exp_est, group = group, color = group)
  ) +
  geom_line(aes(linetype = group), size = 2) +
  geom_point(aes(shape = group), size = 5) +
  labs(
    x = NULL, y = "Urban Wage Premium", group = "Specification",
    shape = "Specification", color = "Specification",
    linetype = "Specification"
  ) +
  scale_y_continuous(labels = scales::percent, limits = c(-0.02, 0.42)) +
  scale_x_continuous(breaks = seq(1940, 2020, by = 10)) +
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
  kfbmisc::theme_kyle(base_size = 18) +
  guides(
    colour = guide_legend(
      title.position = "top", nrow = 1,
      override.aes = list(linetype = 0)
    )
  ) +
  theme(
    legend.position = c(0.5, 0.88),
    panel.grid.minor.x = element_blank(),
    legend.background = element_rect(fill = "white", color = "gray20"),
    axis.line.y = element_blank(), axis.ticks.y = element_blank(),
    axis.line.x = element_blank(), axis.ticks.x = element_blank()
  ))

ggsave(glue("{gh}/paper/figures/urbanpremium_region.pdf"), region, width = 14, height = 4)


## College vs. Non-College -----------------------------------------------------

results_college <- results_college |>
  mutate(
    year = as.numeric(year),
    est_lower90 = est - 1.65 * se,
    est_upper90 = est + 1.65 * se,
    est_lower95 = est - 1.96 * se,
    est_upper95 = est + 1.96 * se,
    exp_est = exp(est) - 1,
    exp_est_lower90 = exp(est_lower90) - 1,
    exp_est_upper90 = exp(est_upper90) - 1,
    exp_est_lower95 = exp(est_lower95) - 1,
    exp_est_upper95 = exp(est_upper95) - 1,
    group = factor(group)
  )

(plot_college <- ggplot(results_college, aes(x = year, y = exp_est, group = group, color = group, shape = group)) + 
  geom_line(size = 2, linetype = 1) +
  geom_point(size = 5) +
  labs(
    x = NULL, y = "Urban Wage Premium", 
    group = "Specification",
    shape = "Specification", color = "Specification",
    linetype = "Specification"
  ) +
  scale_y_continuous(labels = scales::percent, limits = c(-0.02, 0.42)) +
  scale_x_continuous(breaks = seq(1940, 2020, by = 10)) +
  scale_color_manual(values = c(
    "College" = "grey10",
    "No College" = "grey40"
  )) +
  scale_linetype_manual(values = c(
    "College" = 1,
    "No College" = 1
  )) +
  scale_shape_manual(values = c(
    "College" = 15,
    "No College" = 16
  )) +
  kfbmisc::theme_kyle(base_size = 18) +
  guides(
    colour = guide_legend(
      title.position = "top", nrow = 1, 
      override.aes = list(linetype = 0)
    )
  ) +
  theme(
    legend.position = c(0.5, 0.88),
    panel.grid.minor.x = element_blank(),
    legend.background = element_rect(fill = "white", color = "gray20"),
    axis.line.y = element_blank(), axis.ticks.y = element_blank(),
    axis.line.x = element_blank(), axis.ticks.x = element_blank()
  ))

ggsave(glue("{gh}/paper/figures/urbanpremium_college.pdf"), plot_college, width = 14, height = 4)




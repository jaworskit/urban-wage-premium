## results-skills.R ------------------------------------------------------------
## Kyle Butts, CU Boulder Economics

library(tidyverse)
library(glue)
library(broom)
library(vroom)
library(fixest)
library(collapse)
# devtools::install_github("kylebutts/kfbmisc")
library(kfbmisc)
library(data.table)

# Local
project <- "/Users/kylebutts/Dropbox/UrbanWagePremium"
gh <- "/Users/kylebutts/Documents/Projects/urban-wage-premium"

# Results ----------------------------------------------------------------------

## College vs. No College ------------------------------------------------------

data <- NULL
results_college <- NULL
ests_college <- NULL
ests_nocollege <- NULL
year_seq <- seq(1940, 2020, 10)

# Loop through year
# Removed 2005 and 2015 for data quality issue in 2005
for (i in 1:length(year_seq)) {
  y <- year_seq[i]
  cli::cli_alert_info("Starting on year {y}")

  # Sample has every observation except 15% sample of 1940 full count
  rm(data)
  data <- glue("{project}/data/dta/urban_wage_final_{y}.dta") |>
    haven::read_dta() |>
    data.table::setDT()

  ## Urban, Individual Controls, & Group Averages -------------
  est_college <- feols(
    ln_weeklywage ~ i(urban, ref = FALSE) + ln_ma_removeown + ..("by_college_share_") + i(educ) + i(white) + i(agegroup),
    data = data[college_degree == 1, ], cluster = ~metarea, weights = ~perwt, lean = TRUE
  )

  est_nocollege <- feols(
    ln_weeklywage ~ i(urban, ref = FALSE) + ln_ma_removeown + ..("by_college_share_") + i(educ) + i(white) + i(agegroup),
    data = data[college_degree == 0, ], cluster = ~metarea, weights = ~perwt, lean = TRUE
  )

  ests_college[[i]] <- est_college
  ests_nocollege[[i]] <- est_nocollege

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




## High School vs. No High School ----------------------------------------------

data <- NULL
results_hs <- NULL
ests_hs <- NULL
ests_nohs <- NULL
year_seq <- seq(1940, 2020, 10)

# Loop through year
# Removed 2005 and 2015 for data quality issue in 2005
for (i in 1:length(year_seq)) {
  y <- year_seq[i]
  cli::cli_alert_info("Starting on year {y}")

  # Sample has every observation except 15% sample of 1940 full count
  rm(data)
  data <- glue("{project}/data/dta/urban_wage_final_{y}.dta") |>
    haven::read_dta() |>
    data.table::setDT()

  ## Urban, Individual Controls, & Group Averages -------------
  est_hs <- feols(
    ln_weeklywage ~ i(urban, ref = FALSE) + ln_ma_removeown + ..("by_hs_share_") + i(educ) + i(white) + i(agegroup),
    data = data[hs_degree == 1, ], cluster = ~metarea, weights = ~perwt, lean = TRUE
  )

  est_nohs <- feols(
    ln_weeklywage ~ i(urban, ref = FALSE) + ln_ma_removeown + ..("by_hs_share_") + i(educ) + i(white) + i(agegroup),
    data = data[hs_degree == 0, ], cluster = ~metarea, weights = ~perwt, lean = TRUE
  )

  ests_hs[[i]] <- est_hs
  ests_nohs[[i]] <- est_nohs

  results_hs <- bind_rows(results_hs, tibble(
    year = rep(y, times = 2),
    est = list(est_hs, est_nohs) |>
      lapply(function(x) {
        coef(x)[["urban::1"]]
      }) |>
      unlist(),
    se = list(est_hs, est_nohs) |>
      lapply(function(x) {
        se(x)[["urban::1"]]
      }) |>
      unlist(),
    group = c("High School", "No High School")
  ))
}

# Export Results ---------------------------------------------------------------
# save(results_college, results_hs, ests_college, ests_nocollege, est_hs, est_nohs, file = glue("{gh}/data/estimates-skill.RData"))

# load(file = glue("{gh}/data/estimates-urban_indicator.RData"))


# Plot Point Estimates ---------------------------------------------------------

# exponentiate log differences
results_college <- results_college %>%
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

results_hs <- results_hs %>%
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



## Plot College vs. Non-college ------------------------------------------------

(plot_college <- ggplot(results_college, aes(x = year, y = exp_est, group = group, color = group, shape = group)) + 
  geom_line(size = 2, linetype = 1) +
  geom_point(size = 5) +
  labs(
    x = NULL, y = "Urban Wage Premium", 
    group = NULL, color = NULL, shape = NULL
  ) +
  scale_y_continuous(labels = scales::percent, limits = c(-0.075, 0.45)) +
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
  theme_kyle(base_size = 24) +
  guides(colour = guide_legend(title.position = "top", nrow = 1)) +
  theme(
    legend.position = "bottom",
    panel.grid.minor.x = element_blank()
  ))

ggsave(glue("{gh}/paper/figures/urbanpremium_college.pdf"), plot_college, width = 14, height = 6)


## Plot High-school vs. Non-high school ----------------------------------------

(plot_hs <- ggplot(results_hs, aes(x = year, y = exp_est, group = group, color = group, shape = group)) + 
  geom_line(size = 2, linetype = 1) +
  geom_point(size = 5) +
  labs(
    x = NULL, y = "Urban Wage Premium", 
    group = NULL, color = NULL, shape = NULL
  ) +
  scale_y_continuous(labels = scales::percent, limits = c(-0.075, 0.45)) +
  scale_x_continuous(breaks = seq(1940, 2020, by = 10)) +
  scale_color_manual(values = c(
    "High School" = "grey10",
    "No High School" = "grey40"
  )) +
  scale_linetype_manual(values = c(
    "High School" = 1,
    "No High School" = 1
  )) +
  scale_shape_manual(values = c(
    "High School" = 15,
    "No High School" = 16
  )) +
  theme_kyle(base_size = 24) +
  guides(colour = guide_legend(title.position = "top", nrow = 1)) +
  theme(
    legend.position = "bottom",
    panel.grid.minor.x = element_blank()
  ))

ggsave(glue("{gh}/paper/figures/urbanpremium_hs.pdf"), plot_hs, width = 14, height = 6)




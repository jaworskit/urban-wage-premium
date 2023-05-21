## results-skills.R ------------------------------------------------------------
## Kyle Butts, CU Boulder Economics

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

# Results ----------------------------------------------------------------------

## College vs. No College ------------------------------------------------------

data <- NULL
results_college <- NULL
ests_college <- NULL
ests_nocollege <- NULL
year_seq <- seq(1940, 2010, 10)

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
    share_educ10 + share_educ11
)

# Loop through year
# Removed 2005 and 2015 for data quality issue in 2005
for (i in 1:length(year_seq)) {
  y <- year_seq[i]
  cli::cli_alert_info("Starting on year {y}")

  # Sample has every observation except 15% sample of 1940 full count
  rm(data)
  data <- glue("{project}/data/dta/urban_wage_final_{y}.parquet") |>
    arrow::read_parquet() |>
    collect()

  data <- data |> haven::zap_labels() |> as.data.table()

  ## Urban, Individual Controls, & Group Averages -------------
  est_college <- feols(
    ln_weeklywage ~ 
      i(urban, ref = 0) + ln_ma_removeown + ..group_averages | 
      agegroup + educ + white,
    data = data[college_degree == 1, ], 
    cluster = ~metarea, weights = ~perwt, lean = TRUE
  )

  est_nocollege <- feols(
    ln_weeklywage ~ 
      i(urban, ref = 0) + ln_ma_removeown + ..group_averages | 
      agegroup + educ + white,
    data = data[college_degree == 0, ], 
    cluster = ~metarea, weights = ~perwt, lean = TRUE
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

# Export Results ---------------------------------------------------------------
# save(results_college, ests_college, ests_nocollege, file = glue("{gh}/data/estimates-skill.RData"))

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


## Plot College vs. Non-college ------------------------------------------------

(plot_college <- ggplot(results_college, aes(x = year, y = exp_est, group = group, color = group, shape = group)) + 
  geom_line(size = 2, linetype = 1) +
  geom_point(size = 5) +
  labs(
    x = NULL, y = "Urban Wage Premium", 
    group = NULL, color = NULL, shape = NULL
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
  theme_kyle(base_size = 18) +
  guides(colour = guide_legend(title.position = "top", nrow = 1)) +
  theme(
    legend.position = "bottom",
    panel.grid.minor.x = element_blank()
  ))

ggsave(glue("{gh}/paper/figures/urbanpremium_college.pdf"), plot_college, width = 14, height = 4)






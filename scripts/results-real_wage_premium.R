## results-urban_indicator.R ---------------------------------------------------
## Kyle Butts, CU Boulder Economics

library(tidyverse)
library(data.table)
library(glue)
library(broom)
library(vroom)
library(fixest)
library(collapse)
library(arrow)
# devtools::install_github("kylebutts/kfbmisc")
library(kfbmisc)

# Local
project <- "/Users/kylebutts/Dropbox/UrbanWagePremium"
gh <- "/Users/kylebutts/Documents/Projects/urban-wage-premium"

# Results ----------------------------------------------------------------------

## Regression Results ----------------------------------------------------------

data <- NULL
results <- NULL
ests_unadjusted <- list()
ests_controls <- list()
ests_group <- list()
# year_seq <- c(1940, 1950, 1960, 1970, 1980, 1990, 2000, 2010)
year_seq <- c(1940, 1960, 1970, 1980, 1990, 2000, 2010)

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
for (i in 1:length(year_seq)) {
  y <- year_seq[i]
  cli::cli_alert_info("Starting on year {y}")

  # Sample has every observation except 15% sample of 1940 full count
  # rm(data)
  
  data <- glue("{project}/data/dta/urban_wage_final_{y}.parquet") |>
    arrow::read_parquet() |> 
    collect()

  data = data |>
    filter(rent > 0) |> 
    # filter(!(year == 1940 & rent > 500)) |>
    mutate(
      take_home_pay = 52 * weeklywage - 12 * rent_avg
    ) |> 
    filter(take_home_pay > 0) |> 
    mutate(
      ln_realwage = log(take_home_pay),
      ln_rent = log(rent)
    )

  ## Urban ---------------------------------------------------------------------

  est1 <- feols(
    ln_realwage ~ i(urban),
    data = data, cluster = ~metarea, weights = ~perwt, lean = TRUE
  )

  ests_unadjusted[[i]] <- est1

  ## Urban & Individual Controls -----------------------------------------------

  est2 <- feols(
    ln_realwage ~ i(urban) | agegroup + educ + white,
    data = data, cluster = ~metarea, weights = ~perwt, lean = TRUE
  )

  ests_controls[[i]] <- est2

  ## Urban, Individual Controls & Group Averages -------------------------------

  (est3 <- feols(
    ln_realwage ~ 
      i(urban, ref = FALSE) + ln_ma_removeown + ..group_averages |
      agegroup + educ + white,
    data = data, cluster = ~metarea, weights = ~perwt, lean = TRUE
  ))

  ests_group[[i]] <- est3

  results <- bind_rows(results, tibble(
    year = rep(y, times = 3),
    est = list(est1, est2, est3) |>
      lapply(function(x) {
        coef(x)[["urban::1"]]
      }) |>
      unlist(),
    se = list(est1, est2, est3) |>
      lapply(function(x) {
        se(x)[["urban::1"]]
      }) |>
      unlist(),
    group = c("Urban Only", "Controls", "Group Averages")
  ))
}


# Export Results ---------------------------------------------------------------
# save(results, ests_unadjusted, ests_controls, ests_group, file = glue("{gh}/data/estimates-ln_realwage.RData"))

# load(file = glue("{gh}/data/estimates-real_wage.RData"))

# Plot Point Estimates ---------------------------------------------------------

# exponentiate log differences
results <- results %>%
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
    group = factor(group, levels = c("Raw", "Urban Only", "Controls", "Group Averages"))
  )

# temp = results
# results = results |> filter(year != 1950)

## Unadjusted ------------------------------------------------------------------

(urban <- ggplot(
  results[results$group == "Urban Only", ],
  aes(x = year, y = exp_est)
) +
  geom_line(linewidth = 2, linetype = 1, color = "black") +
  geom_errorbar(
    aes(ymin = exp_est_lower95, ymax = exp_est_upper95), 
    linewidth = 1.5, width = 1,
    color = "gray10"
  ) +
  geom_point(size = 5, shape = 15, color = "black") +
  labs(
    x = NULL, y = "Urban Wage Premium (Net of Housing)"
  ) +
  scale_y_continuous(labels = scales::percent, limits = c(-0.05, 0.33)) +
  scale_x_continuous(breaks = seq(1940, 2020, by = 10)) +
  kfbmisc::theme_kyle(base_size = 18) +
  guides(colour = guide_legend(title.position = "top", nrow = 1)) +
  theme(
    legend.position = "bottom",
    panel.grid.minor.x = element_blank(),
    axis.line.y = element_blank(), axis.ticks.y = element_blank(),
    axis.line.x = element_blank(), axis.ticks.x = element_blank()
  ))

ggsave(glue("{gh}/paper/figures/netofhousing_wagepremium_urban.pdf"), urban, width = 14, height = 6)

## Casual Estimate Only --------------------------------------------------------

(causal <- ggplot(
  results[results$group == "Group Averages", ],
  aes(x = year, y = exp_est)
) +
  geom_line(linewidth = 2, linetype = 1, color = "black") +
  geom_errorbar(
    aes(ymin = exp_est_lower95, ymax = exp_est_upper95), 
    linewidth = 1.5, width = 1,
    color = "gray10"
  ) +
  geom_point(size = 5, shape = 15, color = "black") +
  labs(
    x = NULL, y = "Urban Wage Premium (Net of Housing)"
  ) +
  scale_y_continuous(labels = scales::percent) +
  scale_y_continuous(labels = scales::percent, limits = c(-0.05, 0.33)) +
  scale_x_continuous(breaks = seq(1940, 2020, by = 10)) +
  kfbmisc::theme_kyle(base_size = 18) +
  guides(colour = guide_legend(title.position = "top", nrow = 1)) +
  theme(
    legend.position = "bottom",
    panel.grid.minor.x = element_blank(),
    axis.line.y = element_blank(), axis.ticks.y = element_blank(),
    axis.line.x = element_blank(), axis.ticks.x = element_blank()
  ))

ggsave(glue("{gh}/paper/figures/netofhousing_wagepremium_causal.pdf"), causal, width = 14, height = 6)
 


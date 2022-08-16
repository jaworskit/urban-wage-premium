## results-urban_indicator.R ---------------------------------------------------
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

## Boustan et. al --------------------------------------------------------------

# data <- glue("{project}/data/dta/urban_wage_final.dta") |>
#   haven::read_dta() |>
#   data.table::setDT()

# df0 <- data %>%
#   collapse::collap(totalincome + perwt ~ year + urban, fsum) %>%
#   mutate(weeklywage = totalincome / perwt) %>%
#   pivot_wider(id_cols = c("year"), values_from = weeklywage, names_from = urban, names_prefix = "urban_") %>%
#   mutate(est = log(urban_1 / urban_0), group = "Raw") %>%
#   select(year, est, group)

# # Store results
# results <- df0



## Regression Results ----------------------------------------------------------

data <- NULL
results <- NULL
ests_unadjusted <- list()
ests_controls <- list()
ests_group <- list()
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

  ## Urban ---------------------------------------------------------------------

  est1 <- feols(ln_weeklywage ~ i(urban),
    data = data, cluster = ~metarea, weights = ~perwt, lean = TRUE
  )

  ests_unadjusted[[i]] <- est1

  ## Urban & Individual Controls -----------------------------------------------

  est2 <- feols(ln_weeklywage ~ i(urban) | agegroup + educ + white,
    data = data, cluster = ~metarea, weights = ~perwt, lean = TRUE
  )

  ests_controls[[i]] <- est2

  ## Urban, Individual Controls & Group Averages -------------------------------

  est3 <- feols(
    ln_weeklywage ~ i(urban, ref = FALSE) + ln_ma_removeown + ..("^share_") + i(educ) + i(white) + i(agegroup),
    data = data, cluster = ~metarea, weights = ~perwt, lean = TRUE
  )

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
# save(results, ests_unadjusted, ests_controls, ests_group, file = glue("{gh}/data/estimates-urban_indicator.RData"))

# load(file = glue("{gh}/data/estimates-urban_indicator.RData"))



# Tables -----------------------------------------------------------------------

table_unadjusted <- fixest::etable(
  ests_unadjusted,
  keep = "Urban",
  dict = c(
    ln_weeklywage = "$\\log(\\text{Weekly Wage})$",
    "urban::1" = "$\\text{Urban} = 1$",
    "metarea" = "MSA/CBSA"
  ),
  fitstat = c("n"),
  tex = TRUE
)

table_controls <- fixest::etable(
  ests_controls,
  keep = "Urban",
  dict = c(
    ln_weeklywage = "$\\log(\\text{Weekly Wage})$",
    "urban::1" = "$\\text{Urban} = 1$",
    "metarea" = "MSA/CBSA"
  ),
  fitstat = c("n"),
  tex = TRUE
)

table_group <- fixest::etable(
  ests_group,
  keep = "Urban",
  dict = c(
    ln_weeklywage = "$\\log(\\text{Weekly Wage})$",
    "urban::1" = "$\\text{Urban} = 1$",
    "metarea" = "MSA/CBSA"
  ),
  fitstat = c("n"),
  tex = TRUE
)

cat(
  c(
    "\\multicolumn{10}{l}{\\hspace{-2.5mm}{\\bfseries \\itshape Panel A: Raw Wage Premium}} \\\\",
    table_unadjusted[9:11], 
    "\n\\multicolumn{10}{l}{\\hspace{-2.5mm}{\\bfseries \\itshape Panel B: Individual Controls}} \\\\",
    table_controls[9:11], 
    "\n\\multicolumn{10}{l}{\\hspace{-2.5mm}{\\bfseries \\itshape Panel C: Individual Controls \\& Group Averages}} \\\\",
    table_group[9:11],
    "\n",
    table_unadjusted[13]
  ), 
  sep = "\n"
)






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

## Unadjusted ------------------------------------------------------------------

(urban <- ggplot(
  results[results$group == "Urban Only", ],
  aes(x = year, y = exp_est)
) +
  geom_line(size = 2, linetype = 1, color = "black") +
  geom_linerange(size = 1.2, aes(ymin = exp_est_lower95, ymax = exp_est_upper95), color = "black") +
  geom_point(size = 5, shape = 15, color = "black") +
  labs(
    x = NULL, y = "Urban Wage Premium"
  ) +
  scale_y_continuous(labels = scales::percent, limits = c(-0.075, 0.45)) +
  scale_x_continuous(breaks = seq(1940, 2020, by = 10)) +
  theme_kyle(base_size = 24) +
  guides(colour = guide_legend(title.position = "top", nrow = 1)) +
  theme(
    legend.position = "bottom",
    panel.grid.minor.x = element_blank()
  ))

ggsave(glue("{gh}/paper/figures/urbanpremium_urban.pdf"), urban, width = 14, height = 6)


## Regression Results ----------------------------------------------------------

(controls <- ggplot(
  results,
  aes(x = year, y = exp_est, group = group, color = group, ymin = exp_est_lower95, ymax = exp_est_upper95)
) +
  geom_line(aes(linetype = group), size = 2) +
  geom_linerange(size = 1.2) +
  geom_point(aes(shape = group), size = 5) +
  labs(
    x = NULL, y = "Urban Wage Premium", group = "Specification",
    shape = "Specification", color = "Specification",
    linetype = "Specification"
  ) +
  scale_y_continuous(labels = scales::percent, limits = c(-0.075, 0.45)) +
  scale_x_continuous(breaks = seq(1940, 2020, by = 10)) +
  # ggsci::scale_color_jama() +
  scale_color_manual(values = c(
    # "Raw" = "grey40",
    "Urban Only" = "grey70",
    "Controls" = "grey40",
    # "Market Access" = "grey10",
    "Group Averages" = "grey10"
    # "Rent" = "grey10"
  )) +
  scale_linetype_manual(values = c(
    # "Raw" = 2,
    "Urban Only" = 1,
    "Controls" = 1,
    # "Market Access" = 1,
    "Group Averages" = 1
    # "Rent" = 1
  )) +
  scale_shape_manual(values = c(
    # "Raw" = 15,
    "Urban Only" = 15,
    "Controls" = 16,
    # "Market Access" = 17,
    "Group Averages" = 18
    # "Rent" = 4
  )) +
  theme_kyle(base_size = 24) +
  guides(
    colour = guide_legend(
      title.position = "top", nrow = 1,
      override.aes = list(linetype = 0)
    )
  ) +
  theme(
    legend.position = c(0.5, 0.88),
    panel.grid.minor.x = element_blank(),
    legend.background = element_rect(fill = "white", color = "gray20")
  ))

ggsave(glue("{gh}/paper/figures/urbanpremium_controls.pdf"), controls, width = 14, height = 6)


## Casual Estimate Only --------------------------------------------------------

(causal <- ggplot(
  results[results$group == "Group Averages", ],
  aes(x = year, y = exp_est)
) +
  geom_line(size = 2, linetype = 1, color = "black") +
  geom_linerange(size = 1.2, aes(ymin = exp_est_lower95, ymax = exp_est_upper95), color = "black") +
  geom_point(size = 5, shape = 15, color = "black") +
  labs(
    x = NULL, y = "Urban Wage Premium"
  ) +
  scale_y_continuous(labels = scales::percent, limits = c(-0.075, 0.45)) +
  scale_x_continuous(breaks = seq(1940, 2020, by = 10)) +
  theme_kyle(base_size = 24) +
  guides(colour = guide_legend(title.position = "top", nrow = 1)) +
  theme(
    legend.position = "bottom",
    panel.grid.minor.x = element_blank()
  ))

ggsave(glue("{gh}/paper/figures/urbanpremium_causal.pdf"), urban, width = 14, height = 6)



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
for (i in 1:length(year_seq)) {
  y <- year_seq[i]
  cli::cli_alert_info("Starting on year {y}")

  # Sample has every observation except 15% sample of 1940 full count
  rm(data)
  
  data <- glue("{project}/data/dta/urban_wage_final_{y}.parquet") |>
    arrow::read_parquet() |>
    collect()

  ## Urban ---------------------------------------------------------------------

  est1 <- feols(
    ln_weeklywage ~ i(urban),
    data = data, cluster = ~metarea, weights = ~perwt, lean = TRUE
  )

  ests_unadjusted[[i]] <- est1

  ## Urban & Individual Controls -----------------------------------------------

  est2 <- feols(
    ln_weeklywage ~ i(urban) | agegroup + educ + white,
    data = data, cluster = ~metarea, weights = ~perwt, lean = TRUE
  )

  ests_controls[[i]] <- est2

  ## Urban, Individual Controls & Group Averages -------------------------------

  est3 <- feols(
    ln_weeklywage ~ 
      i(urban, ref = FALSE) + 
      ln_ma_removeown + ..group_averages | 
      educ + white + agegroup,
    data = data, cluster = ~metarea, weights = ~perwt, lean = TRUE
  )
  # coef(est3)[["urban::1"]]
  # se(est3)[["urban::1"]]

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
    "\\multicolumn{9}{l}{\\hspace{-2.5mm}{\\bfseries \\itshape Panel A: Raw Wage Premium}} \\\\",
    table_unadjusted[9:11], 
    "\n",
    "\n\\multicolumn{9}{l}{\\hspace{-2.5mm}{\\bfseries \\itshape Panel B: Individual Controls}} \\\\",
    table_controls[9:11], 
    "\n",
    "\n\\multicolumn{9}{l}{\\hspace{-2.5mm}{\\bfseries \\itshape Panel C: Individual Controls \\& Group Averages}} \\\\",
    table_group[9:11],
    "\n",
    table_unadjusted[13]
  ) |>
    stringr::str_squish(), 
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
  geom_line(linewidth = 2, linetype = 1, color = "black") +
  geom_errorbar(
    aes(ymin = exp_est_lower95, ymax = exp_est_upper95), 
    linewidth = 1.5, width = 1
  ) +
  geom_point(size = 5, shape = 15, color = "black") +
  labs(
    x = NULL, y = "Urban Wage Premium"
  ) +
  scale_y_continuous(labels = scales::percent, limits = c(-0.02, 0.42)) +
  scale_x_continuous(breaks = seq(1940, 2010, by = 10)) +
  kfbmisc::theme_kyle(base_size = 20) +
  # pilot::theme_pilot() +
  guides(colour = guide_legend(title.position = "top", nrow = 1)) +
  theme(
    axis.title.y = element_text(size = rel(0.8)),
    legend.position = "bottom",
    panel.grid.minor.x = element_blank(), 
    axis.line.y = element_blank(), axis.ticks.y = element_blank(),
    axis.line.x = element_blank(), axis.ticks.x = element_blank()
  ))

ggsave(glue("{gh}/paper/figures/urbanpremium_urban.pdf"), urban, width = 14, height = 6)


## Regression Results ----------------------------------------------------------

(controls <- ggplot(
  # results,
  results |> 
    subset(results$group != "Group Averages") |>
    DT(, year_shift := ifelse(group == "Controls", year + 0.5, year - 0.5)),
  aes(x = year_shift, y = exp_est, group = group, color = group, ymin = exp_est_lower95, ymax = exp_est_upper95)
) +
  geom_line(aes(linetype = group), linewidth = 2) +
  geom_errorbar(
    aes(ymin = exp_est_lower95, ymax = exp_est_upper95), 
    linewidth = 1.5, width = 1
  ) +
  geom_point(aes(shape = group), size = 5) +
  labs(
    x = NULL, y = "Urban Wage Premium", group = "Specification",
    shape = "Specification", color = "Specification",
    linetype = "Specification"
  ) +
  scale_y_continuous(labels = scales::percent, limits = c(-0.02, 0.42)) +
  scale_x_continuous(breaks = seq(1940, 2010, by = 10)) +
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
  kfbmisc::theme_kyle(base_size = 20) +
  guides(
    colour = guide_legend(
      title.position = "top", nrow = 1,
      override.aes = list(linetype = 0)
    )
  ) +
  theme(
    axis.title.y = element_text(size = rel(0.8)),
    legend.position = c(0.5, 0.88),
    panel.grid.minor.x = element_blank(),
    legend.background = element_rect(fill = "white", color = "gray20"),
    axis.line.y = element_blank(), axis.ticks.y = element_blank(),
    axis.line.x = element_blank(), axis.ticks.x = element_blank()
  ))

ggsave(glue("{gh}/paper/figures/urbanpremium_controls.pdf"), controls, width = 14, height = 6)


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
  geom_point(size = 6, shape = 18, color = "black") +
  labs( 
    x = NULL, y = "Urban Wage Premium"
  ) +
  scale_y_continuous(labels = scales::percent, limits = c(-0.02, 0.42)) +
  scale_x_continuous(breaks = seq(1940, 2010, by = 10)) +
  kfbmisc::theme_kyle(base_size = 20) +
  guides(colour = guide_legend(title.position = "top", nrow = 1)) +
  theme(
    axis.title.y = element_text(size = rel(0.8)),
    legend.position = "bottom",
    panel.grid.minor.x = element_blank(),
    axis.line.y = element_blank(), axis.ticks.y = element_blank(),
    axis.line.x = element_blank(), axis.ticks.x = element_blank()
  ))

ggsave(glue("{gh}/paper/figures/urbanpremium_causal.pdf"), causal, width = 14, height = 6)



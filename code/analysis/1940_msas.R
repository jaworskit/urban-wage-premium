# TODO: Rewrite and make sure put in urban_premium
library(tidyverse)
library(data.table)
library(glue)
library(broom)
library(vroom)
library(fixest)
library(arrow)
# remotes::install_github("kylebutts/kfbmisc")
library(kfbmisc)

# Local
dropbox <- "~/Dropbox/UrbanWagePremium"
gh <- "~/Documents/Projects/urban-wage-premium"

# Results ----------------------------------------------------------------------

results <- NULL
year_seq <- seq(1940, 2010, 10)

setFixest_fml(
  ..group_averages = ~
    # Share white
    share_white + # share_nonwhite +
      # Share age groups
      share_agegroup_5 + share_agegroup_6 + share_agegroup_7 + share_agegroup_8 +
      share_agegroup_9 + share_agegroup_10 + share_agegroup_11 + share_agegroup_12 + # share_agegroup_13 +
      # Share veteran
      share_vetstat_1 + # share_vetstat_2 +
      share_marst_1 + share_marst_6 +
      share_marst_2 +
      # < HS
      I(share_educ_1 + share_educ_2 + share_educ_3 + share_educ_4 + share_educ_5) +
      # HS
      share_educ_6 +
      # Some College
      I(share_educ_7 + share_educ_8 + share_educ_9) +
      # BA and >BA
      share_educ_10 + share_educ_11
)


# Loop through year
for (y in year_seq) {
  # Sample has every observation except 15% sample of 1940 full count
  cli::cli_alert_info("Starting on year {y}")

  data <- glue("{dropbox}/data/dta/urban_wage_final_{y}.parquet") |>
    arrow::read_parquet() |>
    collect()

  data <- as.data.table(data)

  # data <- data[!is.na(pop_1940) | metarea == 0, ]
  data[, urban_top20 := fcase(
    top20, "Top 20",
    !top20, "Other Urban",
    default = "Non-urban"
  )]

  ## Urban on full sample ------------------------------------------------------

  est_full <- feols(
    ln_weeklywage ~
      i(urban, ref = FALSE) +
        ln_ma_removeown + ..group_averages |
        educ + white + agegroup,
    data = data, cluster = ~metarea, weights = ~perwt, lean = TRUE
  )

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


  results <- bind_rows(results, tibble(
    year = rep(y, times = 2),
    est = list(est_full, est_1940_msas) |>
      lapply(function(x) {
        coef(x)[["urban::1"]]
      }) |>
      unlist(),
    se = list(est_full, est_1940_msas) |>
      lapply(function(x) {
        se(x)[["urban::1"]]
      }) |>
      unlist(),
    group = c("All MSAs", "1940 MSAs")
  ))
}

results <- results |>
  mutate(
    exp_est = exp(est) - 1,
    est_lower90 = est - 1.65 * se,
    est_upper90 = est + 1.65 * se,
    est_lower95 = est - 1.96 * se,
    est_upper95 = est + 1.96 * se,
    exp_est_lower90 = exp(est_lower90) - 1,
    exp_est_upper90 = exp(est_upper90) - 1,
    exp_est_lower95 = exp(est_lower95) - 1,
    exp_est_upper95 = exp(est_upper95) - 1
  )

## 1940 MSAs -------------------------------------------------------------------


(urban_1940_msas <- ggplot(
  results |>
    filter(group == "1940 MSAs"),
  aes(x = year, y = exp_est)
) +
  geom_line(linewidth = 2, linetype = 1, color = "black") +
  geom_point(size = 5, shape = 15, color = "black") +
  geom_errorbar(
    aes(ymin = exp_est_lower95, ymax = exp_est_upper95),
    linewidth = 1.5, width = 1,
    color = "gray10"
  ) +
  labs(
    x = NULL, y = "Urban Wage Premium",
  ) +
  scale_y_continuous(
    labels = scales::percent,
    limits = c(-0.02, 0.44),
    expand = c(0, 0)
  ) +
  scale_x_continuous(breaks = seq(1940, 2020, by = 10)) +
  kfbmisc::theme_kyle(base_size = 16) +
  theme(
    legend.position = "bottom",
    axis.line.y = element_blank(), axis.ticks.y = element_blank(),
    axis.line.x = element_blank(), axis.ticks.x = element_blank()
  ))

## Full sample and 1940s -------------------------------------------------------

(urban_compared <- ggplot(
  results |>
    mutate(
      year = ifelse(group == "All MSAs", year - 0.5, year + 0.5)
    ) |>
    arrange(desc(group)),
  aes(x = year, y = exp_est, group = group, color = group, shape = group)
) +
  geom_line(linewidth = 2, linetype = 1) +
  geom_point(size = 5, shape = 15) +
  geom_errorbar(
    aes(ymin = exp_est_lower95, ymax = exp_est_upper95),
    linewidth = 1.5, width = 1,
  ) +
  labs(
    x = NULL, y = "Urban Wage Premium",
    group = NULL, shape = NULL,
    color = NULL, linetype = NULL
  ) +
  scale_y_continuous(
    labels = scales::percent,
    limits = c(-0.02, 0.44),
    expand = c(0, 0)
  ) +
  scale_x_continuous(breaks = seq(1940, 2020, by = 10)) +
  scale_color_manual(values = c(
    "1940 MSAs" = "grey10",
    "All MSAs" = "grey70"
  )) +
  kfbmisc::theme_kyle(base_size = 16) +
  guides(
    colour = guide_legend(
      title.position = "top", nrow = 1,
      override.aes = list(linetype = 0)
    )
  ) +
  theme(
    legend.position = c(0.5, 0.88),
    legend.background = element_rect(fill = "white", color = "gray20"),
    legend.margin = margin(4, 12, 12, 12),
    panel.grid.minor.x = element_blank(),
    axis.line.y = element_blank(), axis.ticks.y = element_blank(),
    axis.line.x = element_blank(), axis.ticks.x = element_blank()
  ))


# %%
kfbmisc::tikzsave(
  glue("{gh}/out/figures/urban_premium/1940_msas.pdf"),
  urban_1940_msas,
  width = 14, height = 6
)
kfbmisc::tikzsave(
  glue("{gh}/out/figures/urban_premium/1940_msas_compared.pdf"),
  urban_compared,
  width = 14, height = 6
)

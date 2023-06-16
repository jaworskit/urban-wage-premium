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

results <- NULL
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
for (y in year_seq) {

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
  kfbmisc::theme_kyle(base_size = 18) +
  theme(
    legend.position = "bottom",
    axis.line.y = element_blank(), axis.ticks.y = element_blank(),
    axis.line.x = element_blank(), axis.ticks.x = element_blank()
  ))

ggsave(glue("{gh}/paper/figures/urbanpremium_1940_msas.pdf"), urban_1940_msas, width = 14, height = 6)


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
  kfbmisc::theme_kyle(base_size = 18) +
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

ggsave(
  glue("{gh}/paper/figures/urbanpremium_1940_msas_compared.pdf"), 
  urban_compared, width = 14, height = 6
)



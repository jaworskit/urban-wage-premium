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
results_top20 <- NULL
results_region <- NULL

# Loop through year
for (y in c(1940, 1950, 1960, 1970, 1980, 1990, 2000, 2010, 2020)) {

  # Sample has every observation except 15% sample of 1940 full count
  cli::cli_alert_info("Starting on year {y}")
  data <- glue("{project}/data/dta/urban_wage_final_{y}.dta") |>
    haven::read_dta() |>
    data.table::setDT()

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

  data <- data[!is.na(pop_1940) | metarea == 0, ]
  data[, urban_top20 := fcase(
    top20, "Top 20",
    !top20, "Urban",
    default = "Non-urban"
  )]

  # get regions
  data <- dplyr::left_join(data, regions, by = "statefip")
  cli::cli_alert_warning("Year {y} has {nrow(data)} observations")



  ## Urban on 1940 MSA subsample -----------------------------------------------

  est <- feols(
    ln_weeklywage ~ i(urban) + ln_ma_removeown + ..("share_") + i(educ) + i(white) + i(agegroup),
    data = data, cluster = ~metarea, weights = ~perwt, lean = TRUE
  )


  results <- bind_rows(
    results,
    tibble(
      code = names(coef(est)), 
      est = coef(est), 
      se = se(est), 
      year = y
    )
  )

  ## Top 20 Populous Urban Areas ---------------------------------------------

  est_top20 <- feols(
    ln_weeklywage ~ i(urban_top20, ref = "Non-urban") + ln_ma_removeown + ..("share_") + i(educ) + i(white) + i(agegroup),
    data = data, cluster = ~metarea, weights = ~perwt, lean = TRUE
  )

  results_top20 <- bind_rows(
    results_top20,
    tibble(code = names(coef(est_top20)), est = coef(est_top20), year = y)
  )

  ## Regions of US -----------------------------------------------------------

  est_region <- feols(
    ln_weeklywage ~ i(urban, i.region, ref = "0") + ln_ma_removeown + ..("share_") + i(educ) + i(white) + i(agegroup),
    data = data, cluster = ~metarea, weights = ~perwt, lean = TRUE
  )

  results_region <- bind_rows(
    results_region,
    tibble(code = names(coef(est_region)), est = coef(est_region), year = y)
  )
}


# Export Results ---------------------------------------------------------------
# save(results, results_top20, results_region, file = glue("{gh}/data/estimates-heterogeneity.RData"))


# Plot Point Estimates ---------------------------------------------------------
# load(file = glue("{gh}/data/estimates-heterogeneity.RData"))



results_clean <- results |>
  filter(stringr::str_starts(code, "urban")) |>
  mutate(
    group = "Urban",
    group = factor(group, levels = "Urban"),
    code = NULL,
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

results_top20_clean <- results_top20 |>
  filter(stringr::str_starts(code, "urban_top20")) |>
  mutate(
    group = stringr::str_remove(code, "urban_top20::"),
    group = factor(group, levels = c("Top 20", "Urban")),
    code = NULL,
    exp_est = exp(est) - 1
  )

results_region_clean <- results_region |>
  filter(stringr::str_starts(code, "urban")) |>
  mutate(
    group = stringr::str_remove(code, "urban::1:region::"),
    group = factor(group, levels = c("North", "West", "South", "Midwest")),
    code = NULL,
    exp_est = exp(est) - 1
  )



(urban_1940_msas <- ggplot(
    results_clean, 
    aes(x = year, y = exp_est)
  ) +
  geom_line(size = 2, linetype = 1, color = "black") +
  geom_point(size = 5, shape = 15, color = "black") +
  labs(
    x = NULL, y = "Urban Wage Premium", group = "Specification",
    shape = "Specification", color = "Specification",
    linetype = "Specification"
  ) +
  scale_y_continuous(labels = scales::percent, limits = c(-0.06, 0.45)) +
  scale_x_continuous(breaks = seq(1940, 2020, by = 10)) +
  kfbmisc::theme_kyle(base_size = 24) +
  theme(legend.position = "bottom"))


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
  scale_y_continuous(labels = scales::percent, limits = c(-0.06, 0.45)) +
  scale_x_continuous(breaks = seq(1940, 2020, by = 10)) +
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
  guides(
    colour = guide_legend(
      title.position = "top", nrow = 1,
      override.aes = list(linetype = 0)
    )
  ) +
  theme(
    legend.position = c(0.5, 0.88),
    panel.grid.minor.x = element_blank(),
    legend.background = element_rect(fill="white", color="gray20")
  ))


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
  scale_y_continuous(labels = scales::percent, limits = c(-0.06, 0.45)) +
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
  kfbmisc::theme_kyle(base_size = 24) +
  guides(
    colour = guide_legend(
      title.position = "top", nrow = 1,
      override.aes = list(linetype = 0)
    )
  ) +
  theme(
    legend.position = c(0.5, 0.88),
    panel.grid.minor.x = element_blank(),
    legend.background = element_rect(fill="white", color="gray20")
  ))


kfbmisc::ggpreview(urban_1940_msas, device = "pdf", width = 14, height = 6)

ggsave(glue("{gh}/paper/figures/urbanpremium_1940_msas.pdf"), urban_1940_msas, width = 14, height = 6)
ggsave(glue("{gh}/paper/figures/urbanpremium_top20.pdf"), top20, width = 14, height = 6)
ggsave(glue("{gh}/paper/figures/urbanpremium_region.pdf"), region, width = 14, height = 6)


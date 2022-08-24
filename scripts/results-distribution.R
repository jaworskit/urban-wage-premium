## results-distribution.R ------------------------------------------------------
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



# Regression Results -----------------------------------------------------------

dataset <- NULL
results <- NULL
year_seq <- seq(1940, 2020, 10)

# Loop through year
# Removed 2005 and 2015 for data quality issue in 2005
for (i in 1:length(year_seq)) {
  y <- year_seq[i]
  # y <- 1950
  cli::cli_alert_info("Starting on year {y}")

  # Sample has every observation except 15% sample of 1940 full count
  rm(data)
  data <- glue("{project}/data/dta/urban_wage_final_{y}.dta") |>
    haven::read_dta() |>
    data.table::setDT()

  data = data[perwt != 0, ]

  ## Urban, Individual Controls & Group Averages -------------------------------

  est <- feols(
    ln_weeklywage ~ i(urban, ref = 0) + ln_ma_removeown + ..("^share_") + i(educ) + i(white) + i(agegroup),
    data = data, cluster = ~metarea, weights = ~perwt
  )
  
  urban_premium = coef(est)["urban::1"]
  average_ln_weeklywage = data[, weighted.mean(ln_weeklywage, w = perwt)]

  data[, 
    ln_weeklywage_resid := ln_weeklywage - predict(est) + urban_premium * urban 
  ]
  data$average_ln_weeklywage = average_ln_weeklywage
  data$urban_premium = urban_premium

  dataset <- rbind(dataset, data[, .(year, urban, ln_weeklywage_resid, average_ln_weeklywage, urban_premium)])

  # ggplot(data) +
  #   geom_density(
  #     aes(
  #       x = ln_weeklywage_resid, 
  #       group = ifelse(urban, "Urban", "Nonurban"), 
  #       color = ifelse(urban, "Urban", "Nonurban")
  #     ), 
  #     alpha = 0.5
  #   )
}

# Export Results ---------------------------------------------------------------
# save(dataset, file = glue("{gh}/data/estimates-distribution.RData"))
# load(file = glue("{gh}/data/estimates-distribution.RData"))

# Plot Results -----------------------------------------------------------------

dataset[, urban := ifelse(urban == 1, "Urban", "Nonurban")]
dataset[, year := as.numeric(year)]

# Gives us our urban wage premium estimates
# dataset[, mean(ln_weeklywage_resid), by = .(year, urban)]

## 90th-10th percentile
wage_percentiles = dataset[, .(
  ln_weeklywage_resid_mean = mean(ln_weeklywage_resid),
  ln_weeklywage_resid_05   = quantile(ln_weeklywage_resid, 0.05),
  ln_weeklywage_resid_10   = quantile(ln_weeklywage_resid, 0.1),
  ln_weeklywage_resid_20   = quantile(ln_weeklywage_resid, 0.2),
  ln_weeklywage_resid_50   = quantile(ln_weeklywage_resid, 0.5),
  ln_weeklywage_resid_80   = quantile(ln_weeklywage_resid, 0.8),
  ln_weeklywage_resid_90   = quantile(ln_weeklywage_resid, 0.9),
  ln_weeklywage_resid_95   = quantile(ln_weeklywage_resid, 0.95)
), by = year]

gap_9010 = wage_percentiles[, .(year, gap_9010 = ln_weeklywage_resid_90 - ln_weeklywage_resid_10)]

(plot_gap_9010 <- ggplot(gap_9010) + 
  geom_point(
    aes(x = year, y = gap_9010),
    size = 3
  ) +
  geom_line(
    aes(x = year, y = gap_9010),
    size = 1.2
  ) +
  labs(
    y = "Log Wage Residaul, 90th - 10th", x = NULL, 
    group = NULL, color = NULL
  ) +
  theme_kyle(base_size = 24)
)

# kfbmisc::ggpreview(plot_gap_9010, device = "pdf", width = 14, height = 6)
ggsave(glue("{gh}/paper/figures/urbanpremium_gap_9010.pdf"), plot_gap_9010,  width = 14, height = 6)

(plot_9010 <- ggplot(wage_percentiles) + 
  geom_linerange(
    aes(x = year, ymin = ln_weeklywage_resid_10, ymax = ln_weeklywage_resid_90),
    size = 2, color = "gray10", alpha = 0.6
  ) +
  geom_linerange(
    aes(x = year, ymin = ln_weeklywage_resid_20, ymax = ln_weeklywage_resid_80),
    size = 2, color = "gray10", alpha = 0.9
  ) +
  geom_point(
    aes(x = year, y = ln_weeklywage_resid_90),
    size = 3
  ) +
  geom_point(
    aes(x = year, y = ln_weeklywage_resid_10),
    size = 3
  ) +
  geom_point(
    aes(x = year, y = ln_weeklywage_resid_50),
    size = 2
  ) +
  labs(
    y = "Wage Residual", x = NULL, 
    group = NULL, color = NULL
  ) +
  theme_kyle(base_size = 24))

# kfbmisc::ggpreview(plot_9010, device = "pdf", width = 14, height = 6)
ggsave(glue("{gh}/paper/figures/urbanpremium_9010.pdf"), plot_9010,  width = 14, height = 6)

(plot_9505 <- ggplot(wage_percentiles) + 
  geom_linerange(
    aes(x = year, ymin = ln_weeklywage_resid_05, ymax = ln_weeklywage_resid_95),
    size = 2, color = "gray10", alpha = 0.6
  ) +
  geom_linerange(
    aes(x = year, ymin = ln_weeklywage_resid_20, ymax = ln_weeklywage_resid_80),
    size = 2, color = "gray10", alpha = 0.9
  ) +
  geom_point(
    aes(x = year, y = ln_weeklywage_resid_95),
    size = 3
  ) +
  geom_point(
    aes(x = year, y = ln_weeklywage_resid_05),
    size = 3
  ) +
  geom_point(
    aes(x = year, y = ln_weeklywage_resid_50),
    size = 2
  ) +
  labs(
    y = "Wage Residual", x = NULL, 
    group = NULL, color = NULL
  ) +
  theme_kyle(base_size = 24))

# kfbmisc::ggpreview(plot_9505, device = "pdf", width = 14, height = 6)
ggsave(glue("{gh}/paper/figures/urbanpremium_9505.pdf"), plot_9505,  width = 14, height = 6)




## 90th-10th percentile by Urban/Non-urban
wage_percentiles = dataset[, .(
  ln_weeklywage_resid_mean = mean(ln_weeklywage_resid),
  ln_weeklywage_resid_05   = quantile(ln_weeklywage_resid, 0.05),
  ln_weeklywage_resid_10   = quantile(ln_weeklywage_resid, 0.1),
  ln_weeklywage_resid_20   = quantile(ln_weeklywage_resid, 0.2),
  ln_weeklywage_resid_50   = quantile(ln_weeklywage_resid, 0.5),
  ln_weeklywage_resid_80   = quantile(ln_weeklywage_resid, 0.8),
  ln_weeklywage_resid_90   = quantile(ln_weeklywage_resid, 0.9),
  ln_weeklywage_resid_95   = quantile(ln_weeklywage_resid, 0.95)
), by = .(year, urban)]

wage_percentiles[, .(year, gap_90_10 = ln_weeklywage_resid_90 - ln_weeklywage_resid_10), by = urban]

wage_percentiles[, year_pos := year + 1 - 2 * (urban == "Urban")]

(plot_9010_urban <- ggplot(wage_percentiles) + 
  geom_linerange(
    aes(x = year_pos, ymin = ln_weeklywage_resid_10, ymax = ln_weeklywage_resid_90, color = urban, group = urban),
    size = 2, alpha = 0.6
  ) +
  geom_linerange(
    aes(x = year_pos, ymin = ln_weeklywage_resid_20, ymax = ln_weeklywage_resid_80, color = urban, group = urban),
    size = 2, alpha = 0.9
  ) +
  geom_point(
    aes(x = year_pos, y = ln_weeklywage_resid_90),
    size = 3
  ) +
  geom_point(
    aes(x = year_pos, y = ln_weeklywage_resid_10),
    size = 3
  ) +
  geom_point(
    aes(x = year_pos, y = ln_weeklywage_resid_50),
    size = 2.5
  ) +
  labs(
    y = "Wage Residual", x = NULL, 
    group = NULL, color = NULL
  ) +
  scale_color_manual(
    values = c("Urban" = "grey10", "Non-Urban" = "grey40")
  ) +
  scale_x_continuous(breaks = seq(1940, 2020, by = 10)) +
  theme_kyle(base_size = 24) + 
  guides(colour = guide_legend(nrow = 1)) +
  theme(
    legend.position = "bottom",
    panel.grid.minor.x = element_blank()
  ))

# kfbmisc::ggpreview(plot_9010_urban, device = "pdf", width = 14, height = 6)
ggsave(glue("{gh}/paper/figures/urbanpremium_9010_urban.pdf"), plot_9010_urban,  width = 14, height = 6)


(plot_9505_urban <- ggplot(wage_percentiles) + 
  geom_linerange(
    aes(x = year_pos, ymin = ln_weeklywage_resid_05, ymax = ln_weeklywage_resid_95, color = urban, group = urban),
    size = 2, alpha = 0.6
  ) +
  geom_linerange(
    aes(x = year_pos, ymin = ln_weeklywage_resid_20, ymax = ln_weeklywage_resid_80, color = urban, group = urban),
    size = 2, alpha = 0.9
  ) +
  geom_point(
    aes(x = year_pos, y = ln_weeklywage_resid_95),
    size = 3
  ) +
  geom_point(
    aes(x = year_pos, y = ln_weeklywage_resid_05),
    size = 3
  ) +
  geom_point(
    aes(x = year_pos, y = ln_weeklywage_resid_50),
    size = 2
  ) +
  labs(
    y = "Wage Residual", x = NULL, 
    group = NULL, color = NULL
  ) +
  scale_color_manual(
    values = c("Urban" = "grey10", "Non-Urban" = "grey40")
  ) +
  scale_x_continuous(breaks = seq(1940, 2020, by = 05)) +
  theme_kyle(base_size = 24) + 
  guides(colour = guide_legend(nrow = 1)) +
  theme(
    legend.position = "bottom",
    panel.grid.minor.x = element_blank()
  ))

# kfbmisc::ggpreview(plot_9505_urban, device = "pdf", width = 14, height = 6)
ggsave(glue("{gh}/paper/figures/urbanpremium_9505_urban.pdf"), plot_9505_urban,  width = 14, height = 6)



## Distributional Facet Plot
(distributions <- ggplot(dataset[abs(ln_weeklywage_resid) < 2, ]) +
  geom_density(
    aes(
      x = ln_weeklywage_resid, 
      group = urban,
      color = urban
    ), 
    size = 1.5
  ) + 
  facet_wrap(~ year) +
  labs(
    x = "Residaulized Log Weekly Wage", y = "Density", group = NULL, color = NULL
  ) + 
  scale_color_manual(values = c(
    "Urban" = "grey10",
    "Non-Urban" = "grey40"
  )) +
  theme_kyle(base_size = 24) + 
  guides(colour = guide_legend(nrow = 1)) +
  theme(
    legend.position = "bottom"
  ))

kfbmisc::ggpreview(distributions, device = "pdf", width = 20, height = 14)

ggsave(glue("{gh}/paper/figures/urbanpremium_distribution.pdf"), distributions, width = 20, height = 14)

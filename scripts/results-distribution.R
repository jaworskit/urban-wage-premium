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



## Regression Results ----------------------------------------------------------

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

  ## Urban, Individual Controls & Group Averages -------------------------------

  est <- feols(
    ln_weeklywage ~ i(urban, ref = 0) + ln_ma_removeown + ..("share_") + i(educ) + i(white) + i(agegroup),
    data = data, cluster = ~metarea, weights = ~perwt
  )
  
  urban_premium = coef(est)["urban::1"]
  average_ln_weeklywage = mean(data$ln_weeklywage)

  data$ln_weeklywage_resid <- data$ln_weeklywage - predict(est) + urban_premium * data$urban

  dataset <- rbind(dataset, data[, .(year, urban, ln_weeklywage_resid)])

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
# save(datasets, file = glue("{gh}/data/estimates-distribution.RData"))

# load(file = glue("{gh}/data/estimates-distribution.RData"))

# ---- Plot Results ------------------------------------------------------------

dataset[, urban := ifelse(urban == 1, "Urban", "Nonurban")]
dataset = dataset[abs(ln_weeklywage_resid) < 2, ]

# Gives us our urban wage premium estimates
# dataset[, mean(ln_weeklywage_resid), by = .(year, urban)]

(distributions <- ggplot(dataset) +
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

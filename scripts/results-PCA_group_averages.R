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

# y <- 1940
y <- 2010


data <- glue("{project}/data/dta/urban_wage_final_{y}.parquet") |>
    arrow::read_parquet() |>
    collect()

data <- data |> haven::zap_labels() |> as.data.table()

data[, code := ifelse(
  code == 0, paste0(statefip, "00000"), code
)]

data[, share_some_college := share_educ7 + share_educ8 + share_educ9]
data[, share_less_than_hs := share_educ1 + share_educ2 + share_educ3 + share_educ4 + share_educ5]

group_averages <- c("ln_ma_removeown", "share_race0",  "share_age5", "share_age6", "share_age7", "share_age8", "share_age9", "share_age10", "share_age11", "share_age12", "share_vetstat1", "share_marst1", "share_marst6", "share_marst2", "share_less_than_hs", "share_educ6", "share_some_college", "share_educ10", "share_educ11")

group_averages_df = data[, 
  lapply(.SD, first), 
  by = code, 
  .SDcols = group_averages
]

prcomp(group_averages_df[, -"code"], center = TRUE, scale. = TRUE) |> 
  summary()

# 1940 Importance of components:
# PC1    PC2    PC3     PC4     PC5     PC6     PC7    PC8     
# Standard deviation
# 2.6064 1.9199 1.5341 1.24941 1.00116 0.9649 0.75964 0.68007
# Proportion of Variance
# 0.3575 0.1940 0.1239 0.08216 0.05275 0.0490 0.03037 0.02434
# Cumulative Proportion
# 0.3575 0.5515 0.6754 0.75757 0.81032 0.8593 0.88969 0.91403


# 2010 Importance of components:
# PC1    PC2    PC3     PC4     PC5     PC6     PC7     PC8
# Standard deviation
# 2.4123 1.7397 1.4753 1.33807 1.08831 1.03845 0.93499 0.75165
# Proportion of Variance
# 0.3063 0.1593 0.1145 0.09423 0.06234 0.05676 0.04601 0.02974
# Cumulative Proportion
# 0.3063 0.4656 0.5801 0.67436 0.73669 0.79345 0.83946 0.86920


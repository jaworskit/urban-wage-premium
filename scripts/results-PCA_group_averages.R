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

y <- 2020
data <- glue("{project}/data/dta/urban_wage_final_{y}.dta") |>
  haven::read_dta() |>
  data.table::setDT()
data[, code := ifelse(
  code == 0, paste0(statefip, "00000"), code
)]


group_averages <- c(
  "ln_ma_removeown", 
  colnames(data)[stringr::str_starts(colnames(data), "share_")]
)
group_averages_df = data[, 
  lapply(.SD, first), 
  by = code, 
  .SDcols = group_averages
]

# Remove 0s
if(y == 2020) {
  group_averages_df = group_averages_df[, -c("share_vetstat0", "share_vetstat9", "share_educ9", "share_educ99")]
}

prcomp(group_averages_df[, -"code"], center = TRUE, scale. = TRUE) |> 
  summary()

# 1940 Importance of components:
# PC1    PC2    PC3     PC4     PC5     PC6     PC7    PC8     
# Standard deviation     
# 2.9734 2.5686 1.6806 1.48592 1.42283 1.30126 1.15799 1.1068 
# Proportion of Variance 
# 0.2526 0.1885 0.0807 0.06308 0.05784 0.04838 0.03831 0.0350 
# Cumulative Proportion  
# 0.2526 0.4411 0.5218 0.58489 0.64273 0.69111 0.72942 0.7644 

# 2020 Importance of components:
# PC1    PC2    PC3     PC4     PC5     PC6     PC7    PC8     
# Standard deviation     
# 2.7215 2.2588 1.75498 1.57862 1.3070 1.08614 1.0725 1.03100
# Proportion of Variance 
# 0.2389 0.1646 0.09935 0.08039 0.0551 0.03805 0.0371 0.03429
# Cumulative Proportion  
# 0.2389 0.4035 0.50285 0.58324 0.6383 0.67639 0.7135 0.74779 

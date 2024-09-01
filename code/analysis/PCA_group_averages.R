# %%
library(tidyverse)
library(glue)
library(arrow)
library(collapse)
# remotes::install_github("kylebutts/kfbmisc")
library(kfbmisc)

dropbox <- "/Users/kylebutts/Dropbox/UrbanWagePremium"
gh <- "/Users/kylebutts/Documents/Projects/urban-wage-premium"

# %%
data <- glue("{dropbox}/data/parquet/urban_wage") |>
  arrow::open_dataset()

group_averages_pca_decomp <- function(data, y = 2010) {
  group_averages_df <- data |>
    filter(year == y) |>
    mutate(
      code = if_else(code == 0, paste0(statefip, "0000"), as.character(code)),
      share_some_college = share_educ_7 + share_educ_8 + share_educ_9,
      share_less_than_hs = share_educ_1 + share_educ_2 + share_educ_3 + share_educ_4 + share_educ_5
    ) |>
    select(
      code, ln_ma_removeown,
      share_white,
      share_agegroup_5, share_agegroup_6, share_agegroup_7, share_agegroup_8, share_agegroup_9, share_agegroup_10, share_agegroup_11, share_agegroup_12,
      share_vetstat_1,
      share_marst_1, share_marst_6, share_marst_2,
      share_less_than_hs,
      share_educ_6, share_some_college, share_educ_10, share_educ_11
    ) |>
    collect() |>
    slice(1, .by = code)

  group_averages_df |>
    select(-code) |>
    prcomp(center = TRUE, scale. = TRUE) |>
    summary()
}

# %%
group_averages_pca_decomp(data, y = 1940)
# 1940 Importance of components:
# PC1    PC2    PC3     PC4     PC5     PC6     PC7    PC8
# Standard deviation
# 2.6064 1.9199 1.5341 1.24941 1.00116 0.9649 0.75964 0.68007
# Proportion of Variance
# 0.3575 0.1940 0.1239 0.08216 0.05275 0.0490 0.03037 0.02434
# Cumulative Proportion
# 0.3575 0.5515 0.6754 0.75757 0.81032 0.8593 0.88969 0.91403

group_averages_pca_decomp(data, y = 2010)
# 2010 Importance of components:
# PC1    PC2    PC3     PC4     PC5     PC6     PC7    PC8
# Standard deviation
# 2.4126 1.7381 1.4753 1.34008 1.08813 1.03841 0.93401 0.7525
# Proportion of Variance
# 0.3064 0.1590 0.1146 0.09452 0.06232 0.05675 0.04591 0.0298
# Cumulative Proportion
# 0.3064 0.4653 0.5799 0.67442 0.73674 0.79349 0.83941 0.8692

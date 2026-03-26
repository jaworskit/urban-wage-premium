# %%
library(tidyverse)
library(glue)
library(arrow)
library(collapse)
library(here)
# remotes::install_github("kylebutts/kfbmisc")
library(kfbmisc)

dropbox <- "~/Dropbox/Projects/UrbanWagePremium"
gh <- "~/Documents/Projects/urban-wage-premium"

source(here("code/utils/calculate_group_averages.R"))

group_averages <- c(
  "share_nonwhite",
  "share_agegroup_5",
  "share_agegroup_6",
  "share_agegroup_7",
  "share_agegroup_8",
  "share_agegroup_9",
  "share_agegroup_10",
  "share_agegroup_11",
  "share_agegroup_12",
  "share_vetstat_1",
  "share_marst_1",
  "share_marst_2",
  "share_marst_6",
  "share_educ_lt_hs",
  "share_educ_hs",
  "share_educ_some_college",
  "share_educ_college_plus"
)
setFixest_fml(
  ..group_averages = reformulate(group_averages)
)

# %%
data <- glue("{dropbox}/data/parquet/urban_wage") |>
  arrow::open_dataset() |>
  filter(ind_main_sample == TRUE)

group_averages_pca_decomp <- function(data, y = 2010) {
  group_averages_df <- data |>
    filter(year == y) |>
    mutate(
      code = if_else(code == 0, paste0(statefip, "0000"), as.character(code)),
    ) |>
    collect() |>
    calculate_group_averages(by = "msacode") |>
    select(
      code,
      ln_ma_removeown,
      share_nonwhite,
      share_agegroup_5,
      share_agegroup_6,
      share_agegroup_7,
      share_agegroup_8,
      share_agegroup_9,
      share_agegroup_10,
      share_agegroup_11,
      share_agegroup_12,
      share_vetstat_1,
      share_marst_1,
      share_marst_2,
      share_marst_6,
      share_educ_lt_hs,
      share_educ_hs,
      share_educ_some_college,
      share_educ_college_plus
    ) |>
    slice(1, .by = code)

  group_averages_df |>
    select(-code) |>
    drop_na() |>
    prcomp(center = TRUE, scale. = TRUE)
}

# %%
decomp_1940 <- group_averages_pca_decomp(data, y = 1940)
decomp_2010$importance[, 1:8]
# 1940 Importance of components:
##                             PC1      PC2      PC3     PC4      PC5      PC6       PC7       PC8
## Standard deviation     2.399214 1.583181 1.465779 1.33414 1.083605 1.025641 0.8833033 0.7374075
## Proportion of Variance 0.319790 0.139250 0.119360 0.09889 0.065230 0.058440 0.0433500 0.0302100
## Cumulative Proportion  0.319790 0.459040 0.578400 0.67729 0.742520 0.800960 0.8443100 0.8745100

decomp_2010 <- group_averages_pca_decomp(data, y = 2010)
decomp_2010$importance[, 1:8]
# 2010 Importance of components:
#                             PC1      PC2      PC3     PC4      PC5      PC6       PC7       PC8
# Standard deviation     2.399214 1.583181 1.465779 1.33414 1.083605 1.025641 0.8833033 0.7374075
# Proportion of Variance 0.319790 0.139250 0.119360 0.09889 0.065230 0.058440 0.0433500 0.0302100
# Cumulative Proportion  0.319790 0.459040 0.578400 0.67729 0.742520 0.800960 0.8443100 0.8745100

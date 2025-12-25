# %%
library(tidyverse)
library(here)
library(glue)
library(arrow)
library(duckplyr)
library(tinytable)
library(collapse)
dropbox = "~/Dropbox/Projects/UrbanWagePremium"

data <- glue("{dropbox}/data/parquet/urban_wage") |>
  arrow::open_dataset() |>
  filter(ind_main_sample == TRUE)

#' Utilities
# %%
extract_body <- function(tt) {
  tinytable:::build_tt(tt, "latex")@body
}

# %%
summ <- data |>
  mutate(
    hs_degree = as.numeric(educ >= 6),
    college_degree = as.numeric(educ >= 10),
    nonurban = 1 - urban
  ) |>
  summarize(
    .by = c(year),
    frac_urban = sum(urban * perwt, na.rm = TRUE) / sum(perwt, na.rm = TRUE),
    frac_college = sum(perwt * college_degree) / sum(perwt),
    frac_college_urban = sum(perwt * urban * college_degree) /
      sum(perwt * urban),
    frac_college_nonurban = sum(perwt * nonurban * college_degree) /
      sum(perwt * nonurban),
    mean_weeklywage = sum(perwt * weeklywage) / sum(perwt),
    mean_weeklywage_urban = sum(perwt * urban * weeklywage) /
      sum(perwt * urban),
    mean_weeklywage_nonurban = sum(perwt * nonurban * weeklywage) /
      sum(perwt * nonurban),
  ) |>
  mutate(
    premia = (mean_weeklywage_urban / mean_weeklywage_nonurban - 1)
  ) |>
  arrange(year) |>
  collect()

# %%
summ_tab <- summ |>
  select(
    year,
    premia,
    frac_urban,
    frac_college,
    frac_college_urban,
    frac_college_nonurban
  ) |>
  tt() |>
  format_tt(
    j = "frac\\_|premia",
    fn = scales::label_percent(accuracy = 0.1, scale = 100, suffix = "")
  )

print(summ_tab, "latex")

# %%
summ_tab |>
  extract_body() |>
  cat(file = here("out/tables/summary_stats/summary.tex"), sep = "\n")

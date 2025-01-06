#' This file estimates the premium seperately for large and small urban areas
# %%
library(tidyverse)
library(fixest)
library(glue)
library(arrow)
library(collapse)
library(tinytable)
# remotes::install_github("kylebutts/kfbmisc")
library(kfbmisc)

dropbox <- "~/Dropbox/UrbanWagePremium"
gh <- "~/Documents/Projects/urban-wage-premium"

# %%
data <- glue("{dropbox}/data/parquet/urban_wage") |>
  arrow::open_dataset()


#' Utilities
# %%
extract_body <- function(tt) {
  tinytable:::build_tt(tt, "latex")@body
}

# %%
premias <- map(c(1940, 2010), function(y) {
  metarea_raw_premias <- data |>
    filter(year == y) |>
    collect() |>
    feols(
      ln_weeklywage ~ 0 | metarea,
      weights = ~perwt, lean = TRUE
    ) |>
    fixef()

  metarea_raw_premias |>
    _$metarea |>
    enframe("metarea", "est_premia") |>
    mutate(est_premia = est_premia - est_premia[metarea == 0]) |>
    mutate(year = y, .before = 1)
}) |>
  list_rbind()


#' ### Top 10 by year
# %%
pop1940 <- glue(
  "{dropbox}/data/urbanareas/metarea_population_1940_1950.csv"
) |>
  read_csv(show_col_types = FALSE) |>
  mutate(metarea = as.character(metarea))

premias <- premias |>
  left_join(pop1940, by = "metarea") |>
  # Manually add metarea name:
  # https://cps.ipums.org/cps/codes/msafp_codes_sep1995apr2004.shtml
  mutate(
    metarea_name = case_when(
      metarea == 1930 ~ "Danbury, CT",
      metarea == 5190 ~ "Monmouth-Ocean, NJ",
      metarea == 5350 ~ "Nashua, NH",
      .default = metarea_name
    )
  )

top10_by_year <- premias |>
  arrange(-est_premia) |>
  slice(1:10, .by = year)


#' ### Output to Table
# %%
top10_1940 <- top10_by_year |>
  filter(year == 1940) |>
  select(metarea_name, est_premia)
top10_2010 <- top10_by_year |>
  filter(year == 2010) |>
  select(metarea_name, est_premia)

tab_top10 <- cbind(top10_1940, top10_2010) |>
  tt() |>
  format_tt(
    j = c(2, 4),
    fn = scales::label_percent(accuracy = 0.1, suffix = "\\%")
  )

print(tab_top10, "markdown")
cat(extract_body(tab_top10), sep = "\n")
cat(
  extract_body(tab_top10),
  sep = "\n",
  file = here("out/tables/summary_stats/top10_1940_2010.tex")
)
# Flint, MI & 48.7\% & San Jose-Sunnyvale-Santa Clara, CA & 63.2\% \\
# Detroit, MI & 46.2\% & Bridgeport-Stamford-Norwalk, CT & 54.7\% \\
# San Francisco, CA & 43.8\% & San Francisco-San Mateo-Redwood City,CA & 52\% \\
# Seattle-Everett, WA & 43\% & Washington-Arlington-Alexandria DC-VA & 42.9\% \\
# Washington DC, MD/VA/WV & 42.6\% & Boston-Quincy, MA & 42.7\% \\
# Lansing-East Lansing, MI & 39.2\% & Seattle-Bellevue-Everett, WA & 42.2\% \\
# Sacramento, CA & 39\% & Santa Cruz-Watsonville, CA & 36.7\% \\
# New York, NY & 38.8\% & Trenton-Ewing, NJ & 36.1\% \\
# Milwaukee-Waukesha, WI & 38.5\% & Midland, TX & 34\% \\
# Rochester, NY & 38.4\% & Baltimore-Towson, MD & 33.6\% \\

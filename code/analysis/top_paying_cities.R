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

dropbox <- "~/Dropbox/Projects/UrbanWagePremium"
gh <- "~/Documents/Projects/urban-wage-premium"

data <- glue("{dropbox}/data/parquet/urban_wage") |>
  arrow::open_dataset() |>
  filter(ind_main_sample == TRUE)

code_to_metarea <- data |>
  mutate(code = as.character(code)) |>
  select(code, metarea) |>
  unique() |>
  collect() |>
  filter(code != "0")


# %%
## Utilities
extract_body <- function(tt) {
  tinytable:::build_tt(tt, "latex")@body
}

# %%
ests <- map(c(1940, 2010), function(y) {
  metarea_raw_premias <- data |>
    filter(year == y) |>
    collect() |>
    feols(
      ln_weeklywage ~ 0 | code,
      weights = ~perwt,
      lean = TRUE
    ) |>
    fixef()

  metarea_raw_premias |>
    _$code |>
    enframe("code", "est_premia") |>
    mutate(code = str_replace(code, "code::", "")) |>
    mutate(est_premia = est_premia - est_premia[code == "0"]) |>
    mutate(year = y, .before = 1) |>
    filter(code != "0")
}) |>
  list_rbind()


### Top 15 by year ----
# %%
## Manually add metarea name:
## https://cps.ipums.org/cps/codes/msafp_codes_sep1995apr2004.shtml
premias <- ests |>
  tidylog::left_join(code_to_metarea, by = "code") |>
  unique()


top15_by_year <- premias |>
  arrange(desc(est_premia)) |>
  filter(.by = year, row_number(desc(est_premia)) <= 15)

# top15_by_year |> View()

#' ### Output to Table
# %%
top10_1940 <- top15_by_year |>
  filter(.by = year, row_number(desc(est_premia)) <= 10) |>
  filter(year == 1940) |>
  select(metarea, est_premia)
top10_2010 <- top15_by_year |>
  filter(.by = year, row_number(desc(est_premia)) <= 10) |>
  filter(year == 2010) |>
  select(metarea, est_premia)

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

top15_1940 <- top15_by_year |>
  filter(year == 1940) |>
  select(metarea, est_premia)
top15_2010 <- top15_by_year |>
  filter(year == 2010) |>
  select(metarea, est_premia)

tab_top15 <- cbind(top15_1940, top15_2010) |>
  tt() |>
  format_tt(
    j = c(2, 4),
    fn = scales::label_percent(accuracy = 0.1, suffix = "\\%")
  )

print(tab_top15, "markdown")

cat(extract_body(tab_top15), sep = "\n")
cat(
  extract_body(tab_top15),
  sep = "\n",
  file = here("out/tables/summary_stats/top15_1940_2010.tex")
)

# Flint, MI & 48.4\% & Stamford, CT & 80.5\% \\
# Detroit, MI & 45.9\% & Danbury, CT & 56.9\% \\
# San Francisco-Oakland-Vallejo, CA & 43.4\% & San Jose, CA & 51.5\% \\
# Seattle-Everett, WA & 42.6\% & Washington, DC/MD/VA & 46.5\% \\
# Washington, DC/MD/VA & 42.3\% & Monmouth-Ocean, NJ & 45.5\% \\
# Lansing-E. Lansing, MI & 38.8\% & Boston, MA/NH & 41.5\% \\
# Sacramento, CA & 38.6\% & Trenton, NJ & 41.3\% \\
# New York, NY-Northeastern NJ & 38.4\% & Bridgeport, CT & 41.2\% \\
# Milwaukee, WI & 38.1\% & San Francisco-Oakland-Vallejo, CA & 39.6\% \\
# Rochester, NY & 38.1\% & Nashua, NH & 37.7\% \\

# Los Angeles-Long Beach, CA & 37.8\% & Seattle-Everett, WA & 36.6\% \\
# Chicago, IL & 37.7\% & Dutchess Co., NY & 36.1\% \\
# Minneapolis-St. Paul, MN & 37.5\% & Ventura-Oxnard-Simi Valley, CA & 35.3\% \\
# Cleveland, OH & 37.5\% & Baltimore, MD & 34.0\% \\
# Peoria, IL & 37.4\% & Ann Arbor, MI & 32.5\% \\

## OLD:
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

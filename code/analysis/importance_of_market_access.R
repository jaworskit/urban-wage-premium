# %%
library(tidyverse)
library(fixest)
library(glue)
library(here)
library(arrow)
library(collapse)
# remotes::install_github("kylebutts/kfbmisc")
library(kfbmisc)

dropbox <- "~/Dropbox/UrbanWagePremium"
gh <- "~/Documents/Projects/urban-wage-premium"

# %%
data <- glue("{dropbox}/data/parquet/urban_wage") |>
  arrow::open_dataset()


#' ## Regression estimates of urban wage premium
# %%
setFixest_fml(
  ..individual_fe = ~ agegroup + educ + white,
  ..group_averages = ~
    # Share white
    share_white +
      # Share age groups
      share_agegroup_5 +
      share_agegroup_6 +
      share_agegroup_7 +
      share_agegroup_8 +
      share_agegroup_9 +
      share_agegroup_10 +
      share_agegroup_11 +
      share_agegroup_12 +
      # Share veteran
      share_vetstat_1 +
      # Share marital status
      share_marst_1 +
      share_marst_6 +
      share_marst_2 +
      # < HS
      I(
        share_educ_1 + share_educ_2 + share_educ_3 + share_educ_4 + share_educ_5
      ) +
      # HS
      share_educ_6 +
      # Some College
      I(share_educ_7 + share_educ_8 + share_educ_9) +
      # BA and >BA
      share_educ_10 +
      share_educ_11
)

formula_vars <- getFixest_fml() |>
  map(function(x) all.vars(xpd(rhs = x))) |>
  list_c()

included_vars <- c(
  "metarea",
  "perwt",
  "ln_weeklywage",
  "rent",
  "urban",
  formula_vars
)

get_data_y <- function(data, y) {
  data |>
    filter(year == y) |>
    select(!!included_vars) |>
    collect()
}
year_seq <- seq(1940, 2010, by = 10)

# %%
ests_with_ma <- map(year_seq, function(y) {
  data_y <- get_data_y(data, y)
  est <- feols(
    ln_weeklywage ~
      i(urban) + ln_ma_removeown + ..group_averages | ..individual_fe,
    data = data_y,
    weights = ~perwt,
    cluster = ~metarea,
    lean = TRUE
  )
  est
})
ests_without_ma <- map(year_seq, function(y) {
  data_y <- get_data_y(data, y)
  est <- feols(
    ln_weeklywage ~ i(urban) + ..group_averages | ..individual_fe,
    data = data_y,
    weights = ~perwt,
    cluster = ~metarea,
    lean = TRUE
  )
  est
})

#' ## Tables
# %%
setFixest_etable(
  dict = c(
    "ln_weeklywage" = "$\\log(\\text{Weekly Wage})$",
    "urban::1" = "$\\text{Urban} = 1$",
    "metarea" = "MSA/CBSA"
  ),
  fitstat = c("n")
)

# %%
table_with_ma <- fixest::etable(ests_with_ma, keep = "Urban", tex = TRUE)
table_without_ma <- fixest::etable(ests_without_ma, keep = "Urban", tex = TRUE)

get_urban_row <- function(tab) {
  urban_row <- grep("Urban", tab)
  tab[urban_row:(urban_row + 1)]
}

tab_combined <- c(
  "\\multicolumn{9}{l}{\\hspace{-2.5mm}{\\bfseries \\itshape Panel A: With Market Access}} \\\\",
  get_urban_row(table_with_ma),
  "\\midrule\n",
  "\n\\multicolumn{9}{l}{\\hspace{-2.5mm}{\\bfseries \\itshape Panel B: Without Market Access}} \\\\",
  get_urban_row(table_without_ma)
) |>
  stringr::str_squish()

cat(
  tab_combined,
  file = here("out/tables/urban_premium/ests_without_ma.tex"),
  sep = "\n"
)

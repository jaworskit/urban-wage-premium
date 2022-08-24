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

# Results ----------------------------------------------------------------------

## Regression Results ----------------------------------------------------------

data <- NULL
results <- NULL
ests_unadjusted <- list()
ests_controls <- list()
ests_group <- list()
year_seq <- seq(1940, 2020, 10)

# Loop through year
for (i in 1:length(year_seq)) {
  y <- year_seq[i]
  cli::cli_alert_info("Starting on year {y}")

  # Sample has every observation except 15% sample of 1940 full count
  rm(data)
  data <- glue("{project}/data/dta/urban_wage_final_{y}.dta") |>
    haven::read_dta() |>
    data.table::setDT()
  
  # Filter out highest earners
  data <- data[data$incwage != max(data$incwage), ]

  ## Urban ---------------------------------------------------------------------

  est1 <- feols(ln_weeklywage ~ i(urban),
    data = data, cluster = ~metarea, weights = ~perwt, lean = TRUE
  )

  ests_unadjusted[[i]] <- est1

  ## Urban & Individual Controls -----------------------------------------------

  est2 <- feols(ln_weeklywage ~ i(urban) | agegroup + educ + white,
    data = data, cluster = ~metarea, weights = ~perwt, lean = TRUE
  )

  ests_controls[[i]] <- est2

  ## Urban, Individual Controls & Group Averages -------------------------------

  est3 <- feols(
    ln_weeklywage ~ i(urban, ref = FALSE) + ln_ma_removeown + ..("^share_") + i(educ) + i(white) + i(agegroup),
    data = data, cluster = ~metarea, weights = ~perwt, lean = TRUE
  )

  ests_group[[i]] <- est3

  results <- bind_rows(results, tibble(
    year = rep(y, times = 3),
    est = list(est1, est2, est3) |>
      lapply(function(x) {
        coef(x)[["urban::1"]]
      }) |>
      unlist(),
    se = list(est1, est2, est3) |>
      lapply(function(x) {
        se(x)[["urban::1"]]
      }) |>
      unlist(),
    group = c("Urban Only", "Controls", "Group Averages")
  ))
}


# Tables -----------------------------------------------------------------------

table_unadjusted <- fixest::etable(
  ests_unadjusted,
  keep = "Urban",
  dict = c(
    ln_weeklywage = "$\\log(\\text{Weekly Wage})$",
    "urban::1" = "$\\text{Urban} = 1$",
    "metarea" = "MSA/CBSA"
  ),
  fitstat = c("n"),
  tex = TRUE
)

table_controls <- fixest::etable(
  ests_controls,
  keep = "Urban",
  dict = c(
    ln_weeklywage = "$\\log(\\text{Weekly Wage})$",
    "urban::1" = "$\\text{Urban} = 1$",
    "metarea" = "MSA/CBSA"
  ),
  fitstat = c("n"),
  tex = TRUE
)

table_group <- fixest::etable(
  ests_group,
  keep = "Urban",
  dict = c(
    ln_weeklywage = "$\\log(\\text{Weekly Wage})$",
    "urban::1" = "$\\text{Urban} = 1$",
    "metarea" = "MSA/CBSA"
  ),
  fitstat = c("n"),
  tex = TRUE
)

cat(
  c(
    "\\multicolumn{10}{l}{\\hspace{-2.5mm}{\\bfseries \\itshape Panel A: Raw Wage Premium}} \\\\",
    table_unadjusted[9:11], 
    "\n\\multicolumn{10}{l}{\\hspace{-2.5mm}{\\bfseries \\itshape Panel B: Individual Controls}} \\\\",
    table_controls[9:11], 
    "\n\\multicolumn{10}{l}{\\hspace{-2.5mm}{\\bfseries \\itshape Panel C: Individual Controls \\& Group Averages}} \\\\",
    table_group[9:11],
    "\n",
    table_unadjusted[13]
  ), 
  sep = "\n"
)

# Results ----------------------------------------------------------------------
# \multicolumn{10}{l}{\hspace{-2.5mm}{\bfseries \itshape Panel A: Raw Wage Premium}} \\
# $\text{Urban} = 1$  & 0.3126$^{***}$ & 0.2024$^{***}$ & 0.2219$^{***}$ & 0.2052$^{***}$ & 0.1572$^{***}$ & 0.1942$^{***}$ & 0.2019$^{***}$ & 0.1758$^{***}$ & 0.1837$^{***}$\\   
# & (0.0172)       & (0.0124)       & (0.0132)       & (0.0115)       & (0.0108)       & (0.0157)       & (0.0144)       & (0.0164)       & (0.0187)\\   
# \midrule

# \multicolumn{10}{l}{\hspace{-2.5mm}{\bfseries \itshape Panel B: Individual Controls}} \\
# $\text{Urban} = 1$  & 0.2785$^{***}$ & 0.1894$^{***}$ & 0.2002$^{***}$ & 0.1795$^{***}$ & 0.1271$^{***}$ & 0.1615$^{***}$ & 0.1572$^{***}$ & 0.1271$^{***}$ & 0.1241$^{***}$\\   
# & (0.0165)       & (0.0123)       & (0.0121)       & (0.0116)       & (0.0114)       & (0.0162)       & (0.0125)       & (0.0135)       & (0.0135)\\   
# \midrule

# \multicolumn{10}{l}{\hspace{-2.5mm}{\bfseries \itshape Panel C: Individual Controls \& Group Averages}} \\
# $\text{Urban} = 1$  & 0.1196$^{***}$ & 0.1146   & 0.0955$^{***}$ & 0.0553$^{***}$ & 0.0314$^{**}$ & 0.0485$^{***}$ & 0.0366$^{***}$ & 0.0336$^{*}$ & -0.0037\\   
# & (0.0138)       & (0.2640) & (0.0172)       & (0.0188)       & (0.0147)      & (0.0135)       & (0.0138)       & (0.0180)     & (0.1738)\\   
# \midrule


# Observations        & 2,535,954      & 67,477         & 1,357,566      & 279,306        & 1,892,573      & 2,262,035      & 2,583,057      & 530,756        & 2,757,393\\  


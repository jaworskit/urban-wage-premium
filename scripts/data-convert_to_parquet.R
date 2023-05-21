library(glue)
library(here)
library(parquetize)
library(arrow)

# Local
project <- "/Users/kylebutts/Dropbox/UrbanWagePremium"
gh <- "/Users/kylebutts/Documents/Projects/urban-wage-premium"


for (y in seq(1940, 2010, 10)) {
  cli::cli_alert_info("Starting on year {y}")
  df = haven::read_dta(
    glue("{project}/data/dta/urban_wage_final_{y}.dta")
  )
  df |>
    haven::zap_labels() |>
    haven::zap_label() |>
    arrow::write_parquet( 
      glue("{project}/data/dta/urban_wage_final_{y}.parquet")
    )
}


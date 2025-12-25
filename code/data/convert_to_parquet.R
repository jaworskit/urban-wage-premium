# %%
library(arrow)
library(tidyverse)
library(here)
library(glue)
dropbox <- "~/Dropbox/Projects/UrbanWagePremium"
source(here("code/utils/dta_to_parquet_dataset.R"))

#' This will proceed in two steps:
#' 1. First, the `dta` file will be chunked into a parquet dataset in a temporary directory
#' 2. The dataset will be opened and written
# %%
infile <- glue("{dropbox}/data/dta/urban_wage.dta")
tempdir <- glue("{dropbox}/data/temp/urban_wage")
outdir <- glue("{dropbox}/data/parquet/urban_wage")

# Time-consuming so added a toggle
if (TRUE) {
  if (fs::dir_exists(tempdir)) {
    fs::dir_delete(tempdir)
  }
  fs::dir_create(tempdir, recurse = TRUE)
  dta_to_parquet_dataset(
    file = infile,
    outfile = glue("{tempdir}/part-{{i}}.parquet")
  )

  tempdir |>
    arrow::open_dataset() |>
    mutate(msacode = if_else(code == 0, statefip * 10000, code)) |>
    arrow::write_dataset(path = outdir, partitioning = "year")

  fs::dir_delete(tempdir)
}

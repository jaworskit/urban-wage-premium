library(arrow)
library(tidyverse)
library(glue)
library(haven)

# Local
project <- "/Users/kylebutts/Dropbox/UrbanWagePremium"
gh <- "/Users/kylebutts/Documents/Projects/urban-wage-premium" 

# # Convert to arrow dataset
# glue("{project}/data/dta/urban_wage_with_ma.dta") |> 
#   haven::read_dta() |> 
#   haven::zap_labels() |>
#   group_by(year) |>
#   write_dataset(
#     glue("{project}/data/urban_wage_with_ma/")
#   )

data = glue("{project}/data/urban_wage_with_ma/") |>
  arrow::open_dataset()

data |> 
  mutate(
    msacode = ifelse(is.na(metarea), statefip, metarea),
    hs_degree = as.numeric(educ >= 6),
    college_degree = as.numeric(educ >= 10)
  ) |>
  group_by(year, msacode) |> 
  summarize(
    perwt_total = sum(perwt),
    share_race1 = sum(perwt * as.numeric(white == 1)) / sum(perwt),
    share_race0 = sum(perwt * as.numeric(white == 0)) / sum(perwt),
    share_age5 = sum(perwt * as.numeric(agegroup == 5)) / sum(perwt), 
    share_age6 = sum(perwt * as.numeric(agegroup == 6)) / sum(perwt), 
    share_age7 = sum(perwt * as.numeric(agegroup == 7)) / sum(perwt), 
    share_age8 = sum(perwt * as.numeric(agegroup == 8)) / sum(perwt), 
    share_age9 = sum(perwt * as.numeric(agegroup == 9)) / sum(perwt), 
    share_age10 = sum(perwt * as.numeric(agegroup == 10)) / sum(perwt), 
    share_age11 = sum(perwt * as.numeric(agegroup == 11)) / sum(perwt), 
    share_age12 = sum(perwt * as.numeric(agegroup == 12)) / sum(perwt), 
    share_age13 = sum(perwt * as.numeric(agegroup == 13)) / sum(perwt), 
    share_educ0 = sum(perwt * as.numeric(educ == 0)) / sum(perwt), 
    share_educ1 = sum(perwt * as.numeric(educ == 1)) / sum(perwt), 
    share_educ2 = sum(perwt * as.numeric(educ == 2)) / sum(perwt), 
    share_educ3 = sum(perwt * as.numeric(educ == 3)) / sum(perwt), 
    share_educ4 = sum(perwt * as.numeric(educ == 4)) / sum(perwt), 
    share_educ5 = sum(perwt * as.numeric(educ == 5)) / sum(perwt), 
    share_educ6 = sum(perwt * as.numeric(educ == 6)) / sum(perwt), 
    share_educ7 = sum(perwt * as.numeric(educ == 7)) / sum(perwt), 
    share_educ8 = sum(perwt * as.numeric(educ == 8)) / sum(perwt), 
    share_educ9 = sum(perwt * as.numeric(educ == 9)) / sum(perwt), 
    share_educ10 = sum(perwt * as.numeric(educ == 10)) / sum(perwt), 
    share_educ11 = sum(perwt * as.numeric(educ == 11)) / sum(perwt), 
    share_educ99 = sum(perwt * as.numeric(educ == 99)) / sum(perwt), 
    share_marst1 = sum(perwt * as.numeric(marst == 1)) / sum(perwt), 
    share_marst2 = sum(perwt * as.numeric(marst == 2)) / sum(perwt), 
    share_marst3 = sum(perwt * as.numeric(marst == 3)) / sum(perwt), 
    share_marst4 = sum(perwt * as.numeric(marst == 4)) / sum(perwt),
    share_marst5 = sum(perwt * as.numeric(marst == 5)) / sum(perwt), 
    share_marst6 = sum(perwt * as.numeric(marst == 6)) / sum(perwt), 
    share_vetstat0 = sum(perwt * as.numeric(vetstat == 0)) / sum(perwt), 
    share_vetstat1 = sum(perwt * as.numeric(vetstat == 1)) / sum(perwt), 
    share_vetstat2 = sum(perwt * as.numeric(vetstat == 2)) / sum(perwt), 
    share_vetstat9 = sum(perwt * as.numeric(vetstat == 9)) / sum(perwt)
  ) |>
  arrange(year, msacode) |>
  collect()



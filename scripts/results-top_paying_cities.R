## results-top_cities.R --------------------------------------------------------
## Kyle Butts, CU Boulder Economics
##
## This file creates a table for top 10 populous cities in 1940 and 2020

library(tidyverse)
library(data.table)
library(glue)
library(broom)
library(vroom)
library(fixest)
library(collapse)
# devtools::install_github("kylebutts/kfbmisc")
library(kfbmisc)


# Local
project <- "/Users/kylebutts/Dropbox/UrbanWagePremium"
gh <- "/Users/kylebutts/Documents/Projects/urban-wage-premium"

premias <- NULL

for (y in c(1940, 2020)) {
  cli::cli_alert_info("Starting on year {y}")

  # Sample has every observation except 15% sample of 1940 full count
  rm(data)
  data <- glue("{project}/data/dta/urban_wage_final_{y}.dta") |>
    haven::read_dta() |>
    data.table::setDT()

  premias <- rbind(premias, 
    feols(ln_weeklywage ~ 0 | metarea,
      data = data, cluster = ~metarea, weights = ~perwt
    ) |> 
      fixef() |> 
      with({
        data.table(
          metarea = metarea[order(metarea, decreasing = T)] |> 
            names(),
          est_premia = metarea[order(metarea, decreasing = T)],
          year = y
        )
      })
  )

}

premias[, 
  metarea := as.numeric(metarea)
][, 
  est_premia := est_premia - est_premia[metarea == 0], by = year
]

premias <- premias[, .SD[order(est_premia, decreasing = T), ], by = year]


## 1940 Top 10

pop1940 <- glue(
  "{project}/data/urbanareas/metarea_population_1940_1950.csv"
) |>
  fread() |>
  within({ 
    cbsa_name = metarea_name 
    metarea_name = pop_1940 = pop_1950 = NULL
  })


top10_1940 <- merge(
    premias[year == 1940, .SD[1:10]],
    pop1940,
    by = "metarea"
  )

top10_1940 <- top10_1940[order(est_premia, decreasing = TRUE), ]

top10_1940 <- top10_1940[, .(cbsa_name, est_premia = paste0(round(est_premia, 3)*100, "\\%"))]


## 2020 Top 10

cbsa2020_to_msa1950 <- glue(
  "{project}/data/urbanareas/cbsa2015_to_msa1950.csv"
) |>
  fread() |> 
  within({ metarea = NULL })

top10_2020 <- merge(
    premias[year == 2020, .SD[1:10]],
    cbsa2020_to_msa1950,
    by.x = "metarea",
    by.y = "cbsa"
  )

top10_2020 <- top10_2020[order(est_premia, decreasing = TRUE), ]

top10_2020 <- top10_2020[, .(cbsa_name, est_premia = paste0(round(est_premia, 3)*100, "\\%"))]




## ---- Output to Table --------------------------------------------------------

# https://stackoverflow.com/a/20759218
printmrow <- function(x) {
  paste(paste(x, collapse = " & "), "\\\\ \n")
}

cbind(
  as.matrix(top10_1940), 
  as.matrix(top10_2020)
) |> 
  apply(1, printmrow) |>
  # cat(file = here::here("paper/results/summary_stats/top10.tex")) |>
  cat() 







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
library(arrow)


# Local
project <- "/Users/kylebutts/Dropbox/UrbanWagePremium"
gh <- "/Users/kylebutts/Documents/Projects/urban-wage-premium"

premias <- NULL

for (y in c(1940, 2020)) {
  cli::cli_alert_info("Starting on year {y}")

  # Sample has every observation except 15% sample of 1940 full count
  rm(data)
  data <- glue("{project}/data/dta/urban_wage_final_{y}.parquet") |>
    arrow::read_parquet() |>
    collect()

  data <- data |> haven::zap_labels() |> as.data.table()

  premias <- rbind(premias, 
    feols(
      ln_weeklywage ~ 0 | metarea,
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

top10_1940 <- top10_1940[, .(cbsa_name, est_premia = paste0(round(est_premia, 3) * 100, "\\%"))]


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

top10_2020 <- top10_2020[, .(cbsa_name, est_premia = paste0(round(est_premia, 3) * 100, "\\%"))]




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
  # cat(file = here::here("paper/tables/summary_stats/top10.tex")) |>
  cat() 

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




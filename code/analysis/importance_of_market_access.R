# %%
library(tidyverse)
library(fixest)
library(glue)
library(here)
library(arrow)
library(collapse)
# remotes::install_github("kylebutts/kfbmisc")
library(kfbmisc)

dropbox <- "~/Dropbox/Projects/UrbanWagePremium"
source(here("code/utils/calculate_group_averages.R"))

options(pillar.print_max = 30)

# %%
data <- glue("{dropbox}/data/parquet/urban_wage") |>
  arrow::open_dataset() |>
  filter(ind_main_sample == TRUE) |>
  filter(weeks >= 48) |>
  filter(!is.na(perwt), perwt > 0)

#' ## Replicating Bouston et al.
# %%
# df0 <- data %>%
#   collapse::collap(totalincome + perwt ~ year + urban, fsum) %>%
#   mutate(weeklywage = totalincome / perwt) %>%
#   pivot_wider(id_cols = c("year"), values_from = weeklywage, names_from = urban, names_prefix = "urban_") %>%
#   mutate(est = log(urban_1 / urban_0), group = "Raw") %>%
#   select(year, est, group)
#
# # Store results
# results <- df0

#' ## Regression estimates of urban wage premium
# %%
group_averages <- c(
  "share_nonwhite",
  "share_agegroup_5",
  "share_agegroup_6",
  "share_agegroup_7",
  "share_agegroup_8",
  "share_agegroup_9",
  "share_agegroup_10",
  "share_agegroup_11",
  "share_agegroup_12",
  "share_vetstat_1",
  "share_marst_1",
  "share_marst_2",
  "share_marst_6",
  "share_educ_lt_hs",
  "share_educ_hs",
  "share_educ_some_college",
  "share_educ_college_plus"
)
individual_dummy_vars <- c("agegroup", "educ", "white")
included_vars <- c(
  "msacode",
  "metarea",
  "perwt",
  "ln_weeklywage",
  "urban",
  individual_dummy_vars,
  "ln_ma_removeown",
  group_averages
)
setFixest_fml(
  ..individual_controls = ~ I(marst == 1) +
    I(marst == 2) +
    I(marst == 6) +
    I(vetstat == 1),
  ..individual_fe = reformulate(individual_dummy_vars),
  ..group_averages = reformulate(group_averages)
)

year_seq <- seq(1940, 2010, by = 10)

# %%
ests <- map(year_seq, function(y) {
  cat(sprintf("On year: %s", y), "\n")

  data_y <- data |>
    filter(year == y) |>
    # 1970 does not have vet stat
    mutate(vetstat = if_else(year == 1970 & is.na(vetstat), 0, vetstat)) |>
    collect()

  data_y <- data_y |>
    calculate_group_averages(by = "msacode")

  cat("  -> est_with_ma\n")
  est_with_ma <- feols(
    ln_weeklywage ~
      i(urban) +
      ..individual_controls +
      ln_ma_removeown +
      ..group_averages |
      ..individual_fe,
    data = data_y,
    weights = ~perwt,
    cluster = ~metarea,
    lean = TRUE,
    notes = FALSE,
    warn = FALSE
  )

  cat("  -> est_without_ma\n")
  est_without_ma <- feols(
    ln_weeklywage ~
      i(urban) +
      ..individual_controls +
      ..group_averages |
      ..individual_fe,
    data = data_y,
    weights = ~perwt,
    cluster = ~metarea,
    lean = TRUE,
    notes = FALSE,
    warn = FALSE
  )

  rm(data_y)

  list(
    est_with_ma = est_with_ma,
    est_without_ma = est_without_ma
  )
})

# %%
ests_with_ma <- map(ests, ~ .x$est_with_ma)
ests_without_ma <- map(ests, ~ .x$est_without_ma)

tidy_urban_ests <- function(x) {
  x |>
    broom::tidy() |>
    filter(term == "urban::1") |>
    select(est = estimate, se = std.error) |>
    mutate(n_obs = x$nobs)
}

ests <- imap(year_seq, function(y, i) {
  bind_rows(
    tidy_urban_ests(ests_with_ma[[i]]) |>
      mutate(group = "With MA", year = y, .before = 1),
    tidy_urban_ests(ests_without_ma[[i]]) |>
      mutate(group = "Without MA", year = y, .before = 1)
  )
}) |>
  list_rbind() |>
  mutate(
    group = factor(
      group,
      levels = c("With MA", "Without MA")
    )
  ) |>
  mutate(
    est_lower90 = est - 1.65 * se,
    est_upper90 = est + 1.65 * se,
    est_lower95 = est - 1.96 * se,
    est_upper95 = est + 1.96 * se,
    exp_est = exp(est) - 1,
    exp_est_lower90 = exp(est_lower90) - 1,
    exp_est_upper90 = exp(est_upper90) - 1,
    exp_est_lower95 = exp(est_lower95) - 1,
    exp_est_upper95 = exp(est_upper95) - 1
  )

# %%
table_with_ma <- fixest::etable(ests_with_ma, keep = "urban", tex = TRUE)
table_without_ma <- fixest::etable(ests_without_ma, keep = "urban", tex = TRUE)

get_urban_row <- function(tab) {
  urban_row <- grep("urban", tab)
  row <- tab[urban_row:(urban_row + 1)]
  row <- gsub("urban", "Urban", row)
  return(row)
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

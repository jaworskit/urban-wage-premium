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
source("code/utils/calculate_group_averages.R")

options(pillar.print_max = 30)

# %%
data <- glue("{dropbox}/data/parquet/urban_wage") |>
  arrow::open_dataset() |>
  filter(ind_main_sample == TRUE) |>
  filter(!is.na(perwt), perwt > 0)

diamond_amenities <-
  glue("{dropbox}/data/diamond/gmm_estimation_data.dta") |>
  haven::read_dta() |>
  haven::zap_label() |>
  haven::zap_labels() |>
  haven::zap_formats()

diamond_amenities <- diamond_amenities |>
  # 1980, 1990, 2000
  mutate(year = 1970 + 10 * year) |>
  filter(year == 2000) |>
  mutate(id = id - 3) |>
  rename(code = id) |>
  filter(bp == 3)

amenities <- c(
  "ln_col_ratio",
  "lnstudent_teacher_ratio",
  "lnstudent_spending",
  "lnest_pc5600",
  "lnest_pc5800",
  "lnest_pc7830",
  "lnprop_rate",
  "lnviolent_rate",
  "lnaadt_IH",
  "lnaadt_MRU",
  "lnbus_pc",
  "lntransit_pc",
  "lnmedian_aqi",
  "lnparks_spending",
  "lnunemp_rate",
  "lnpatent_pc",
  "WRLURI_msa",
  "unaval_msa"
)

diamond_amenities <- diamond_amenities |>
  select(code, all_of(amenities))

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
data_y <- data |>
  filter(year == 2000) |>
  collect() |>
  calculate_group_averages(by = "msacode") |>
  slice_head(by = msacode, n = 1)

merged <- data_y |>
  select(code, all_of(group_averages)) |>
  filter(code != 0) |>
  tidylog::inner_join(diamond_amenities, by = "code") |>
  drop_na()

# %%
library(fixest)
ests <- feols(
  xpd(
    lhs = paste0("c(", paste0(amenities, collapse = ", "), ")"),
    rhs = group_averages
  ),
  data = merged,
  vcov = "hc1"
)

betas <- as.matrix(coef(ests)[, 3:ncol(coef(ests))])

etable(ests)

# %%
# row-rank of \betas shows that all the amenities load onto (at least some) of the group-averages
msg <- sprintf(
  "There are %s group averages that we use. The row-rank of the betas is %s.",
  ncol(betas),
  qr(betas)$rank
)
cat(msg)
cat(msg, file = here("out/statements/group_averages_on_diamond_amenities.txt"))

# %%
missing_in_diamond <- data_y |>
  select(code, all_of(group_averages)) |>
  filter(code != 0) |>
  anti_join(
    diamond_amenities,
    by = "code"
  )

# We match all but Hawaii, Alaska, and `Hamilton-Middletown, OH` which is part of our `Cincinatti-Hamilton, OH/KY/IN`
missing_in_ours <- data_y |>
  select(code, all_of(group_averages)) |>
  filter(code != 0) |>
  anti_join(
    diamond_amenities,
    y = _,
    by = "code"
  )
missing_in_ours |> pull(code)

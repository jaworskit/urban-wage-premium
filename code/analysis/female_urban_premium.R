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
  filter(sex == 2 & ind1950 != 1) |> ## Female main sample
  filter(!is.na(perwt), perwt > 0)

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

  cat("  -> est_unadjusted\n")
  est_unadjusted <- feols(
    ln_weeklywage ~ i(urban),
    data = data_y,
    weights = ~perwt,
    cluster = ~metarea,
    lean = TRUE,
    notes = FALSE,
    warn = FALSE
  )
  cat("  -> est_controls\n")
  est_controls <- feols(
    ln_weeklywage ~ i(urban) + ..individual_controls | ..individual_fe,
    data = data_y,
    weights = ~perwt,
    cluster = ~metarea,
    lean = TRUE,
    notes = FALSE,
    warn = FALSE
  )
  cat("  -> est_group\n")
  est_group <- feols(
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

  list(
    est_unadjusted = est_unadjusted,
    est_controls = est_controls,
    est_group = est_group
  )
})

# %%
ests_unadjusted <- map(ests, ~ .x$est_unadjusted)
ests_controls <- map(ests, ~ .x$est_controls)
ests_group <- map(ests, ~ .x$est_group)

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

table_unadjusted <- fixest::etable(
  ests_unadjusted,
  keep = "Urban",
  tex = TRUE
)
table_controls <- fixest::etable(
  ests_controls,
  keep = "Urban",
  tex = TRUE
)
table_group <- fixest::etable(
  ests_group,
  keep = "Urban",
  tex = TRUE
)

get_urban_row <- function(tab) {
  urban_row <- grep("Urban", tab)
  tab[urban_row:(urban_row + 1)]
}

tab_combined <- c(
  "\\multicolumn{9}{l}{\\hspace{-2.5mm}{\\bfseries \\itshape Panel A: Raw Wage Premium}} \\\\",
  get_urban_row(table_unadjusted),
  "\\midrule\n",
  "\n\\multicolumn{9}{l}{\\hspace{-2.5mm}{\\bfseries \\itshape Panel B: Individual Controls}} \\\\",
  get_urban_row(table_controls),
  "\\midrule\n",
  "\n\\multicolumn{9}{l}{\\hspace{-2.5mm}{\\bfseries \\itshape Panel C: Individual Controls \\& Group Averages}} \\\\",
  get_urban_row(table_group),
  "\\midrule\n",
  table_unadjusted[13]
) |>
  stringr::str_squish()

cat(tab_combined, sep = "\n")

cat(
  tab_combined,
  sep = "\n",
  file = here("out/tables/urban_premium/female_ests.tex")
)

#' ## Plot Point Estimates
# %%
tidy_urban_ests <- function(x) {
  x |>
    broom::tidy() |>
    filter(term == "urban::1") |>
    select(est = estimate, se = std.error) |>
    mutate(n_obs = x$nobs)
}

ests <- imap(year_seq, function(y, i) {
  bind_rows(
    tidy_urban_ests(ests_unadjusted[[i]]) |>
      mutate(group = "Urban Only", year = y, .before = 1),
    tidy_urban_ests(ests_controls[[i]]) |>
      mutate(group = "Individual Controls", year = y, .before = 1),
    tidy_urban_ests(ests_group[[i]]) |>
      mutate(group = "Group Averages", year = y, .before = 1)
  )
}) |>
  list_rbind() |>
  mutate(
    group = factor(
      group,
      levels = c("Raw", "Urban Only", "Individual Controls", "Group Averages")
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
plot_ests <- function(ests, which_groups, base_size = 16) {
  stopifnot(is.character(which_groups))
  plot_guides <- if (length(which_groups) > 1) {
    list(
      guides(
        color = guide_legend(
          title.position = "top",
          nrow = 1,
          override.aes = list(linetype = 0)
        )
      )
    )
  } else {
    list(
      guides(color = "none", shape = "none")
    )
  }

  ggplot(
    ests |>
      filter(group %in% .env$which_groups),
    aes(
      x = year,
      y = exp_est,
      group = group,
      color = group,
      ymin = exp_est_lower95,
      ymax = exp_est_upper95
    )
  ) +
    geom_line(linewidth = 2) +
    geom_errorbar(
      aes(ymin = exp_est_lower95, ymax = exp_est_upper95),
      linewidth = 1.5,
      width = 1
    ) +
    geom_point(aes(shape = group), size = 3) +
    labs(
      x = NULL,
      y = "Urban Wage Premium",
      group = NULL,
      shape = NULL,
      color = NULL
    ) +
    scale_y_continuous(
      labels = scales::label_percent(suffix = "\\%"),
      limits = c(-0.05, 0.54),
      expand = c(0, 0)
    ) +
    scale_x_continuous(breaks = seq(1940, 2010, by = 10)) +
    scale_color_manual(
      values = c(
        "Urban Only" = "grey70",
        "Individual Controls" = "grey40",
        "Group Averages" = "grey10"
      )
    ) +
    scale_shape_manual(
      values = c(
        "Urban Only" = 15,
        "Individual Controls" = 16,
        "Group Averages" = 18
      )
    ) +
    plot_guides +
    kfbmisc::theme_kyle(base_size = base_size) +
    theme(
      axis.title.y = element_text(size = rel(0.8)),
      legend.position = "inside",
      legend.position.inside = c(0.5, 0.88),
      legend.background = element_rect(fill = "white", color = "gray20"),
      legend.margin = margin(4, 6, 6, 6),
      panel.grid.minor.x = element_blank(),
      axis.line.y = element_blank(),
      axis.ticks.y = element_blank(),
      axis.line.x = element_blank(),
      axis.ticks.x = element_blank()
    )
}

# %%
(urban <- ests |>
  plot_ests("Urban Only"))
(controls <- ests |>
  plot_ests(c("Urban Only", "Individual Controls")))
(causal <- ests |>
  plot_ests("Group Averages"))
(controls_and_causal <- ests |>
  plot_ests(c("Individual Controls", "Group Averages")))
(combined <- ests |>
  plot_ests(c("Urban Only", "Individual Controls", "Group Averages")))

# %%
fs::dir_create(here("out/figures/female_urban_premium/"))
kfbmisc::tikzsave(
  here("out/figures/female_urban_premium/raw.pdf"),
  urban,
  width = 11,
  height = 5
)
# kfbmisc::tikzsave(
#   here("out/figures/slides/raw_premium.pdf"),
#   urban,
#   width = 11,
#   height = 3.5
# )
kfbmisc::tikzsave(
  here("out/figures/female_urban_premium/controls.pdf"),
  controls,
  width = 11,
  height = 5
)
kfbmisc::tikzsave(
  here("out/figures/female_urban_premium/causal.pdf"),
  causal,
  width = 11,
  height = 5
)
kfbmisc::tikzsave(
  here("out/figures/female_urban_premium/controls_and_causal.pdf"),
  controls_and_causal,
  width = 11,
  height = 5
)
kfbmisc::tikzsave(
  here("out/figures/female_urban_premium/combined.pdf"),
  combined,
  width = 11,
  height = 5
)

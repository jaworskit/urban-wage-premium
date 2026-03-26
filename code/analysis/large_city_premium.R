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

data <- glue("{dropbox}/data/parquet/urban_wage") |>
  arrow::open_dataset() |>
  filter(ind_main_sample == TRUE) |>
  filter(!is.na(perwt), perwt > 0)

msa_pop_long <- read_csv(glue(
  "{dropbox}/data/urbanareas/metarea_population_1940_2010.csv"
)) |>
  mutate(
    pop_gt_1million = +(pop >= 1000000),
    pop_gt_1million = replace_na(pop_gt_1million, 0),
    pop_gt_1.5million = +(pop >= 1500000),
    pop_gt_1.5million = replace_na(pop_gt_1.5million, 0)
  )

## Regression estimates of urban wage premium ----
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
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

# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
ests <- map(year_seq, function(y) {
  cat(sprintf("On year: %s", y), "\n")

  data_y <- data |>
    filter(year == y) |>
    # 1970 does not have vet stat
    mutate(vetstat = if_else(year == 1970 & is.na(vetstat), 0, vetstat)) |>
    collect()

  data_y <- data_y |>
    calculate_group_averages(by = "msacode")

  data_y <- data_y |>
    left_join(msa_pop_long |> select(-metarea), by = c("code", "year"))

  cat("  -> est_group\n")
  est_group <- feols(
    ln_weeklywage ~
      i(urban, i.pop_gt_1million) +
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

  est_group
})


## Plot Point Estimates ----
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
coef <- ests |>
  imap(function(x, i) {
    curr_year <- year_seq[[i]]
    x |>
      broom::tidy() |>
      select(term, est = estimate, se = std.error) |>
      filter(str_detect(term, "urban")) |>
      mutate(
        term = case_when(
          term == "urban::1:pop_gt_1million::0" ~ "MSA < 1 Million",
          term == "urban::1:pop_gt_1million::1" ~ "MSA >= 1 Million"
        ),
        group = term,
        n_obs = x$nobs,
        year = year_seq[i]
      )
  }) |>
  list_rbind() |>
  mutate(
    group = factor(group, levels = c("MSA < 1 Million", "MSA >= 1 Million"))
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


(plot_big_vs_small_cities <- ggplot(
  coef,
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
  # geom_errorbar(
  #   aes(ymin = exp_est_lower95, ymax = exp_est_upper95),
  #   linewidth = 1.5,
  #   width = 1
  # ) +
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
    limits = c(-0.06, 0.55),
    expand = c(0, 0, 0, 0)
  ) +
  scale_x_continuous(breaks = seq(1940, 2010, by = 10)) +
  scale_color_manual(
    values = c(
      "MSA >= 1 Million" = "grey10",
      "MSA < 1 Million" = "grey40"
    )
  ) +
  scale_shape_manual(
    values = c(
      "MSA >= 1 Million" = 15,
      "MSA < 1 Million" = 16
    )
  ) +
  kfbmisc::theme_kyle(base_size = 16) +
  guides(
    colour = guide_legend(
      title.position = "top",
      nrow = 1,
      override.aes = list(linetype = 0)
    )
  ) +
  theme(
    legend.position = "inside",
    legend.position.inside = c(0.5, 0.88),
    legend.background = element_rect(fill = "white", color = "gray20"),
    legend.margin = margin(4, 6, 6, 6),
    panel.grid.minor.x = element_blank(),
    axis.line.y = element_blank(),
    axis.ticks.y = element_blank(),
    axis.line.x = element_blank(),
    axis.ticks.x = element_blank()
  ))

kfbmisc::tikzsave(
  here("out/figures/heterogeneity/big_vs_small_cities.pdf"),
  plot_big_vs_small_cities,
  width = 14,
  height = 4
)

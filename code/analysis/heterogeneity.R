#' This file estimates the premium seperately for large and small urban areas
# %%
library(tidyverse)
library(fixest)
library(glue)
library(here)
library(arrow)
library(collapse)
library(broom)
# remotes::install_github("kylebutts/kfbmisc")
library(kfbmisc)

dropbox <- "~/Dropbox/UrbanWagePremium"
source("code/utils/calculate_group_averages.R")

# Flags:
run_estimators <- TRUE

# %%
data <- glue("{dropbox}/data/parquet/urban_wage") |>
  arrow::open_dataset() |>
  mutate(msacode = if_else(code == 0, statefip * 10000, code))
# |>
# filter(!is.na(perwt), perwt > 0)

pop_1940 <- glue(
  "{dropbox}/data/urbanareas/metarea_population_1940_1950.csv"
) |>
  read_csv(show_col_types = FALSE) |>
  arrange(desc(pop_1940)) |>
  mutate(pop_rank = row_number(desc(pop_1940)))

codes_1940_msas <- pop_1940$metarea
codes_top_20 <- pop_1940 |>
  filter(pop_rank <= 20) |>
  pull(metarea)

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
  ..individual_fe = reformulate(individual_dummy_vars),
  ..group_averages = reformulate(group_averages),
  ..by_college_degree_group_averages = reformulate(paste0(
    "by_college_degree_",
    group_averages
  )),
  ..by_race_group_averages = reformulate(paste0("by_race_", group_averages)),
  ..by_older_group_averages = reformulate(paste0("by_older_", group_averages))
)

year_seq <- seq(1940, 2010, by = 10)

# %%
est_1940_msas <- function(data_y) {
  y <- data_y$year[1]

  est <- data_y |>
    filter(is_1940_msa == TRUE | code == 0) |>
    feols(
      ln_weeklywage ~ i(urban) + ..group_averages | ..individual_fe,
      weights = ~perwt,
      cluster = ~metarea,
      lean = TRUE,
      notes = FALSE
    )

  broom::tidy(est) |>
    filter(str_detect(term, "^urban")) |>
    mutate(term = str_replace(term, "urban::1", "Urban")) |>
    select(term, est = estimate, se = std.error) |>
    mutate(year = y, .before = 1)
}

# %%
est_top20 <- function(data_y) {
  y <- data_y$year[1]

  data_y |>
    feols(
      ln_weeklywage ~
        i(urban, i.top20, ref = "0") + ..group_averages | ..individual_fe,
      weights = ~perwt,
      cluster = ~metarea,
      lean = TRUE,
      notes = FALSE
    ) |>
    broom::tidy() |>
    filter(str_detect(term, "^urban")) |>
    mutate(term = str_replace(term, "urban::1:top20::", "")) |>
    select(term, est = estimate, se = std.error) |>
    mutate(term = if_else(term == "TRUE", "Top 20", "Other Urban")) |>
    mutate(year = y, .before = 1)
}

# %%
# Region-specific estimates
est_region <- function(data_y) {
  y <- data_y$year[1]

  data_y |>
    feols(
      ln_weeklywage ~
        i(urban, i.census_region, ref = "0") +
          ..group_averages |
          ..individual_fe,
      weights = ~perwt,
      cluster = ~metarea,
      lean = TRUE,
      notes = FALSE
    ) |>
    broom::tidy() |>
    filter(str_detect(term, "^urban")) |>
    mutate(term = str_replace(term, "urban::1:census_region::", "")) |>
    select(term, est = estimate, se = std.error) |>
    mutate(year = y, .before = 1)
}

# %%
# College vs. No-college
est_by_college <- function(data_y) {
  y <- data_y$year[1]

  est <- data_y |>
    feols(
      ln_weeklywage ~
        i(urban, ref = 0) +
          ln_ma_removeown +
          ..by_college_degree_group_averages |
          agegroup + educ + white,
      weights = ~perwt,
      split = ~college_degree,
      cluster = ~metarea,
      lean = TRUE,
      notes = FALSE
    )

  unname(est) |>
    imap(function(x, i) {
      college <- as.numeric(models(est)$sample[i])
      df <- broom::tidy(est[[i]])
      df |>
        filter(str_detect(term, "^urban")) |>
        mutate(term = if_else(college == 1, "College", "No College")) |>
        select(term, est = estimate, se = std.error) |>
        mutate(year = y, .before = 1)
    }) |>
    list_rbind()
}

# %%
# White vs. Black estimates
est_by_race <- function(data_y) {
  y <- data_y$year[1]

  est <- data_y |>
    filter(white == TRUE | black == TRUE) |>
    feols(
      ln_weeklywage ~
        i(urban, ref = 0) +
          ln_ma_removeown +
          ..by_race_group_averages |
          agegroup + educ + white,
      weights = ~perwt,
      split = ~race,
      cluster = ~metarea,
      lean = TRUE,
      notes = FALSE
    )

  unname(est) |>
    imap(function(x, i) {
      race <- models(est)$sample[i]
      df <- broom::tidy(est[[i]])
      df |>
        filter(str_detect(term, "^urban")) |>
        mutate(term = .env$race) |>
        select(term, est = estimate, se = std.error) |>
        mutate(year = y, .before = 1)
    }) |>
    list_rbind()
}

# Region x Race
est_by_race_and_region <- function(data_y) {
  y <- data_y$year[1]

  est <- data_y |>
    filter(white == TRUE | black == TRUE) |>
    feols(
      ln_weeklywage ~
        i(urban, i.census_region, ref = "0") +
          ln_ma_removeown +
          ..by_race_group_averages |
          agegroup + educ + white,
      weights = ~perwt,
      split = ~race,
      cluster = ~metarea,
      lean = TRUE,
      notes = FALSE
    )

  unname(est) |>
    imap(function(x, i) {
      race <- models(est)$sample[i]
      df <- broom::tidy(est[[i]])
      df |>
        filter(str_detect(term, "^urban")) |>
        mutate(
          term = str_replace(term, "urban::(.*):census_region::", ""),
          region = term,
          race = .env$race,
          group = fct(paste0(race, " x ", region))
        ) |>
        select(race, region, group, est = estimate, se = std.error) |>
        mutate(year = y, .before = 1)
    }) |>
    list_rbind()
}

# %%
# Ages above and below 40
est_by_older <- function(data_y) {
  y <- data_y$year[1]

  est <- data_y |>
    feols(
      ln_weeklywage ~
        i(urban, ref = 0) +
          ln_ma_removeown +
          ..by_older_group_averages |
          agegroup + educ + white,
      weights = ~perwt,
      split = ~older,
      cluster = ~metarea,
      lean = TRUE,
      notes = FALSE
    )

  unname(est) |>
    imap(function(x, i) {
      older <- as.logical(as.numeric(models(est)$sample[i]))
      df <- broom::tidy(est[[i]])
      df |>
        filter(str_detect(term, "^urban")) |>
        mutate(term = if_else(older == TRUE, "Older", "Younger")) |>
        select(term, est = estimate, se = std.error) |>
        mutate(year = y, .before = 1)
    }) |>
    list_rbind()
}

# %%
if (run_estimators) {
  ests <- map(year_seq, function(y) {
    cat(sprintf("On year: %s", y), "\n")

    data_y <- data |>
      filter(year == y) |>
      # 1970 does not have vet stat
      mutate(vetstat = if_else(year == 1970 & is.na(vetstat), 0, vetstat)) |>
      mutate(
        is_1940_msa = code %in% codes_1940_msas,
        top20 = code %in% codes_top_20
      ) |>
      mutate(
        race = case_when(
          white == 1 ~ "White",
          black == 1 ~ "Black",
          .default = "Other Race"
        )
      ) |>
      collect()

    data_y <- data_y |>
      calculate_group_averages(by = "msacode") |>
      calculate_group_averages(by = c("msacode", "college_degree")) |>
      calculate_group_averages(by = c("msacode", "race")) |>
      calculate_group_averages(by = c("msacode", "older"))

    cat("  -> est_1940_msas\n")
    est_1940_msas <- est_1940_msas(data_y)
    cat("  -> est_top20\n")
    est_top20 <- est_top20(data_y)
    cat("  -> est_region\n")
    est_region <- est_region(data_y)
    cat("  -> est_by_college\n")
    est_by_college <- est_by_college(data_y)
    cat("  -> est_by_race\n")
    est_by_race <- est_by_race(data_y)
    cat("  -> est_by_race_and_region\n")
    est_by_race_and_region <- est_by_race_and_region(data_y)
    cat("  -> est_by_older\n")
    est_by_older <- est_by_older(data_y)

    list(
      est_1940_msas = est_1940_msas,
      est_top20 = est_top20,
      est_region = est_region,
      est_by_college = est_by_college,
      est_by_race = est_by_race,
      est_by_race_and_region = est_by_race_and_region,
      est_by_older = est_by_older
    )
  })

  saveRDS(ests, file = here("data/estimates-heterogeneity.rds"))
}

# %%
ests <- readRDS(here("data/estimates-heterogeneity.rds"))

# %%
process_ests <- function(ests) {
  ests |>
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
}

# %%
ests_1940_msas <- map(ests, ~ .x$est_1940_msas) |>
  list_rbind() |>
  process_ests()

ests_top20 <- map(ests, ~ .x$est_top20) |>
  list_rbind() |>
  process_ests() |>
  mutate(group = fct(term, c("Top 20", "Other Urban")))

ests_region <- map(ests, ~ .x$est_region) |>
  list_rbind() |>
  process_ests() |>
  mutate(
    group = fct(term, c("North", "West", "South", "Midwest"))
  )

ests_by_college <- map(ests, ~ .x$est_by_college) |>
  list_rbind() |>
  process_ests() |>
  mutate(
    group = fct(term, c("College", "No College"))
  )

ests_by_race <- map(ests, ~ .x$est_by_race) |>
  list_rbind() |>
  process_ests() |>
  mutate(
    group = fct(term, c("White", "Black"))
  )

ests_by_race_and_region <- map(ests, ~ .x$est_by_race_and_region) |>
  list_rbind() |>
  process_ests()

ests_by_older <- map(ests, ~ .x$est_by_older) |>
  list_rbind() |>
  process_ests() |>
  mutate(
    group = fct(term, c("Older", "Younger"))
  )

#' ## Plot point estimates
# %%
basic_heterogeneity_plot <- function(ests) {
  ggplot(
    ests,
    aes(x = year, y = exp_est, group = group, color = group, shape = group)
  ) +
    geom_line(linewidth = 2) +
    geom_point(size = 5) +
    labs(
      x = NULL,
      y = "Urban Wage Premium",
      group = NULL,
      shape = NULL,
      color = NULL
    ) +
    scale_y_continuous(
      labels = scales::label_percent(suffix = "\\%"),
      limits = c(-0.05, 0.52),
      expand = c(0, 0, 0, 0)
    ) +
    scale_x_continuous(breaks = seq(1940, 2020, by = 10)) +
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
    )
}

# %%
(plot_top20 <- basic_heterogeneity_plot(ests_top20) +
  scale_color_manual(
    values = c(
      "Top 20" = "grey10",
      "Other Urban" = "grey40"
    )
  ) +
  scale_shape_manual(
    values = c(
      "Top 20" = 15,
      "Other Urban" = 16
    )
  ))

# %%
(plot_region <- basic_heterogeneity_plot(ests_region) +
  scale_color_manual(
    values = c(
      "North" = ggsci::pal_jama("default")(4)[1],
      "South" = ggsci::pal_jama("default")(4)[2],
      "West" = ggsci::pal_jama("default")(4)[3],
      "Midwest" = ggsci::pal_jama("default")(4)[4]
    )
  ) +
  scale_shape_manual(
    values = c(
      "North" = 15,
      "South" = 16,
      "West" = 17,
      "Midwest" = 18
    )
  ))

# %%
(plot_college <- basic_heterogeneity_plot(ests_by_college) +
  scale_color_manual(
    values = c(
      "College" = "grey10",
      "No College" = "grey40"
    )
  ) +
  scale_shape_manual(
    values = c(
      "College" = 15,
      "No College" = 16
    )
  ))

# %%
(plot_race <- basic_heterogeneity_plot(ests_by_race) +
  scale_color_manual(
    values = c(
      "White" = "grey10",
      "Black" = "grey40"
    )
  ) +
  scale_shape_manual(
    values = c(
      "White" = 15,
      "Black" = 16
    )
  ))

# %%
(plot_older <- basic_heterogeneity_plot(ests_by_older) +
  scale_color_manual(
    values = c(
      "Older" = "grey10",
      "Younger" = "grey40"
    )
  ) +
  scale_shape_manual(
    values = c(
      "Older" = 15,
      "Younger" = 16
    )
  ))

# %%
(plot_region_white <- ests_by_race_and_region |>
  mutate(group = region) |>
  filter(race == "White") |>
  basic_heterogeneity_plot() +
  scale_color_manual(
    values = c(
      "North" = ggsci::pal_jama("default")(4)[1],
      "South" = ggsci::pal_jama("default")(4)[2],
      "West" = ggsci::pal_jama("default")(4)[3],
      "Midwest" = ggsci::pal_jama("default")(4)[4]
    )
  ) +
  scale_shape_manual(
    values = c(
      "North" = 15,
      "South" = 16,
      "West" = 17,
      "Midwest" = 18
    )
  ))
(plot_region_black <- ests_by_race_and_region |>
  mutate(group = region) |>
  filter(race == "Black") |>
  basic_heterogeneity_plot() +
  scale_color_manual(
    values = c(
      "North" = ggsci::pal_jama("default")(4)[1],
      "South" = ggsci::pal_jama("default")(4)[2],
      "West" = ggsci::pal_jama("default")(4)[3],
      "Midwest" = ggsci::pal_jama("default")(4)[4]
    )
  ) +
  scale_shape_manual(
    values = c(
      "North" = 15,
      "South" = 16,
      "West" = 17,
      "Midwest" = 18
    )
  ))

# %%
kfbmisc::tikzsave(
  here("out/figures/heterogeneity/by_top20.pdf"),
  plot_top20,
  width = 14,
  height = 4
)
kfbmisc::tikzsave(
  here("out/figures/heterogeneity/by_region.pdf"),
  plot_region,
  width = 14,
  height = 4
)
kfbmisc::tikzsave(
  here("out/figures/heterogeneity/by_college.pdf"),
  plot_college,
  width = 14,
  height = 4
)
kfbmisc::tikzsave(
  here("out/figures/heterogeneity/by_race.pdf"),
  plot_race,
  width = 14,
  height = 4
)
kfbmisc::tikzsave(
  here("out/figures/heterogeneity/by_older.pdf"),
  plot_older,
  width = 14,
  height = 4
)
kfbmisc::tikzsave(
  here("out/figures/heterogeneity/by_region_white.pdf"),
  plot_region_white,
  width = 14,
  height = 4
)
kfbmisc::tikzsave(
  here("out/figures/heterogeneity/by_region_black.pdf"),
  plot_region_black,
  width = 14,
  height = 4
)

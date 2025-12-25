# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
library(tidyverse)
library(fixest)
library(glue)
library(here)
library(arrow)
library(collapse)
library(broom)
# remotes::install_github("kylebutts/kfbmisc")
library(kfbmisc)

dropbox <- "~/Dropbox/Projects/UrbanWagePremium"
source(here("code/utils/calculate_group_averages.R"))
source(here("code/utils/basic_heterogeneity_table.R"))

## Flags:
run_estimators <- TRUE

## Load data
data <- glue("{dropbox}/data/parquet/urban_wage") |>
  arrow::open_dataset() |>
  filter(ind_main_sample == TRUE) |>
  mutate(msacode = if_else(code == 0, statefip * 10000, code))
# |>
# filter(!is.na(perwt), perwt > 0)

msa_pop_long <- read_csv(glue(
  "{dropbox}/data/urbanareas/metarea_population_1940_2010.csv"
))

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

### Specific heterogeneity estimators ----
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
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

est_top20_separately <- function(data_y) {
  y <- data_y$year[1]

  est1 <- data_y |>
    filter(
      code == 0 | top20 == TRUE
    ) |>
    feols(
      ln_weeklywage ~
        i(urban, ref = 0) +
        ln_ma_removeown +
        ..group_averages |
        agegroup + educ + white,
      weights = ~perwt,
      cluster = ~metarea,
      lean = TRUE,
      notes = FALSE
    )

  est2 <- data_y |>
    filter(
      code == 0 | top20 == FALSE
    ) |>
    feols(
      ln_weeklywage ~
        i(urban, ref = 0) +
        ln_ma_removeown +
        ..group_averages |
        agegroup + educ + white,
      weights = ~perwt,
      cluster = ~metarea,
      lean = TRUE,
      notes = FALSE
    )

  list(
    est1 |>
      broom::tidy() |>
      filter(str_detect(term, "^urban")) |>
      mutate(term = "Top 20") |>
      select(term, est = estimate, se = std.error) |>
      mutate(year = y, .before = 1),
    est2 |>
      broom::tidy() |>
      filter(str_detect(term, "^urban")) |>
      mutate(term = "Other Urban") |>
      select(term, est = estimate, se = std.error) |>
      mutate(year = y, .before = 1)
  ) |>
    list_rbind()
}

est_big_vs_small_city <- function(data_y) {
  y <- data_y$year[1]

  data_y <- data_y |>
    left_join(msa_pop_long |> select(-metarea), by = c("code", "year")) |>
    mutate(
      ## Yankow (2006)
      pop_gt_1million = +(pop >= 1000000),
      pop_gt_1million = replace_na(pop_gt_1million, 0),
      ## Baum-Snow and Pavan (2001)
      pop_gt_1.5million = +(pop >= 1500000),
      pop_gt_1.5million = replace_na(pop_gt_1.5million, 0)
    )

  data_y |>
    feols(
      ln_weeklywage ~
        i(urban, i.pop_gt_1million) +
        ..group_averages |
        ..individual_fe,
      weights = ~perwt,
      cluster = ~metarea,
      lean = TRUE,
      notes = FALSE
    ) |>
    broom::tidy() |>
    filter(str_detect(term, "^urban")) |>
    mutate(term = str_replace(term, "urban::1:pop_gt_1million::", "")) |>
    select(term, est = estimate, se = std.error) |>
    mutate(
      term = if_else(term == "1", "MSA $\\geq 1$ Million", "MSA $< 1$ Million")
    ) |>
    mutate(year = y, .before = 1)
}

## Region-specific estimates
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

## College vs. No-college
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

## White vs. Black estimates
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

## Region x Race
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

## Ages above and below 40
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

## By "manufacturing"
est_by_manufacturing <- function(data_y) {
  y <- data_y$year[1]

  ## Primary Industrial Hub Occupations:
  ##
  ## 51-0000 Production Occupations - This is the core manufacturing workforce (machinists, assemblers, welders, textile workers, etc.)
  ## 47-0000 Construction and Extraction Occupations - Construction workers, miners, and extraction workers
  ## 53-0000 Transportation and Material Moving Occupations - Truck drivers, dockworkers, railroad workers, warehouse workers
  ##
  ## Secondary Industrial Hub Occupations:
  ##
  ## 49-0000 Installation, Maintenance, and Repair Occupations - Mechanics and repairers who kept industrial machinery running
  est <- data_y |>
    mutate(
      occ_is_manufacturing = occupation %in% c(47, 49, 51, 53)
    ) |>
    feols(
      ln_weeklywage ~
        i(occ_is_manufacturing) +
        i(urban, i.occ_is_manufacturing, ref = 0) +
        ln_ma_removeown +
        ..group_averages |
        agegroup + educ + white,
      weights = ~perwt,
      cluster = ~metarea,
      lean = TRUE,
      notes = FALSE
    )

  est |>
    broom::tidy() |>
    filter(str_detect(term, "^urban::1")) |>
    mutate(
      term = if_else(
        str_detect(term, "occ_is_manufacturing::TRUE"),
        "Manufacturing",
        "Other Occupation"
      )
    ) |>
    select(term, est = estimate, se = std.error) |>
    mutate(year = y, .before = 1)
}

## By SOC two-digit occupation
est_by_occupation <- function(data_y) {
  y <- data_y$year[1]

  est <- data_y |>
    mutate(occupation = na_if(occupation, "")) |>
    filter(!is.na(occupation)) |>
    feols(
      ln_weeklywage ~
        i(occupation) +
        i(urban, i.occupation, ref = 0) +
        ln_ma_removeown +
        ..group_averages |
        agegroup + educ + white,
      weights = ~perwt,
      cluster = ~metarea,
      lean = TRUE,
      notes = FALSE
    )

  est |>
    broom::tidy() |>
    filter(str_detect(term, "^urban")) |>
    mutate(
      occupation = str_extract(term, "urban::1:occupation::(.*)", group = 1)
    ) |>
    filter(occupation != "NA") |>
    mutate(
      ## https://www.bls.gov/soc/socguide.htm
      occ_str = case_when(
        occupation == "11" ~ "Management Occupations",
        occupation == "13" ~ "Business and Financial Operations Occupations",
        occupation == "15" ~ "Computer and Mathematical Occupations",
        occupation == "17" ~ "Architecture and Engineering Occupations",
        occupation == "19" ~ "Life, Physical, and Social Science Occupations",
        occupation == "21" ~ "Community and Social Services Occupations",
        occupation == "23" ~ "Legal Occupations",
        occupation == "25" ~ "Education, Training, and Library Occupations",
        occupation ==
          "27" ~ "Arts, Design, Entertainment, Sports, and Media Occupations",
        occupation ==
          "29" ~ "Healthcare Practitioners and Technical Occupations",
        occupation == "31" ~ "Healthcare Support Occupations",
        occupation == "33" ~ "Protective Service Occupations",
        occupation == "35" ~ "Food Preparation and Serving Related Occupations",
        occupation ==
          "37" ~ "Building and Grounds Cleaning and Maintenance Occupations",
        occupation == "39" ~ "Personal Care and Service Occupations",
        occupation == "41" ~ "Sales and Related Occupations",
        occupation == "43" ~ "Office and Administrative Support Occupations",
        occupation == "45" ~ "Farming, Fishing, and Forestry Occupations",
        occupation == "47" ~ "Construction and Extraction Occupations",
        occupation ==
          "49" ~ "Installation, Maintenance, and Repair Occupations",
        occupation == "51" ~ "Production Occupations",
        occupation == "53" ~ "Transportation and Material Moving Occupations",
        occupation == "55" ~ "Military Specific Occupations"
      ),
    ) |>
    select(term = occ_str, occupation, est = estimate, se = std.error) |>
    mutate(year = y, .before = 1)
}

### Estimate all heterogeneity and collect results ----
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
    cat("  -> est_big_vs_small_city\n")
    est_big_vs_small_city <- est_big_vs_small_city(data_y)
    cat("  -> est_top20_separately\n")
    est_top20_separately <- est_top20_separately(data_y)
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
    cat("  -> est_by_manufacturing\n")
    est_by_manufacturing <- est_by_manufacturing(data_y)
    cat("  -> est_by_occupation\n")
    est_by_occupation <- est_by_occupation(data_y)

    list(
      est_1940_msas = est_1940_msas,
      est_top20 = est_top20,
      est_big_vs_small_city = est_big_vs_small_city,
      est_top20_separately = est_top20_separately,
      est_region = est_region,
      est_by_college = est_by_college,
      est_by_race = est_by_race,
      est_by_race_and_region = est_by_race_and_region,
      est_by_older = est_by_older,
      est_by_manufacturing = est_by_manufacturing,
      est_by_occupation = est_by_occupation
    )
  })

  saveRDS(ests, file = here("data/estimates-heterogeneity.rds"))
}

# %%
ests <- readRDS(here("data/estimates-heterogeneity.rds"))

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

ests_1940_msas <- map(ests, ~ .x$est_1940_msas) |>
  list_rbind() |>
  process_ests()

ests_top20 <- map(ests, ~ .x$est_top20) |>
  list_rbind() |>
  process_ests() |>
  mutate(group = fct(term, c("Top 20", "Other Urban")))

ests_big_vs_small_city <- map(ests, ~ .x$est_big_vs_small_city) |>
  list_rbind() |>
  process_ests() |>
  mutate(group = fct(term, c("MSA $< 1$ Million", "MSA $\\geq 1$ Million")))

ests_top20_separately <- map(ests, ~ .x$est_top20_separately) |>
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

ests_by_occupation <- map(ests, ~ .x$est_by_occupation) |>
  list_rbind() |>
  process_ests() |>
  mutate(term = term |> str_replace(" Occupations$", ""))

ests_by_manufacturing <- map(ests, ~ .x$est_by_manufacturing) |>
  list_rbind() |>
  process_ests() |>
  mutate(
    group = fct(term, c("Other Occupation", "Manufacturing"))
  )


## Plot point estimates ----
# %%
basic_heterogeneity_plot <- function(ests, show_errorbar = FALSE) {
  ## Staggered
  errorbar <- NULL
  if (show_errorbar) {
    idx_group <- match(ests$group, unique(ests$group))
    N_group <- length(unique(idx_group))
    shift_min <- -0.5 * length(unique(ests$group)) / 2
    shift_max <- abs(shift_min)

    ests$year <- ests$year +
      seq(shift_min, shift_max, length.out = N_group)[idx_group]

    errorbar <- geom_errorbar(
      aes(ymin = exp_est_lower95, ymax = exp_est_upper95),
      linewidth = 1.5,
      width = 1
    )
  }

  ggplot(
    ests,
    aes(
      x = year,
      y = exp_est,
      group = group,
      color = group,
      shape = group
    )
  ) +
    errorbar +
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
      limits = c(-0.05, 0.4),
      expand = c(0, 0, 0, 0)
    ) +
    scale_x_continuous(breaks = seq(1940, 2020, by = 10)) +
    kfbmisc::theme_kyle(base_size = 16, grid_minor = "h") +
    guides(
      colour = guide_legend(
        title.position = "top",
        nrow = 1,
        override.aes = list(linewidth = 0, size = 3)
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
(plot_top20 <- basic_heterogeneity_plot(ests_top20, show_errorbar = TRUE) +
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
(plot_big_vs_small_cities <- basic_heterogeneity_plot(
  ests_big_vs_small_city,
  show_errorbar = TRUE
) +
  scale_color_manual(
    values = c(
      "MSA $\\geq 1$ Million" = "grey10",
      "MSA $< 1$ Million" = "grey40"
    )
  ) +
  scale_shape_manual(
    values = c(
      "MSA $\\geq 1$ Million" = 15,
      "MSA $< 1$ Million" = 16
    )
  ))

# %%
(plot_top20_separately <- basic_heterogeneity_plot(
  ests_top20_separately,
  show_errorbar = TRUE
) +
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
(plot_college <- basic_heterogeneity_plot(
  ests_by_college,
  show_errorbar = TRUE
) +
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
(plot_race <- basic_heterogeneity_plot(ests_by_race, show_errorbar = TRUE) +
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
(plot_older <- basic_heterogeneity_plot(ests_by_older, show_errorbar = TRUE) +
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
(plot_region <- basic_heterogeneity_plot(ests_region, show_errorbar = TRUE) +
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
## Using five most popular occupations
## data |> pull(occupation, as_vector = TRUE) |> table() |> sort()
##
## data |>
##   summarize(.by = c(year, occupation, occ_str), n_workers = sum(perwt, na.rm = TRUE)) |>
##   collect() |>
##   arrange(year, desc(n_workers)) |>
##   filter(year == 2010)
##
## ests_by_occupation |>
##   filter(occupation %in% c("51", "53", "11", "41", "47")) |>
##   pull(term) |>
##   unique() |> clipr::write_clip()

(plot_occupation <- ggplot(
  ests_by_occupation |>
    filter(
      occupation == "51" |
        occupation == "53" |
        occupation == "11" |
        occupation == "41" |
        occupation == "47"
    ),
  aes(x = year, y = exp_est, group = term, color = term, shape = term)
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
    limits = c(-0.05, 0.4),
    expand = c(0, 0, 0, 0)
  ) +
  scale_x_continuous(breaks = seq(1940, 2020, by = 10)) +
  scale_color_manual(
    # values = c("grey10", "grey30", "grey50", "grey70", "grey90")
    # values = ggsci::pal_jama("default")(5)
    values = unname(kfbmisc::kyle_colors)
  ) +
  scale_shape_manual(values = c(15, 17, 19, 18, 4)) +
  guides(
    color = guide_legend(nrow = 2, override.aes = list(linewidth = 0, size = 3))
  ) +
  kfbmisc::theme_kyle(base_size = 16, legend = "bottom") +
  theme(
    legend.position = "inside",
    legend.position.inside = c(0.5, 0.88),
    legend.background = element_rect(fill = "white", color = "gray20"),
    legend.margin = margin(4, 6, 6, 6),
    legend.key.spacing.y = unit(6, "pt"),
    panel.grid.minor.x = element_blank(),
    axis.line.y = element_blank(),
    axis.ticks.y = element_blank(),
    axis.line.x = element_blank(),
    axis.ticks.x = element_blank()
  ))

# %%
(plot_by_manufacturing <- basic_heterogeneity_plot(ests_by_manufacturing) +
  scale_color_manual(
    values = c(
      "Manufacturing" = "grey10",
      "Other Occupation" = "grey40"
    )
  ) +
  scale_shape_manual(
    values = c(
      "Manufacturing" = 15,
      "Other Occupation" = 16
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
  ) +
  scale_y_continuous(
    labels = scales::label_percent(suffix = "\\%"),
    limits = c(-0.05, 0.52),
    expand = c(0, 0, 0, 0)
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
  ) +
  scale_y_continuous(
    labels = scales::label_percent(suffix = "\\%"),
    limits = c(-0.05, 0.52),
    expand = c(0, 0, 0, 0)
  ))

# %%
kfbmisc::tikzsave(
  here("out/figures/heterogeneity/by_top20.pdf"),
  plot_top20,
  width = 14,
  height = 4
)
kfbmisc::tikzsave(
  here("out/figures/heterogeneity/big_vs_small_cities.pdf"),
  plot_big_vs_small_cities,
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
kfbmisc::tikzsave(
  here("out/figures/heterogeneity/by_manufacturing.pdf"),
  plot_by_manufacturing,
  width = 14,
  height = 4
)
kfbmisc::tikzsave(
  here("out/figures/heterogeneity/by_occupation.pdf"),
  plot_occupation,
  width = 14,
  height = 4
)

## Create tables ----
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
fs::dir_create(here("out/tables/heterogeneity"))

display_and_save <- function(x, file) {
  cat(x, sep = "\n")
  cat(x, file = file, sep = "\n")
}

basic_heterogeneity_table(ests_big_vs_small_city) |>
  display_and_save(here("out/tables/heterogeneity/big_vs_small_cities.tex"))

basic_heterogeneity_table(ests_region) |>
  display_and_save(here("out/tables/heterogeneity/by_region.tex"))

basic_heterogeneity_table(ests_by_college) |>
  display_and_save(here("out/tables/heterogeneity/by_college.tex"))

basic_heterogeneity_table(ests_by_race) |>
  display_and_save(here("out/tables/heterogeneity/by_race.tex"))

basic_heterogeneity_table(ests_by_older) |>
  display_and_save(here("out/tables/heterogeneity/by_older.tex"))

basic_heterogeneity_table(ests_by_manufacturing) |>
  display_and_save(here("out/tables/heterogeneity/by_manufacutring.tex"))

basic_heterogeneity_table(
  ests_by_occupation |> arrange(year, occupation) |> mutate(group = fct(term))
) |>
  display_and_save(here("out/tables/heterogeneity/by_occupation.tex"))

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
gh <- "~/Documents/Projects/urban-wage-premium"

source(here("code/utils/calculate_group_averages.R"))

# %%
data <- glue("{dropbox}/data/parquet/urban_wage") |>
  arrow::open_dataset() |>
  filter(ind_main_sample == TRUE) |>
  mutate(msacode = if_else(code == 0, statefip * 10000, code)) |>
  # 1970 does not have vet stat
  mutate(vetstat = if_else(year == 1970 & is.na(vetstat), 0, vetstat))

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
setFixest_fml(
  ..individual_fe = reformulate(individual_dummy_vars),
  ..group_averages = reformulate(group_averages)
)

get_data_y <- function(data, y) {
  data |>
    filter(year == y) |>
    collect() |>
    calculate_group_averages(by = "msacode")
}
year_seq <- seq(1940, 2010, by = 10)


#' # City-specific Estimates ---------------------------------------------------
# %%
if (FALSE) {
  walk(year_seq, function(y) {
    cat(sprintf("On year %s", y), "\n")

    data_y <- get_data_y(data, y)

    if (nrow(data_y) > 1500000) {
      data_y <- data_y |>
        slice_sample(n = 150000)
    }

    tictoc::tic(sprintf("Year %s", y))
    est <- data_y |>
      feols(
        ln_weeklywage ~
          ..group_averages + i(urban, i.code, ref = 0) | +..individual_fe,
        weights = ~perwt,
        combine.quick = FALSE,
        lean = TRUE,
        vcov = "hc1"
      )
    tictoc::toc()

    res <- coeftable(est) |>
      as_tibble(rownames = "term") |>
      select(term, est = Estimate, se = `Std. Error`) |>
      filter(str_detect(term, "^urban")) |>
      mutate(
        year = y,
        code = str_extract(term, "urban::1:code::(.*)", group = 1),
        .before = est
      ) |>
      select(-term)

    write_csv(res, glue("{gh}/data/temp_city_specific_{y}.csv"))
    rm(data_y)
    rm(est)
    rm(res)
  })
}

# %%
ests_city_specific <- map(year_seq, function(y) {
  read_csv(
    glue("{gh}/data/temp_city_specific_{y}.csv"),
    show_col_types = FALSE
  )
})

ests_city_specific <- ests_city_specific |>
  list_rbind() |>
  mutate(exp_est = exp(est) - 1) |>
  mutate(
    code = as.numeric(code)
  )

# %%
with(ests_city_specific, collapse::qsu(exp_est, g = year))


#' # Plotting City-specific Estimates ------------------------------------------
# %%
# Not currently used
city_speicific_summ <- ests_city_specific |>
  summarize(
    .by = year,
    exp_est_upper = quantile(exp_est, 0.9),
    exp_est_lower = quantile(exp_est, 0.1)
  )

(plot_city_specific <- ggplot() +
  geom_point(
    mapping = aes(x = year, y = exp_est),
    data = ests_city_specific,
    alpha = 0.25
  ) +
  geom_linerange(
    aes(x = year, ymin = exp_est_lower, ymax = exp_est_upper),
    data = city_speicific_summ,
    linewidth = 8,
    alpha = 0.4
  ) +
  labs(
    y = "City-specific Wage Premium",
    x = NULL,
  ) +
  scale_x_continuous(breaks = seq(1940, 2020, by = 10)) +
  scale_y_continuous(
    labels = scales::label_percent(suffix = "\\%"),
    limits = c(-0.25, 0.5)
  ) +
  kfbmisc::theme_kyle(base_size = 16))

#' # Convergence ---------------------------------------------------------------
# %%
changes <- ests_city_specific |>
  filter(year %in% c(1940, 2010)) |>
  filter(n() == 2, .by = code) |>
  select(code, est, se, exp_est, year) |>
  mutate(
    .by = code,
    est_2010 = est[year == 2010],
    se_2010 = se[year == 2010],
    est_1940 = est[year == 1940],
    se_1940 = se[year == 1940],
    exp_est_2010 = exp_est[year == 2010],
    exp_est_1940 = exp_est[year == 1940],
    delta_est = (est[year == 2010] - est[year == 1940]),
    delta_exp_est = (exp_est[year == 2010] - exp_est[year == 1940]),
  ) |>
  arrange(delta_exp_est) |>
  mutate(
    delta_exp_est_binned = fct(case_when(
      delta_exp_est < -0.5 ~ "$< -50\\%$",
      delta_exp_est < -0.2 ~ "$-50\\%$ to $-20\\%$",
      delta_exp_est < -0.05 ~ "$-20\\%$ to $5\\%$",
      delta_exp_est < 0.05 ~ "$-5\\%$ to $5\\%$",
      delta_exp_est < 0.2 ~ "$5\\%$ to $20\\%$",
      delta_exp_est < 0.5 ~ "$20\\%$ to $50\\%$",
      TRUE ~ "$> 50\\%$"
    ))
  )

# %%
changes |>
  filter(year == 1940) |>
  with(collapse::qsu(cbind(exp_est_1940, exp_est_2010)))

# %%
# Notion of convergence from Ganong and Shoag:
# \Delta_{2010, 1940} exp_est = exp_est_{1940} \beta
feols(
  delta_exp_est ~ exp_est,
  data = changes |> filter(year == 1940)
)

(plot_convergence <- changes |>
  filter(year == 1940) |>
  ggplot() +
  geom_point(
    aes(x = exp_est_1940, y = delta_exp_est)
  ) +
  geom_smooth(
    aes(x = exp_est_1940, y = delta_exp_est),
    method = "lm",
    formula = y ~ x,
    color = "#e64173",
    fill = "#e64173",
    alpha = 0.2
  ) +
  labs(
    x = "Estimated Wage Premium in 1940",
    y = "1940 to 2010 Change in Estimated Wage Premium"
  ) +
  scale_x_continuous(labels = scales::label_percent(suffix = "\\%")) +
  scale_y_continuous(labels = scales::label_percent(suffix = "\\%")) +
  kfbmisc::theme_kyle(base_size = 16) +
  theme(
    legend.title = element_text(size = rel(1 / 1.125)),
    legend.text = element_text(size = rel(1 / 1.125^2)),
    legend.position = "bottom",
    legend.justification = "center",
    panel.grid.minor.x = element_blank(),
    axis.title = element_text(size = rel(1 / 1.125^2))
  ))


#' # ----
# %%
(plot_1940_vs_2010 <- ggplot(changes |> filter(year == 1940)) +
  geom_hline(yintercept = 0, linetype = "dotted", color = "grey60") +
  geom_vline(xintercept = 0, linetype = "dotted", color = "grey60") +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
  annotate(
    "label",
    x = 0.42,
    y = 0.39,
    label = "45 degree line",
    hjust = 0,
    fill = "white",
    label.size = 0
  ) +
  geom_point(
    aes(x = est_1940, y = est_2010)
  ) +
  labs(
    x = "Estimated Wage Premium in 1940",
    y = "Estimated Wage Premium in 2010"
  ) +
  scale_x_continuous(labels = scales::label_percent(suffix = "\\%")) +
  scale_y_continuous(labels = scales::label_percent(suffix = "\\%")) +
  kfbmisc::theme_kyle(base_size = 16) +
  theme(
    legend.title = element_text(size = rel(1 / 1.125)),
    legend.text = element_text(size = rel(1 / 1.125^2)),
    legend.position = "bottom",
    legend.justification = "center",
    panel.grid.minor.x = element_blank(),
    axis.title = element_text(size = rel(1 / 1.125^2))
  ))

#' # City-specific changes -----------------------------------------------------
# %%
(plot_changes <- ggplot(changes) +
  geom_line(
    aes(x = year, y = exp_est, group = code, color = delta_exp_est_binned)
  ) +
  geom_point(
    aes(x = year, y = exp_est, color = delta_exp_est_binned),
    size = 1
  ) +
  labs(
    y = "City-specific Wage Premium",
    x = NULL,
    group = NULL,
    color = "Change in City-specific Wage Premium (pct. pt.)"
  ) +
  scale_x_continuous(breaks = seq(1940, 2020, by = 10)) +
  scale_y_continuous(
    labels = scales::label_percent(suffix = "\\%")
  ) +
  scale_color_manual(
    values = c(
      "#d73027",
      "#fdae61",
      "#fee08b",
      "#ffffbf",
      "#d9ef8b",
      "#66bd63",
      "#1a9850"
    ),
    guide = guide_legend(
      title.position = "top",
      nrow = 2,
      byrow = TRUE,
      override.aes = list(size = 2),
      direction = "horizontal"
    )
  ) +
  kfbmisc::theme_kyle(base_size = 16) +
  theme(
    legend.title = element_text(size = rel(1 / 1.125)),
    legend.text = element_text(size = rel(1 / 1.125^2)),
    legend.position = "bottom",
    legend.justification = "center",
    panel.grid.minor.x = element_blank(),
    axis.line.y = element_blank(),
    axis.ticks.y = element_blank(),
    axis.line.x = element_blank(),
    axis.ticks.x = element_blank()
  ))

#' # Rank-rank regression ------------------------------------------------------
# %%
ranks <- ests_city_specific |>
  filter(year %in% c(1940, 2010)) |>
  filter(n() == 2, .by = code) |>
  select(code, est, se, exp_est, year) |>
  mutate(
    .by = year,
    rank = rank(est)
  ) |>
  mutate(
    .by = code,
    rank_2010 = rank[year == 2010],
    rank_1940 = rank[year == 1940],
    delta_rank = rank_2010 - rank_1940
  ) |>
  arrange(rank_1940)

(plot_rank_rank <- ranks |>
  filter(year == 1940) |>
  ggplot() +
  geom_point(
    aes(x = rank_1940, y = rank_2010)
  ) +
  geom_smooth(
    aes(x = rank_1940, y = rank_2010),
    method = "lm",
    formula = y ~ x,
    color = "#e64173",
    fill = "#e64173",
    alpha = 0.2
  ) +
  labs(
    x = "City-wage Premium Rank in 1940",
    y = "City-wage Premium Rank in 2010"
  ) +
  kfbmisc::theme_kyle(base_size = 16) +
  theme(
    legend.title = element_text(size = rel(1 / 1.125)),
    legend.text = element_text(size = rel(1 / 1.125^2)),
    legend.position = "bottom",
    legend.justification = "center",
    panel.grid.minor.x = element_blank(),
    axis.title = element_text(size = rel(1 / 1.125^2))
  ))


# %%
# kfbmisc::tikzsave(
#   glue("{gh}/out/figures/city_specific/distribution.pdf"),
#   plot_city_specific,
#   width = 10, height = 5
# )
kfbmisc::tikzsave(
  glue("{gh}/out/figures/city_specific/convergence.pdf"),
  plot_convergence,
  width = 10,
  height = 5
)
kfbmisc::tikzsave(
  glue("{gh}/out/figures/city_specific/changes.pdf"),
  plot_changes,
  width = 10,
  height = 6
)
kfbmisc::tikzsave(
  glue("{gh}/out/figures/city_specific/rank_rank.pdf"),
  plot_rank_rank,
  width = 10,
  height = 6
)
kfbmisc::tikzsave(
  glue("{gh}/out/figures/city_specific/1940_vs_2010.pdf"),
  plot_1940_vs_2010,
  width = 10,
  height = 6
)

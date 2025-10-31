# %%
library(tidyverse)
library(fixest)
library(glue)
library(here)
library(arrow)
library(collapse)
# remotes::install_github("kylebutts/kfbmisc")
library(kfbmisc)
library(patchwork)

dropbox <- "~/Dropbox/Projects/UrbanWagePremium"
gh <- "~/Documents/Projects/urban-wage-premium"

# %%
data <- glue("{dropbox}/data/parquet/urban_wage") |>
  arrow::open_dataset()

#' ## Regression estimates of urban wage premium
# %%
setFixest_fml(
  ..individual_fe = ~ agegroup + educ + white,
  ..group_averages = ~
    # Share white
    share_white +
      # Share age groups
      share_agegroup_5 +
      share_agegroup_6 +
      share_agegroup_7 +
      share_agegroup_8 +
      share_agegroup_9 +
      share_agegroup_10 +
      share_agegroup_11 +
      share_agegroup_12 +
      # Share veteran
      share_vetstat_1 +
      # Share marital status
      share_marst_1 +
      share_marst_6 +
      share_marst_2 +
      # < HS
      I(
        share_educ_1 + share_educ_2 + share_educ_3 + share_educ_4 + share_educ_5
      ) +
      # HS
      share_educ_6 +
      # Some College
      I(share_educ_7 + share_educ_8 + share_educ_9) +
      # BA and >BA
      share_educ_10 +
      share_educ_11
)

formula_vars <- getFixest_fml() |>
  map(function(x) all.vars(xpd(rhs = x))) |>
  list_c()

included_vars <- c(
  "metarea",
  "perwt",
  "urban",
  "ln_weeklywage",
  "weeklywage",
  "rent",
  "valueh",
  "ln_ma_removeown",
  formula_vars
)

# %%
get_data_y <- function(data, y) {
  data |>
    filter(year == y) |>
    collect()
}
year_seq <- seq(1940, 2010, by = 10)

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
rent_premium <- map(year_seq, function(y) {
  data_y <- get_data_y(data, y)
  data_y <- data_y |>
    filter(net_weeklywage > 0, net_weeklywage_rentonly > 0)

  est <- feols(
    ln_weekly_housing_costs ~ i(urban),
    data = data_y,
    weights = ~perwt,
    cluster = ~metarea,
    lean = TRUE
  )

  est |>
    broom::tidy() |>
    filter(str_detect(term, "^urban")) |>
    select(term, est = estimate, se = std.error) |>
    mutate(year = y, .before = 1)
})

rent_premium <- rent_premium |>
  list_rbind() |>
  process_ests()

# %%
ests <- map(year_seq, function(y) {
  data_y <- get_data_y(data, y)
  data_y <- data_y |>
    filter(net_weeklywage > 0, net_weeklywage_rentonly > 0)

  est_weeklywage <- feols(
    ln_weeklywage ~ i(urban),
    data = data_y,
    weights = ~perwt,
    cluster = ~metarea,
    lean = TRUE
  )
  est_net_weeklywage <- feols(
    ln_net_weeklywage ~ i(urban),
    data = data_y,
    weights = ~perwt,
    cluster = ~metarea,
    lean = TRUE
  )

  bind_rows(
    broom::tidy(est_weeklywage) |>
      filter(str_detect(term, "^urban")) |>
      select(term, est = estimate, se = std.error) |>
      mutate(year = y, outcome = "Weekly Wage", .before = 1),
    broom::tidy(est_net_weeklywage) |>
      filter(str_detect(term, "^urban")) |>
      select(term, est = estimate, se = std.error) |>
      mutate(year = y, outcome = "Weekly Wage Net of Housing", .before = 1)
  )
})

ests <- ests |>
  list_rbind() |>
  process_ests() |>
  mutate(
    outcome = fct(outcome)
  )

# %%
ests_group <- map(year_seq, function(y) {
  data_y <- get_data_y(data, y)
  data_y <- data_y |>
    filter(net_weeklywage > 0, net_weeklywage_rentonly > 0)

  est_weeklywage <- feols(
    ln_weeklywage ~
      i(urban) + ln_ma_removeown + ..group_averages | ..individual_fe,
    data = data_y,
    weights = ~perwt,
    cluster = ~metarea,
    lean = TRUE
  )
  est_net_weeklywage <- feols(
    ln_net_weeklywage ~
      i(urban) + ln_ma_removeown + ..group_averages | ..individual_fe,
    data = data_y,
    weights = ~perwt,
    cluster = ~metarea,
    lean = TRUE
  )

  bind_rows(
    broom::tidy(est_weeklywage) |>
      filter(str_detect(term, "^urban")) |>
      select(term, est = estimate, se = std.error) |>
      mutate(year = y, outcome = "Weekly Wage", .before = 1),
    broom::tidy(est_net_weeklywage) |>
      filter(str_detect(term, "^urban")) |>
      select(term, est = estimate, se = std.error) |>
      mutate(year = y, outcome = "Weekly Wage Net of Housing", .before = 1)
  )
})

ests_group <- ests_group |>
  list_rbind() |>
  process_ests() |>
  mutate(
    outcome = fct(outcome)
  )

# %%
rent_premium_rent_only <- map(year_seq, function(y) {
  data_y <- get_data_y(data, y)
  est <- feols(
    ln_weekly_housing_costs_rent_only ~ i(urban),
    data = data_y,
    weights = ~perwt,
    cluster = ~metarea,
    lean = TRUE
  )

  est |>
    broom::tidy() |>
    filter(str_detect(term, "^urban")) |>
    select(term, est = estimate, se = std.error) |>
    mutate(year = y, .before = 1)
})

rent_premium_rent_only <- rent_premium_rent_only |>
  list_rbind() |>
  process_ests()

# %%
ests_rent_only <- map(year_seq, function(y) {
  data_y <- get_data_y(data, y)
  est_weeklywage <- feols(
    ln_weeklywage ~ i(urban),
    data = data_y,
    weights = ~perwt,
    cluster = ~metarea,
    lean = TRUE
  )
  est_net_weeklywage <- feols(
    ln_net_weeklywage_rentonly ~ i(urban),
    data = data_y,
    weights = ~perwt,
    cluster = ~metarea,
    lean = TRUE
  )

  bind_rows(
    broom::tidy(est_weeklywage) |>
      filter(str_detect(term, "^urban")) |>
      select(term, est = estimate, se = std.error) |>
      mutate(year = y, outcome = "Weekly Wage", .before = 1),
    broom::tidy(est_net_weeklywage) |>
      filter(str_detect(term, "^urban")) |>
      select(term, est = estimate, se = std.error) |>
      mutate(year = y, outcome = "Weekly Wage Net of Housing", .before = 1)
  )
})

ests_rent_only <- ests_rent_only |>
  list_rbind() |>
  process_ests() |>
  mutate(
    outcome = fct(outcome)
  )

# %%
ests_group_rent_only <- map(year_seq, function(y) {
  data_y <- get_data_y(data, y)
  est_weeklywage <- feols(
    ln_weeklywage ~
      i(urban) + ln_ma_removeown + ..group_averages | ..individual_fe,
    data = data_y,
    weights = ~perwt,
    cluster = ~metarea,
    lean = TRUE
  )
  est_net_weeklywage <- feols(
    ln_net_weeklywage_rentonly ~
      i(urban) + ln_ma_removeown + ..group_averages | ..individual_fe,
    data = data_y,
    weights = ~perwt,
    cluster = ~metarea,
    lean = TRUE
  )

  bind_rows(
    broom::tidy(est_weeklywage) |>
      filter(str_detect(term, "^urban")) |>
      select(term, est = estimate, se = std.error) |>
      mutate(year = y, outcome = "Weekly Wage", .before = 1),
    broom::tidy(est_net_weeklywage) |>
      filter(str_detect(term, "^urban")) |>
      select(term, est = estimate, se = std.error) |>
      mutate(year = y, outcome = "Weekly Wage Net of Housing", .before = 1)
  )
})

ests_group_rent_only <- ests_group_rent_only |>
  list_rbind() |>
  process_ests() |>
  mutate(
    outcome = fct(outcome)
  )

# %%
(plot_rent_premium <- ggplot(
  rent_premium,
  aes(x = year, y = exp_est, ymin = exp_est_lower95, ymax = exp_est_upper95)
) +
  geom_line(linewidth = 2) +
  geom_errorbar(
    aes(ymin = exp_est_lower95, ymax = exp_est_upper95),
    linewidth = 1.5,
    width = 1
  ) +
  geom_point(size = 5) +
  labs(
    x = NULL,
    y = "Urban Housing Cost Premium",
    group = NULL,
    shape = NULL,
    color = NULL
  ) +
  scale_y_continuous(
    labels = scales::label_percent(suffix = "\\%"),
    limits = c(0, NA),
    expand = expansion(add = c(0, 0.1))
  ) +
  scale_x_continuous(breaks = seq(1940, 2010, by = 10)) +
  kfbmisc::theme_kyle(base_size = 16) +
  theme(
    axis.title.y = element_text(size = rel(0.8)),
    panel.grid.minor.x = element_blank(),
    axis.line.y = element_blank(),
    axis.ticks.y = element_blank(),
    axis.line.x = element_blank(),
    axis.ticks.x = element_blank()
  ))

# %%
(plot_raw <- ggplot(
  ests,
  aes(
    x = year,
    y = exp_est,
    group = outcome,
    color = outcome,
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
  geom_point(aes(shape = outcome), size = 5) +
  labs(
    x = NULL,
    y = "Urban Wage Premium",
    group = NULL,
    shape = NULL,
    color = NULL
  ) +
  scale_y_continuous(
    labels = scales::label_percent(suffix = "\\%"),
    limits = c(-0.05, 0.5),
    expand = c(0, 0)
  ) +
  scale_x_continuous(breaks = seq(1940, 2010, by = 10)) +
  scale_color_manual(values = c("grey70", "grey10")) +
  scale_shape_manual(values = c(15, 16)) +
  guides(
    color = guide_legend(
      title.position = "top",
      nrow = 1,
      override.aes = list(linetype = 0)
    )
  ) +
  kfbmisc::theme_kyle(base_size = 16) +
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
  ))

# %%
(plot_group_avgs <- ggplot(
  ests_group,
  aes(
    x = year,
    y = exp_est,
    group = outcome,
    color = outcome,
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
  geom_point(aes(shape = outcome), size = 5) +
  labs(
    x = NULL,
    y = "Urban Wage Premium",
    group = NULL,
    shape = NULL,
    color = NULL
  ) +
  scale_y_continuous(
    labels = scales::label_percent(suffix = "\\%"),
    limits = c(-0.05, 0.5),
    expand = c(0, 0)
  ) +
  scale_x_continuous(breaks = seq(1940, 2010, by = 10)) +
  scale_color_manual(values = c("grey70", "grey10")) +
  scale_shape_manual(values = c(15, 16)) +
  guides(
    color = guide_legend(
      title.position = "top",
      nrow = 1,
      override.aes = list(linetype = 0)
    )
  ) +
  kfbmisc::theme_kyle(base_size = 16) +
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
  ))

# %%
(plot_rent_premium_rent_only <- ggplot(
  rent_premium_rent_only,
  aes(x = year, y = exp_est, ymin = exp_est_lower95, ymax = exp_est_upper95)
) +
  geom_line(linewidth = 2) +
  geom_errorbar(
    aes(ymin = exp_est_lower95, ymax = exp_est_upper95),
    linewidth = 1.5,
    width = 1
  ) +
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
    limits = c(0, NA),
    expand = expansion(add = c(0, 0.1))
  ) +
  scale_x_continuous(breaks = seq(1940, 2010, by = 10)) +
  kfbmisc::theme_kyle(base_size = 16) +
  theme(
    axis.title.y = element_text(size = rel(0.8)),
    panel.grid.minor.x = element_blank(),
    axis.line.y = element_blank(),
    axis.ticks.y = element_blank(),
    axis.line.x = element_blank(),
    axis.ticks.x = element_blank()
  ))

# %%
(plot_raw_rent_only <- ggplot(
  ests_rent_only,
  aes(
    x = year,
    y = exp_est,
    group = outcome,
    color = outcome,
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
  geom_point(aes(shape = outcome), size = 5) +
  labs(
    x = NULL,
    y = "Urban Wage Premium",
    group = NULL,
    shape = NULL,
    color = NULL
  ) +
  scale_y_continuous(
    labels = scales::label_percent(suffix = "\\%"),
    limits = c(-0.05, 0.5),
    expand = c(0, 0)
  ) +
  scale_x_continuous(breaks = seq(1940, 2010, by = 10)) +
  scale_color_manual(values = c("grey70", "grey10")) +
  scale_shape_manual(values = c(15, 16)) +
  guides(
    color = guide_legend(
      title.position = "top",
      nrow = 1,
      override.aes = list(linetype = 0)
    )
  ) +
  kfbmisc::theme_kyle(base_size = 16) +
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
  ))

# %%
(plot_group_avgs_rent_only <- ggplot(
  ests_group_rent_only,
  aes(
    x = year,
    y = exp_est,
    group = outcome,
    color = outcome,
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
  geom_point(aes(shape = outcome), size = 5) +
  labs(
    x = NULL,
    y = "Urban Wage Premium",
    group = NULL,
    shape = NULL,
    color = NULL
  ) +
  scale_y_continuous(
    labels = scales::label_percent(suffix = "\\%"),
    limits = c(-0.05, 0.5),
    expand = c(0, 0)
  ) +
  scale_x_continuous(breaks = seq(1940, 2010, by = 10)) +
  scale_color_manual(values = c("grey70", "grey10")) +
  scale_shape_manual(values = c(15, 16)) +
  guides(
    color = guide_legend(
      title.position = "top",
      nrow = 1,
      override.aes = list(linetype = 0)
    )
  ) +
  kfbmisc::theme_kyle(base_size = 16) +
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
  ))

# %%
#  = Using rent and home value
# _rent_only = Just using rent
(plot_rent_premium / plot_rent_premium_rent_only)

# %%
#  = Using rent and home value
# _rent_only = Just using rent
(plot_raw / plot_raw_rent_only)

# %%
#  = Using rent and home value
# _rent_only = Just using rent
(plot_group_avgs / plot_group_avgs_rent_only)

# %%
kfbmisc::tikzsave(
  glue("{gh}/out/figures/raw_housing_costs_differences.pdf"),
  plot_rent_premium,
  width = 11,
  height = 5
)
kfbmisc::tikzsave(
  glue("{gh}/out/figures/net_urbanpremium_raw.pdf"),
  plot_raw,
  width = 11,
  height = 5
)
kfbmisc::tikzsave(
  glue("{gh}/out/figures/net_urbanpremium_causal.pdf"),
  plot_group_avgs,
  width = 11,
  height = 5
)
kfbmisc::tikzsave(
  glue("{gh}/out/figures/raw_housing_costs_differences_rent_only.pdf"),
  plot_rent_premium_rent_only,
  width = 11,
  height = 5
)
kfbmisc::tikzsave(
  glue("{gh}/out/figures/net_urbanpremium_raw_rent_only.pdf"),
  plot_raw_rent_only,
  width = 11,
  height = 5
)
kfbmisc::tikzsave(
  glue("{gh}/out/figures/net_urbanpremium_causal_rent_only.pdf"),
  plot_group_avgs_rent_only,
  width = 11,
  height = 5
)

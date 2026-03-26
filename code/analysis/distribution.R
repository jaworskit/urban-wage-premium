# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
library(tidyverse)
library(glue)
library(broom)
library(vroom)
library(fixest)
library(collapse)
library(arrow)
# remotes::install_github("kylebutts/kfbmisc")
library(kfbmisc)

# Local
dropbox <- "~/Dropbox/Projects/UrbanWagePremium"
source(here("code/utils/calculate_group_averages.R"))

# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
data <- glue("{dropbox}/data/parquet/urban_wage") |>
  arrow::open_dataset() |>
  filter(ind_main_sample == TRUE) |>
  filter(!is.na(perwt), perwt > 0)

# Regression Results -----------------------------------------------------------
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
  ..individual_controls = ~ I(marst == 1) +
    I(marst == 2) +
    I(marst == 6) +
    I(vetstat == 1),
  ..individual_fe = reformulate(individual_dummy_vars),
  ..group_averages = reformulate(group_averages)
)

# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
dataset <- NULL
results <- NULL
year_seq <- seq(1940, 2010, 10)

# Loop through year
for (i in 1:length(year_seq)) {
  y <- year_seq[i]
  cat(sprintf("On year: %s", y), "\n")

  data_y <- data |>
    filter(year == y) |>
    # 1970 does not have vet stat
    mutate(vetstat = if_else(year == 1970 & is.na(vetstat), 0, vetstat)) |>
    collect()

  data_y <- data_y |>
    calculate_group_averages(by = "msacode")

  ## Urban, Individual Controls & Group Averages
  est <- feols(
    ln_weeklywage ~
      i(urban, ref = 0) +
      ..individual_controls +
      ln_ma_removeown +
      ..group_averages |
      ..individual_fe,
    ,
    data = data_y,
    cluster = ~metarea,
    weights = ~perwt
  )

  urban_premium <- coef(est)["urban::1"]
  average_ln_weeklywage <- with(data_y, weighted.mean(ln_weeklywage, w = perwt))

  data_y$ln_weeklywage_resid <- data_y$ln_weeklywage -
    (predict(est, newdata = data_y) + urban_premium * data_y$urban)
  data_y$average_ln_weeklywage <- average_ln_weeklywage
  data_y$urban_premium <- urban_premium

  dataset <- rbind(
    dataset,
    data_y |>
      select(
        year,
        urban,
        ln_weeklywage_resid,
        average_ln_weeklywage,
        urban_premium
      )
  )
}

dataset <- dataset |>
  mutate(
    urban = ifelse(urban == 1, "Urban", "Non-Urban"),
    year = as.numeric(year)
  )

## 90th-10th percentile
wage_percentiles <- dataset |>
  summarize(
    .by = year,
    ln_weeklywage_resid_mean = mean(ln_weeklywage_resid, na.rm = TRUE),
    ln_weeklywage_resid_05 = quantile(ln_weeklywage_resid, 0.05, na.rm = TRUE),
    ln_weeklywage_resid_10 = quantile(ln_weeklywage_resid, 0.10, na.rm = TRUE),
    ln_weeklywage_resid_20 = quantile(ln_weeklywage_resid, 0.20, na.rm = TRUE),
    ln_weeklywage_resid_50 = quantile(ln_weeklywage_resid, 0.50, na.rm = TRUE),
    ln_weeklywage_resid_80 = quantile(ln_weeklywage_resid, 0.80, na.rm = TRUE),
    ln_weeklywage_resid_90 = quantile(ln_weeklywage_resid, 0.90, na.rm = TRUE),
    ln_weeklywage_resid_95 = quantile(ln_weeklywage_resid, 0.95, na.rm = TRUE)
  ) |>
  mutate(gap_9010 = ln_weeklywage_resid_90 - ln_weeklywage_resid_10)

## 90th-10th percentile, seperately for urban/non-urban
wage_percentiles_by_urban <- dataset |>
  summarize(
    .by = c(year, urban),
    ln_weeklywage_resid_mean = mean(ln_weeklywage_resid, na.rm = TRUE),
    ln_weeklywage_resid_05 = quantile(ln_weeklywage_resid, 0.05, na.rm = TRUE),
    ln_weeklywage_resid_10 = quantile(ln_weeklywage_resid, 0.10, na.rm = TRUE),
    ln_weeklywage_resid_20 = quantile(ln_weeklywage_resid, 0.20, na.rm = TRUE),
    ln_weeklywage_resid_50 = quantile(ln_weeklywage_resid, 0.50, na.rm = TRUE),
    ln_weeklywage_resid_80 = quantile(ln_weeklywage_resid, 0.80, na.rm = TRUE),
    ln_weeklywage_resid_90 = quantile(ln_weeklywage_resid, 0.90, na.rm = TRUE),
    ln_weeklywage_resid_95 = quantile(ln_weeklywage_resid, 0.95, na.rm = TRUE)
  ) |>
  mutate(year_pos = year - 1 + 2 * (urban == "Urban"))


# Export Results ---------------------------------------------------------------
# save(dataset, file = here("data/estimates-distribution.RData"))
# load(file = here("data/estimates-distribution.RData"))

# Plot Results -----------------------------------------------------------------
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
(plot_gap_9010 <- ggplot(wage_percentiles) +
  geom_point(
    aes(x = year, y = gap_9010),
    size = 3
  ) +
  geom_line(
    aes(x = year, y = gap_9010),
    size = 1.2
  ) +
  labs(
    y = "Log Wage Residaul, 90th - 10th",
    x = NULL,
    group = NULL,
    color = NULL
  ) +
  scale_x_continuous(breaks = seq(1940, 2020, by = 10)) +
  kfbmisc::theme_kyle(base_size = 16) +
  theme(
    axis.title.y = element_text(size = rel(0.8)),
    panel.grid.minor.x = element_blank(),
    axis.line.y = element_blank(),
    axis.ticks.y = element_blank(),
    axis.line.x = element_blank(),
    axis.ticks.x = element_blank()
  ))

# kfbmisc::ggpreview(plot_gap_9010, device = "pdf", width = 14, height = 6)
tikzsave(
  here("out/figures/distribution/gap_9010.pdf"),
  plot_gap_9010,
  width = 14,
  height = 6
)

# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
(plot_9010 <- ggplot(wage_percentiles) +
  geom_linerange(
    aes(x = year, ymin = ln_weeklywage_resid_10, ymax = ln_weeklywage_resid_90),
    size = 2,
    alpha = 0.4
  ) +
  geom_linerange(
    aes(x = year, ymin = ln_weeklywage_resid_20, ymax = ln_weeklywage_resid_80),
    size = 2,
    alpha = 0.8
  ) +
  # geom_point(
  #   aes(x = year, y = ln_weeklywage_resid_50),
  #   size = 3, color = "grey20"
  # ) +
  geom_point(
    aes(x = year, y = ln_weeklywage_resid_10),
    size = 3,
    color = "grey20"
  ) +
  geom_point(
    aes(x = year, y = ln_weeklywage_resid_90),
    size = 3,
    color = "grey20"
  ) +
  labs(
    y = "Wage Residual",
    x = NULL,
    group = NULL,
    color = NULL
  ) +
  scale_x_continuous(breaks = seq(1940, 2020, by = 10)) +
  kfbmisc::theme_kyle(base_size = 16) +
  theme(
    axis.title.y = element_text(size = rel(0.8)),
    panel.grid.minor.x = element_blank(),
    axis.line.y = element_blank(),
    axis.ticks.y = element_blank(),
    axis.line.x = element_blank(),
    axis.ticks.x = element_blank()
  ))

tikzsave(
  here("out/figures/distribution/9010.pdf"),
  plot_9010,
  width = 14,
  height = 6
)


## 90th-10th percentile by Urban/Non-urban ----
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
(plot_9010_by_urban <- ggplot(wage_percentiles_by_urban) +
  geom_linerange(
    aes(
      x = year_pos,
      ymin = ln_weeklywage_resid_10,
      ymax = ln_weeklywage_resid_90,
      color = urban,
      group = urban
    ),
    size = 2,
    alpha = 0.4
  ) +
  geom_linerange(
    aes(
      x = year_pos,
      ymin = ln_weeklywage_resid_20,
      ymax = ln_weeklywage_resid_80,
      color = urban,
      group = urban
    ),
    size = 2,
    alpha = 0.8
  ) +
  geom_point(
    aes(x = year_pos, y = ln_weeklywage_resid_90, color = urban),
    size = 3,
    color = "grey20"
  ) +
  geom_point(
    aes(x = year_pos, y = ln_weeklywage_resid_10, color = urban),
    size = 3,
    color = "grey20"
  ) +
  labs(
    y = "Wage Residual",
    x = NULL,
    group = NULL,
    color = NULL
  ) +
  scale_color_manual(
    values = c("Urban" = "grey10", "Non-Urban" = "grey40")
  ) +
  scale_x_continuous(
    breaks = seq(1940, 2020, by = 10)
  ) +
  kfbmisc::theme_kyle(base_size = 16) +
  guides(colour = guide_legend(nrow = 1)) +
  theme(
    axis.title.y = element_text(size = rel(0.8)),
    legend.position = "bottom",
    panel.grid.minor.x = element_blank(),
    axis.line.y = element_blank(),
    axis.ticks.y = element_blank(),
    axis.line.x = element_blank(),
    axis.ticks.x = element_blank()
  ))

tikzsave(
  here("out/figures/distribution/9010_urban.pdf"),
  plot_9010_by_urban,
  width = 14,
  height = 6
)


## Distributional Facet Plot ----
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
(distributions <- ggplot(dataset |> filter(abs(ln_weeklywage_resid) < 2)) +
  geom_density(
    aes(
      x = ln_weeklywage_resid,
      group = urban,
      color = urban
    ),
    size = 1.5,
    key_glyph = "path"
  ) +
  facet_wrap(~year) +
  labs(
    x = "Residaulized Log Weekly Wage",
    y = "Density",
    group = NULL,
    color = NULL
  ) +
  scale_color_manual(
    values = c(
      "Urban" = "grey10",
      "Non-Urban" = "grey40"
    )
  ) +
  kfbmisc::theme_kyle(base_size = 16) +
  guides(colour = guide_legend(nrow = 1)) +
  theme(
    axis.title.y = element_text(size = rel(0.8)),
    legend.position = "bottom",
    panel.grid.minor.x = element_blank(),
    axis.line.y = element_blank(),
    axis.ticks.y = element_blank(),
    axis.line.x = element_blank(),
    axis.ticks.x = element_blank()
  ))

tikzsave(
  here("out/figures/distribution/density.pdf"),
  distributions,
  width = 20,
  height = 14
)

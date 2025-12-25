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
  "ln_ma_removeown",
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
  ..individual_fe = ~ agegroup + educ + white
)

year_seq <- seq(1940, 2010, by = 10)

# %%
lo_ests <- map(year_seq, function(y) {
  cat(sprintf("On year: %s", y), "\n")

  data_y <- data |>
    filter(year == y) |>
    # 1970 does not have vet stat
    mutate(vetstat = if_else(year == 1970 & is.na(vetstat), 0, vetstat)) |>
    collect()

  data_y <- data_y |>
    calculate_group_averages(by = "msacode")

  ## Leave-out group average
  lo_ests <- group_averages |>
    imap(function(var, i) {
      setFixest_fml(
        ..group_averages = group_averages[-i]
      )
      est_lo <- feols(
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
      tibble(
        lo_var = group_averages[i],
        lo_est = coef(est_lo)[["urban::1"]]
      )
    }) |>
    list_rbind() |>
    mutate(year = y, .before = 1)

  return(lo_ests)
}) |>
  list_rbind()

main_ests <- map(year_seq, function(y) {
  cat(sprintf("On year: %s", y), "\n")

  data_y <- data |>
    filter(year == y) |>
    # 1970 does not have vet stat
    mutate(vetstat = if_else(year == 1970 & is.na(vetstat), 0, vetstat)) |>
    collect()

  data_y <- data_y |>
    calculate_group_averages(by = "msacode")

  ## Leave-out group average
  setFixest_fml(
    ..group_averages = group_averages
  )
  est_lo <- feols(
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
  tibble(
    lo_var = "Main",
    lo_est = coef(est_lo)[["urban::1"]]
  ) |>
    mutate(year = y, .before = 1)
}) |>
  list_rbind()

merge <- bind_rows(main_ests, lo_ests)


# %%
(plot_leave_one_out_group_averages <- ggplot() +
  geom_line(
    aes(x = year, y = lo_est),
    data = merge |> filter(lo_var == "Main"),
    color = kfbmisc::kyle_color("blue")
  ) +
  geom_point(
    aes(x = year, y = lo_est),
    data = merge |> filter(lo_var == "Main"),
    shape = 15,
    color = kfbmisc::kyle_color("blue")
  ) +
  geom_point(
    aes(x = year, y = lo_est),
    data = merge |> filter(lo_var != "Main"),
    size = 1,
    # position = position_jitter(width = 1)
  ) +
  scale_y_continuous(
    labels = scales::label_percent(suffix = "\\%"),
    limits = c(-0.05, 0.44),
    expand = c(0, 0)
  ) +
  scale_x_continuous(breaks = seq(1940, 2010, by = 10)) +
  labs(x = NULL, y = "Leave one group average out Estimate") +
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

# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
fs::dir_create(here("out/figures/robustness"))
kfbmisc::tikzsave(
  here("out/figures/robustness/leave_one_group_average_out.pdf"),
  plot_leave_one_out_group_averages,
  width = 11,
  height = 5
)

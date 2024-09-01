library(dplyr)
library(collapse)

#' Calculate group-averages using collapse *after* loading data
calculate_group_averages <- function(data, by) {
  stopifnot(by[1] == c("msacode"))
  pre <- if (length(by) == 1) {
    ""
  } else {
    sprintf("by_%s_", paste0(by[2:length(by)], collapse = "_"))
  }

  data |>
    mutate(
      .by = {{ by }},
      "{pre}share_nonwhite" := weighted.mean(white == 0, w = perwt, na.rm = TRUE),
      "{pre}share_agegroup_5" := weighted.mean(agegroup == 5, w = perwt, na.rm = TRUE),
      "{pre}share_agegroup_6" := weighted.mean(agegroup == 6, w = perwt, na.rm = TRUE),
      "{pre}share_agegroup_7" := weighted.mean(agegroup == 7, w = perwt, na.rm = TRUE),
      "{pre}share_agegroup_8" := weighted.mean(agegroup == 8, w = perwt, na.rm = TRUE),
      "{pre}share_agegroup_9" := weighted.mean(agegroup == 9, w = perwt, na.rm = TRUE),
      "{pre}share_agegroup_10" := weighted.mean(agegroup == 10, w = perwt, na.rm = TRUE),
      "{pre}share_agegroup_11" := weighted.mean(agegroup == 11, w = perwt, na.rm = TRUE),
      "{pre}share_agegroup_12" := weighted.mean(agegroup == 12, w = perwt, na.rm = TRUE),
      "{pre}share_educ_lt_hs" := weighted.mean(educ %in% 1:5, w = perwt, na.rm = TRUE),
      "{pre}share_educ_hs" := weighted.mean(educ == 6, w = perwt, na.rm = TRUE),
      "{pre}share_educ_some_college" := weighted.mean(educ %in% 7:9, w = perwt, na.rm = TRUE),
      "{pre}share_educ_college_plus" := weighted.mean(educ %in% 10:11, w = perwt, na.rm = TRUE),
      "{pre}share_marst_1" := weighted.mean(marst == 1, w = perwt, na.rm = TRUE),
      "{pre}share_marst_2" := weighted.mean(marst == 2, w = perwt, na.rm = TRUE),
      "{pre}share_marst_6" := weighted.mean(marst == 6, w = perwt, na.rm = TRUE),
      "{pre}share_vetstat_1" := weighted.mean(vetstat == 1, w = perwt, na.rm = TRUE)
    )
}

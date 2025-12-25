basic_heterogeneity_table <- function(ests) {
  # Get unique groups and years in order
  groups <- levels(ests$group)
  years <- sort(unique(ests$year))

  # Initialize output vector (2 rows per group)
  rows <- character(2 * length(groups))

  # Loop through each group
  for (i in seq_along(groups)) {
    group <- groups[i]

    # Filter data for this group and arrange by year
    group_data <- ests |>
      filter(group == !!group) |>
      arrange(year)

    # Initialize row strings with group name
    est_row <- group |> as.character()
    se_row <- " "

    # Loop through each year
    for (year in years) {
      # Find the row for this group-year combination
      year_data <- group_data |> filter(year == !!year)

      if (nrow(year_data) > 0) {
        est <- year_data$est[1]
        se <- year_data$se[1]
        pval <- 2 * (1 - pnorm(est / se))

        # Add stars based on p-value
        stars <- case_when(
          pval < 0.01 ~ "$^{***}$",
          pval < 0.05 ~ "$^{**}$",
          pval < 0.10 ~ "$^{*}$",
          .default = ""
        )

        est_str <- sprintf("%.3f%s", est, stars)
        se_str <- sprintf("(%.2f)", se)
      } else {
        est_str <- ""
        se_str <- ""
      }

      # Append to row strings
      est_row <- paste(est_row, est_str, sep = " & ")
      se_row <- paste(se_row, se_str, sep = " & ")
    }

    # Clean up se_row (remove leading " & ")
    se_row <- sub("^ & ", "", se_row)

    # Add LaTeX row terminator
    est_row <- paste0(est_row, " \\\\")
    se_row <- paste0(se_row, " \\\\")

    # Store in output
    rows[2 * i - 1] <- est_row
    rows[2 * i] <- se_row
  }

  rows
}

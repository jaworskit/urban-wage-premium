#' Loads `.dta` file chunk by chunk and writes to dataset
#' @inheritParams haven::read_dta
#' @param outfile Output directory and file to write to. This is a string template that must contain "{i}", which will be replaced with an auto-incremented integer to generate basenames of datafiles.
#' @param ... Passed to `haven::read_dta`
#' @param outfile Glue-style string with `{i}` for chunk id
#' @param chunk_size Number of rows per chunk
#' @param callback Function that is called on `current_chunk` (e.g. to filter data)
#'
dta_to_parquet_dataset <- function(
  file,
  outfile,
  chunk_size = 2.5e5,
  callback = NULL,
  ...
) {
  stopifnot(
    "callback must be a function" = is.function(callback) || is.null(callback)
  )
  stopifnot("outfile must be a string" = is.character(outfile))
  if (!grepl("\\{i\\}", outfile)) {
    stop(
      "`outfile` must be a glue-style string with `{i}` for chunk id",
      call. = FALSE
    )
  }

  # There is not a good way to know the number of rows in
  i <- 1
  cli::cli_progress_bar("Loading chunks")
  while (TRUE) {
    cli::cli_progress_update()

    # import and check
    current_chunk <- haven::read_dta(
      file = file,
      skip = chunk_size * i,
      n_max = chunk_size,
      ...
    )
    current_chunk <- haven::zap_labels(current_chunk)
    current_chunk <- haven::zap_label(current_chunk)
    current_chunk <- haven::zap_formats(current_chunk)
    if (nrow(current_chunk) == 0) break

    if (!is.null(callback)) current_chunk <- callback(current_chunk)

    # Export chunk
    outfile_i <- gsub("\\{i\\}", i, outfile)
    arrow::write_parquet(current_chunk, sink = outfile_i, ...)

    i <- i + 1
  }
  cli::cli_progress_done()
}

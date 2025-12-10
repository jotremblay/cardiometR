# S7 Methods for Data Averaging

#' @rdname average
#' @export
method(average, CpetData) <- function(x,
                                       method = c("time", "breath", "rolling"),
                                       window = 30,
                                       ...) {
  method <- match.arg(method)

  if (x@is_averaged) {
    cli::cli_warn("Data is already averaged (window: {x@averaging_window}s). Re-averaging.")
  }

  averaged_breaths <- switch(method,
    time = average_by_time(x@breaths, window),
    breath = average_by_breath(x@breaths, window),
    rolling = average_rolling(x@breaths, window)
  )

  # Return new CpetData with averaged data
  CpetData(
    participant = x@participant,
    metadata = x@metadata,
    breaths = averaged_breaths,
    stages = x@stages,
    is_averaged = TRUE,
    averaging_window = window
  )
}


#' Average Breaths by Time Windows
#'
#' Divides data into fixed time windows and calculates mean values.
#'
#' @param breaths Breath-by-breath data tibble
#' @param window Window size in seconds
#' @return Averaged tibble with one row per time window
#' @keywords internal
average_by_time <- function(breaths, window) {
  # Create time bins
  breaths <- breaths |>
    dplyr::mutate(
      time_bin = floor(time_s / window) * window + window / 2
    )

  # Identify numeric columns to average
  numeric_cols <- names(breaths)[sapply(breaths, is.numeric)]
  numeric_cols <- setdiff(numeric_cols, c("time_s", "time_bin"))

  # Group by time bin and calculate means
  averaged <- breaths |>
    dplyr::group_by(time_bin) |>
    dplyr::summarise(
      time_s = mean(time_s, na.rm = TRUE),
      dplyr::across(
        dplyr::all_of(numeric_cols),
        ~ mean(.x, na.rm = TRUE)
      ),
      # Keep first non-NA value for character columns
      dplyr::across(
        dplyr::where(is.character),
        ~ dplyr::first(stats::na.omit(.x))
      ),
      n_breaths = dplyr::n(),
      .groups = "drop"
    ) |>
    dplyr::select(-time_bin) |>
    dplyr::arrange(time_s)

  tibble::as_tibble(averaged)
}


#' Average Breaths by Fixed Number of Breaths
#'
#' Groups consecutive breaths and calculates mean values.
#'
#' @param breaths Breath-by-breath data tibble
#' @param window Number of breaths per average
#' @return Averaged tibble
#' @keywords internal
average_by_breath <- function(breaths, window) {
  n_rows <- nrow(breaths)
  n_groups <- ceiling(n_rows / window)

  # Create breath groups
  breaths <- breaths |>
    dplyr::mutate(
      breath_group = rep(1:n_groups, each = window, length.out = n_rows)
    )

  # Identify numeric columns
  numeric_cols <- names(breaths)[sapply(breaths, is.numeric)]
  numeric_cols <- setdiff(numeric_cols, "breath_group")

  # Group and average
  averaged <- breaths |>
    dplyr::group_by(breath_group) |>
    dplyr::summarise(
      dplyr::across(
        dplyr::all_of(numeric_cols),
        ~ mean(.x, na.rm = TRUE)
      ),
      dplyr::across(
        dplyr::where(is.character),
        ~ dplyr::first(stats::na.omit(.x))
      ),
      n_breaths = dplyr::n(),
      .groups = "drop"
    ) |>
    dplyr::select(-breath_group) |>
    dplyr::arrange(time_s)

  tibble::as_tibble(averaged)
}


#' Rolling/Moving Average
#'
#' Applies a rolling mean with specified window size.
#'
#' @param breaths Breath-by-breath data tibble
#' @param window Window size in seconds
#' @return Tibble with rolling averages
#' @keywords internal
average_rolling <- function(breaths, window) {
  # Calculate average time between breaths to determine k
  if (nrow(breaths) < 2) {
    return(breaths)
  }

  avg_interval <- mean(diff(breaths$time_s), na.rm = TRUE)
  k <- max(1, round(window / avg_interval))

  # Identify numeric columns
  numeric_cols <- names(breaths)[sapply(breaths, is.numeric)]

  # Apply rolling mean to numeric columns
  averaged <- breaths |>
    dplyr::mutate(
      dplyr::across(
        dplyr::all_of(numeric_cols),
        ~ zoo::rollmean(.x, k = k, fill = NA, align = "center")
      )
    ) |>
    # Remove rows with NA from rolling average edges
    dplyr::filter(!is.na(vo2_ml))

  tibble::as_tibble(averaged)
}

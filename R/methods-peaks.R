# S7 Methods for Peak Value Determination

#' @name find_peaks
#' @rdname find_peaks
#' @export
method(find_peaks, CpetData) <- function(x, averaging = 30, ...) {
  breaths <- x@breaths
  weight_kg <- x@participant@weight_kg

  # Apply rolling average for peak determination
  if (nrow(breaths) < 3) {
    cli::cli_abort("Insufficient data for peak determination")
  }

  # Calculate window size in number of breaths
  avg_interval <- mean(diff(breaths$time_s), na.rm = TRUE)
  k <- max(3, round(averaging / avg_interval))
  k <- min(k, nrow(breaths))  # Can't exceed data length

  # Apply rolling average to key variables
  vo2_smooth <- zoo::rollmean(breaths$vo2_ml, k = k, fill = NA, align = "center")
  vco2_smooth <- zoo::rollmean(breaths$vco2_ml, k = k, fill = NA, align = "center")
  ve_smooth <- zoo::rollmean(breaths$ve_l, k = k, fill = NA, align = "center")
  rer_smooth <- zoo::rollmean(breaths$rer, k = k, fill = NA, align = "center")

  # Find peak values
  vo2_peak <- max(vo2_smooth, na.rm = TRUE)
  vo2_kg_peak <- vo2_peak / weight_kg
  ve_peak <- max(ve_smooth, na.rm = TRUE)
  rer_peak <- max(rer_smooth, na.rm = TRUE)

  # Optional HR peak
  hr_peak <- NULL
  if ("hr_bpm" %in% names(breaths) && !all(is.na(breaths$hr_bpm))) {
    hr_smooth <- zoo::rollmean(breaths$hr_bpm, k = k, fill = NA, align = "center")
    hr_peak <- max(hr_smooth, na.rm = TRUE)
  }

  # Optional power peak
  power_peak <- NULL
  if ("power_w" %in% names(breaths) && !all(is.na(breaths$power_w))) {
    # Power is typically already stable, but smooth anyway
    power_smooth <- zoo::rollmean(breaths$power_w, k = k, fill = NA, align = "center")
    power_peak <- max(power_smooth, na.rm = TRUE)
  }

  PeakValues(
    vo2_peak = vo2_peak,
    vo2_kg_peak = vo2_kg_peak,
    ve_peak = ve_peak,
    rer_peak = rer_peak,
    averaging_s = averaging,
    hr_peak = hr_peak,
    power_peak = power_peak
  )
}


#' Find Time to Peak VO2
#'
#' Determines the time point at which peak VO2 occurred.
#'
#' @param x A CpetData object
#' @param averaging Averaging window in seconds
#'
#' @return Numeric time in seconds at peak VO2
#'
#' @export
find_time_to_peak <- function(x, averaging = 30) {
  breaths <- x@breaths

  # Calculate rolling average
  avg_interval <- mean(diff(breaths$time_s), na.rm = TRUE)
  k <- max(3, round(averaging / avg_interval))
  k <- min(k, nrow(breaths))

  vo2_smooth <- zoo::rollmean(breaths$vo2_ml, k = k, fill = NA, align = "center")

  # Find index of peak
  peak_idx <- which.max(vo2_smooth)

  breaths$time_s[peak_idx]
}


#' Calculate VO2 Plateau
#'
#' Checks if a VO2 plateau was achieved (< 150 mL/min increase
#' in final stage despite increased workload).
#'
#' @param x A CpetData object with stages
#' @param threshold Maximum VO2 increase to consider a plateau (default 150 mL/min)
#'
#' @return List with plateau status and details
#'
#' @export
check_vo2_plateau <- function(x, threshold = 150) {
  if (is.null(x@stages) || nrow(x@stages) < 2) {
    return(list(
      plateau = NA,
      message = "Insufficient stage data to assess plateau"
    ))
  }

  breaths <- x@breaths
  stages <- x@stages

  # Get last two stages
  stage_summary <- breaths |>
    dplyr::left_join(stages, by = "time_s") |>
    dplyr::group_by(stage) |>
    dplyr::summarise(
      mean_vo2 = mean(vo2_ml, na.rm = TRUE),
      mean_power = mean(power_w, na.rm = TRUE),
      .groups = "drop"
    ) |>
    dplyr::filter(!is.na(stage)) |>
    dplyr::arrange(stage)

  if (nrow(stage_summary) < 2) {
    return(list(
      plateau = NA,
      message = "Insufficient complete stages"
    ))
  }

  # Compare last two stages
  n <- nrow(stage_summary)
  vo2_diff <- stage_summary$mean_vo2[n] - stage_summary$mean_vo2[n - 1]
  power_diff <- stage_summary$mean_power[n] - stage_summary$mean_power[n - 1]

  plateau_achieved <- vo2_diff < threshold && power_diff > 0

  list(
    plateau = plateau_achieved,
    vo2_increase = vo2_diff,
    power_increase = power_diff,
    message = if (plateau_achieved) {
      sprintf("VO2 plateau achieved (increase of %.0f mL/min < %.0f threshold)",
              vo2_diff, threshold)
    } else {
      sprintf("No plateau (VO2 increased by %.0f mL/min)", vo2_diff)
    }
  )
}

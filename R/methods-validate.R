# S7 Methods for Data Validation
# Based on ATS/ACCP CPET Guidelines

#' @rdname validate
#' @export
method(validate, CpetData) <- function(x,
                                        rest_duration_min = 2,
                                        aberrant_threshold = 3,
                                        ...) {
  errors <- list()
  warnings <- list()
  info <- list()

  breaths <- x@breaths
  participant <- x@participant

  # 1. Check minimum data requirements
  if (nrow(breaths) < 10) {
    errors <- c(errors, "Insufficient data: fewer than 10 breaths recorded")
  }

  # 2. Check test duration
 test_duration_min <- (max(breaths$time_s) - min(breaths$time_s)) / 60
  if (test_duration_min < 5) {
    warnings <- c(warnings,
      sprintf("Short test duration: %.1f minutes (minimum 5-8 min recommended)", test_duration_min))
  }
  info <- c(info, sprintf("Test duration: %.1f minutes", test_duration_min))

  # 3. Check for rest phase
  rest_check <- check_rest_phase(breaths, rest_duration_min, participant@weight_kg)
  errors <- c(errors, rest_check$errors)
  warnings <- c(warnings, rest_check$warnings)
  info <- c(info, rest_check$info)

  # 4. Check physiological plausibility
  physio_check <- check_physiological_ranges(breaths, participant)
  errors <- c(errors, physio_check$errors)
  warnings <- c(warnings, physio_check$warnings)
  info <- c(info, physio_check$info)

  # 5. Check for aberrant breaths
  aberrant_check <- check_aberrant_breaths(breaths, aberrant_threshold)
  warnings <- c(warnings, aberrant_check$warnings)
  info <- c(info, aberrant_check$info)

  # 6. Check data completeness
  completeness_check <- check_data_completeness(breaths)
  warnings <- c(warnings, completeness_check$warnings)
  info <- c(info, completeness_check$info)

  # 7. Check maximal effort indicators
  effort_check <- check_maximal_effort(breaths, participant@age)
  info <- c(info, effort_check$info)

  ValidationReport(
    is_valid = length(errors) == 0,
    errors = errors,
    warnings = warnings,
    info = info
  )
}


#' Check Rest Phase Adequacy
#'
#' @param breaths Breath data tibble
#' @param min_duration Minimum rest duration in minutes
#' @param weight_kg Participant weight for predicted resting VO2
#' @return List with errors, warnings, info
#' @keywords internal
check_rest_phase <- function(breaths, min_duration, weight_kg) {
  errors <- list()
  warnings <- list()
  info <- list()

  # Check if phase column exists and has REST
  if ("phase" %in% names(breaths)) {
    rest_data <- breaths |>
      dplyr::filter(tolower(phase) == "rest")

    if (nrow(rest_data) == 0) {
      warnings <- c(warnings, "No REST phase identified in data")
    } else {
      rest_duration <- (max(rest_data$time_s) - min(rest_data$time_s)) / 60
      if (rest_duration < min_duration) {
        warnings <- c(warnings,
          sprintf("Rest phase duration (%.1f min) is less than recommended (%.0f min)",
                  rest_duration, min_duration))
      }

      # Check resting VO2 (should be ~3.5 mL/kg/min)
      predicted_rest_vo2 <- 3.5 * weight_kg
      actual_rest_vo2 <- mean(rest_data$vo2_ml, na.rm = TRUE)

      if (actual_rest_vo2 < predicted_rest_vo2 * 0.5) {
        warnings <- c(warnings,
          sprintf("Resting VO2 (%.0f mL/min) is unusually low (predicted: %.0f mL/min)",
                  actual_rest_vo2, predicted_rest_vo2))
      } else if (actual_rest_vo2 > predicted_rest_vo2 * 1.5) {
        warnings <- c(warnings,
          sprintf("Resting VO2 (%.0f mL/min) is unusually high (predicted: %.0f mL/min)",
                  actual_rest_vo2, predicted_rest_vo2))
      }

      # Check resting RER
      rest_rer <- mean(rest_data$rer, na.rm = TRUE)
      if (rest_rer < 0.70 || rest_rer > 0.90) {
        warnings <- c(warnings,
          sprintf("Resting RER (%.2f) outside normal range (0.70-0.90)", rest_rer))
      }

      info <- c(info, sprintf("Rest phase: %.1f min, VO2=%.0f mL/min, RER=%.2f",
                              rest_duration, actual_rest_vo2, rest_rer))
    }
  } else {
    # No phase column - use first few minutes as proxy
    early_data <- breaths |>
      dplyr::filter(time_s < min(time_s) + min_duration * 60)

    if (nrow(early_data) > 0) {
      early_rer <- mean(early_data$rer, na.rm = TRUE)
      info <- c(info, sprintf("Early test RER: %.2f (no phase data available)", early_rer))
    }
  }

  list(errors = errors, warnings = warnings, info = info)
}


#' Check Physiological Plausibility
#'
#' @param breaths Breath data tibble
#' @param participant Participant S7 object
#' @return List with errors, warnings, info
#' @keywords internal
check_physiological_ranges <- function(breaths, participant) {
  errors <- list()
  warnings <- list()
  info <- list()

  # VO2 range check (0-8000 mL/min is extremely generous)
  vo2_min <- min(breaths$vo2_ml, na.rm = TRUE)
  vo2_max <- max(breaths$vo2_ml, na.rm = TRUE)

  if (vo2_min < 0) {
    errors <- c(errors, sprintf("Negative VO2 values detected (min: %.0f)", vo2_min))
  }
  if (vo2_max > 7000) {
    warnings <- c(warnings,
      sprintf("Very high VO2 detected (%.0f mL/min) - verify calibration", vo2_max))
  }

  # RER range check
  rer_min <- min(breaths$rer, na.rm = TRUE)
  rer_max <- max(breaths$rer, na.rm = TRUE)

  if (rer_min < 0.5 || rer_max > 1.5) {
    warnings <- c(warnings,
      sprintf("RER outside typical range (%.2f - %.2f)", rer_min, rer_max))
  }

  # HR check if available
  if ("hr_bpm" %in% names(breaths) && !all(is.na(breaths$hr_bpm))) {
    hr_min <- min(breaths$hr_bpm, na.rm = TRUE)
    hr_max <- max(breaths$hr_bpm, na.rm = TRUE)

    if (hr_min < 30) {
      warnings <- c(warnings, sprintf("Very low HR detected (%.0f bpm)", hr_min))
    }
    if (hr_max > 220) {
      warnings <- c(warnings, sprintf("Very high HR detected (%.0f bpm)", hr_max))
    }

    # Check against age-predicted max
    predicted_hr_max <- 220 - participant@age
    if (hr_max > predicted_hr_max + 20) {
      info <- c(info,
        sprintf("Peak HR (%.0f) exceeds age-predicted max (%.0f) by >20 bpm",
                hr_max, predicted_hr_max))
    }
  }

  # VE check
  ve_max <- max(breaths$ve_l, na.rm = TRUE)
  if (ve_max > 200) {
    warnings <- c(warnings,
      sprintf("Very high VE detected (%.1f L/min) - verify data", ve_max))
  }

  info <- c(info, sprintf("VO2 range: %.0f - %.0f mL/min", vo2_min, vo2_max))
  info <- c(info, sprintf("RER range: %.2f - %.2f", rer_min, rer_max))

  list(errors = errors, warnings = warnings, info = info)
}


#' Check for Aberrant Breaths
#'
#' Identifies potential coughs, swallows, or equipment artifacts.
#'
#' @param breaths Breath data tibble
#' @param threshold SD threshold for outlier detection
#' @return List with warnings, info
#' @keywords internal
check_aberrant_breaths <- function(breaths, threshold = 3) {
  warnings <- list()
  info <- list()

  # Calculate rolling statistics for VO2
  if (nrow(breaths) < 10) {
    return(list(warnings = warnings, info = info))
  }

  # Use a simple approach: flag values > threshold SDs from local mean
  k <- min(11, nrow(breaths))  # Window size for local stats

  local_mean <- zoo::rollmean(breaths$vo2_ml, k = k, fill = NA, align = "center")
  local_sd <- zoo::rollapply(breaths$vo2_ml, width = k, FUN = sd, fill = NA, align = "center")

  # Identify aberrant breaths
  z_scores <- abs(breaths$vo2_ml - local_mean) / local_sd
  n_aberrant <- sum(z_scores > threshold, na.rm = TRUE)

  if (n_aberrant > 0) {
    pct_aberrant <- 100 * n_aberrant / nrow(breaths)
    if (pct_aberrant > 5) {
      warnings <- c(warnings,
        sprintf("%.1f%% of breaths flagged as potentially aberrant (n=%d)",
                pct_aberrant, n_aberrant))
    } else {
      info <- c(info,
        sprintf("%.1f%% of breaths potentially aberrant (n=%d) - within acceptable range",
                pct_aberrant, n_aberrant))
    }
  }

  list(warnings = warnings, info = info)
}


#' Check Data Completeness
#'
#' @param breaths Breath data tibble
#' @return List with warnings, info
#' @keywords internal
check_data_completeness <- function(breaths) {
  warnings <- list()
  info <- list()

  # Check for missing HR
  if ("hr_bpm" %in% names(breaths)) {
    pct_missing_hr <- 100 * sum(is.na(breaths$hr_bpm)) / nrow(breaths)
    if (pct_missing_hr > 10) {
      warnings <- c(warnings,
        sprintf("%.1f%% of HR data is missing", pct_missing_hr))
    }
  } else {
    info <- c(info, "No HR data available")
  }

  # Check for missing power
  if ("power_w" %in% names(breaths)) {
    pct_missing_power <- 100 * sum(is.na(breaths$power_w)) / nrow(breaths)
    if (pct_missing_power > 10) {
      info <- c(info, sprintf("%.1f%% of power data is missing", pct_missing_power))
    }
  }

  # Check required columns
  required <- c("time_s", "vo2_ml", "vco2_ml", "ve_l", "rer")
  for (col in required) {
    if (col %in% names(breaths)) {
      pct_missing <- 100 * sum(is.na(breaths[[col]])) / nrow(breaths)
      if (pct_missing > 0) {
        warnings <- c(warnings, sprintf("%.1f%% of %s data is missing", pct_missing, col))
      }
    }
  }

  list(warnings = warnings, info = info)
}


#' Check Maximal Effort Indicators
#'
#' @param breaths Breath data tibble
#' @param age Participant age
#' @return List with info messages about effort level
#' @keywords internal
check_maximal_effort <- function(breaths, age) {
  info <- list()

  # Peak RER > 1.10 suggests maximal effort
  peak_rer <- max(breaths$rer, na.rm = TRUE)
  if (peak_rer >= 1.15) {
    info <- c(info, sprintf("Peak RER = %.2f: Strong evidence of maximal effort", peak_rer))
  } else if (peak_rer >= 1.10) {
    info <- c(info, sprintf("Peak RER = %.2f: Good evidence of maximal effort", peak_rer))
  } else if (peak_rer >= 1.05) {
    info <- c(info, sprintf("Peak RER = %.2f: Moderate effort (consider if test was maximal)", peak_rer))
  } else {
    info <- c(info, sprintf("Peak RER = %.2f: Submaximal effort", peak_rer))
  }

  # HR relative to predicted max
  if ("hr_bpm" %in% names(breaths) && !all(is.na(breaths$hr_bpm))) {
    peak_hr <- max(breaths$hr_bpm, na.rm = TRUE)
    predicted_max_hr <- 220 - age
    pct_predicted <- 100 * peak_hr / predicted_max_hr

    if (pct_predicted >= 95) {
      info <- c(info, sprintf("Peak HR = %.0f bpm (%.0f%% of predicted max): Maximal", peak_hr, pct_predicted))
    } else if (pct_predicted >= 85) {
      info <- c(info, sprintf("Peak HR = %.0f bpm (%.0f%% of predicted max): Near-maximal", peak_hr, pct_predicted))
    } else {
      info <- c(info, sprintf("Peak HR = %.0f bpm (%.0f%% of predicted max): Submaximal", peak_hr, pct_predicted))
    }
  }

  list(info = info)
}

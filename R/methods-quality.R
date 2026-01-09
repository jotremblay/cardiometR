# S7 Methods for Quality Assessment
# Based on ACSM Guidelines for Exercise Testing and Prescription (11th edition)
# and ATS/ACCP CPET Guidelines

#' @importFrom stats lm coef
#' @keywords internal
NULL

#' Get Quality Rating from Score
#'
#' @param score Numeric score (0-100)
#' @param poor_label Label for poor rating (default "poor")
#' @return Character rating
#' @keywords internal
get_rating <- function(score, poor_label = "poor") {
  if (score >= 90) "excellent"
  else if (score >= 75) "good"
  else if (score >= 60) "acceptable"
  else poor_label
}

# assess_maximal_criteria --------------------------------------------------

#' @rdname assess_maximal_criteria
method(assess_maximal_criteria, CpetData) <- function(x,
                                                       rpe = NULL,
                                                       lactate = NULL,
                                                       rer_threshold = 1.10,
                                                       hr_threshold_pct = 85,
                                                       plateau_threshold = 150,
                                                       ...) {
  breaths <- x@breaths
  age <- x@participant@age


  # --- RER Criterion ---
  peak_rer <- max(breaths$rer, na.rm = TRUE)
  rer_met <- peak_rer >= rer_threshold

  # --- HR Criterion ---
  peak_hr <- NULL
  predicted_hr_max <- NULL
  hr_pct_predicted <- NULL
  hr_met <- NULL

  if ("hr_bpm" %in% names(breaths) && !all(is.na(breaths$hr_bpm))) {
    peak_hr <- max(breaths$hr_bpm, na.rm = TRUE)
    predicted_hr_max <- 220 - age
    hr_pct_predicted <- 100 * peak_hr / predicted_hr_max
    hr_met <- hr_pct_predicted >= hr_threshold_pct
  }

  # --- VO2 Plateau Criterion ---
  plateau_result <- detect_vo2_plateau(breaths, threshold = plateau_threshold)
  vo2_plateau_detected <- plateau_result$detected
  vo2_plateau_delta <- plateau_result$delta
  plateau_met <- vo2_plateau_detected

  # --- RPE Criterion (optional) ---
  rpe_met <- NULL
  if (!is.null(rpe)) {
    rpe_met <- rpe >= 17
  }

  # --- Lactate Criterion (optional) ---
  lactate_met <- NULL
  if (!is.null(lactate)) {
    lactate_met <- lactate >= 8.0
  }

  # --- Count Criteria ---
  criteria_results <- c(rer_met, hr_met, plateau_met, rpe_met, lactate_met)
  criteria_met <- sum(criteria_results, na.rm = TRUE)
  criteria_available <- sum(!is.na(criteria_results))

  # --- Determination ---
  # Need at least 2 of 3 primary criteria (RER, HR, plateau) for maximal
  # Or 3+ total criteria met
  is_maximal <- criteria_met >= 3 || (criteria_met >= 2 && rer_met)

  determination <- if (criteria_met >= 3) {
    "maximal"
  } else if (criteria_met == 2 && rer_met) {
    "likely_maximal"
  } else if (criteria_met >= 1) {
    "submaximal"
  } else {
    "indeterminate"
  }

  ExerciseQualityCriteria(
    peak_rer = peak_rer,
    rer_met = rer_met,
    peak_hr = peak_hr,
    predicted_hr_max = predicted_hr_max,
    hr_pct_predicted = hr_pct_predicted,
    hr_met = hr_met,
    vo2_plateau_detected = vo2_plateau_detected,
    vo2_plateau_delta = vo2_plateau_delta,
    plateau_met = plateau_met,
    rpe_reported = rpe,
    rpe_met = rpe_met,
    lactate_mmol = lactate,
    lactate_met = lactate_met,
    criteria_met = as.integer(criteria_met),
    criteria_available = as.integer(criteria_available),
    is_maximal = is_maximal,
    determination = determination
  )
}


#' Detect VO2 Plateau
#'
#' Identifies if a VO2 plateau occurred at the end of the test.
#' A plateau is defined as < threshold mL/min increase in VO2
#' despite continued increase in workload.
#'
#' @param breaths Breath data tibble
#' @param threshold Maximum VO2 increase to consider a plateau (default 150 mL/min)
#' @param window_s Averaging window in seconds (default 30)
#' @return List with detected (logical) and delta (numeric)
#' @keywords internal
detect_vo2_plateau <- function(breaths, threshold = 150, window_s = 30) {
  n <- nrow(breaths)

  if (n < 60) {
    return(list(detected = FALSE, delta = NA_real_))
  }

  # Use rolling average to smooth data
  k <- min(30, n %/% 4)  # Window for rolling average

  if (k < 5) {
    return(list(detected = FALSE, delta = NA_real_))
  }

  vo2_smooth <- zoo::rollmean(breaths$vo2_ml, k = k, fill = NA, align = "right")

  # Look at the last 20% of the test
  end_start <- floor(n * 0.8)
  end_vo2 <- vo2_smooth[end_start:n]
  end_vo2 <- end_vo2[!is.na(end_vo2)]

  if (length(end_vo2) < 10) {
    return(list(detected = FALSE, delta = NA_real_))
  }

  # Check if there's power data to confirm workload increased
  has_power <- "power_w" %in% names(breaths) && !all(is.na(breaths$power_w))

  if (has_power) {
    power_smooth <- zoo::rollmean(breaths$power_w, k = k, fill = NA, align = "right")
    end_power <- power_smooth[end_start:n]
    end_power <- end_power[!is.na(end_power)]

    # Check if power increased while VO2 plateaued
    if (length(end_power) >= 10) {
      power_increased <- max(end_power) - min(end_power) > 10  # At least 10W increase
      vo2_delta <- max(end_vo2) - min(end_vo2)
      plateau_detected <- power_increased && vo2_delta < threshold

      return(list(detected = plateau_detected, delta = vo2_delta))
    }
  }

  # Without power data, look for leveling off of VO2
  # Compare last 10% to previous 10%
  mid_point <- floor(length(end_vo2) / 2)
  first_half <- mean(end_vo2[1:mid_point], na.rm = TRUE)
  second_half <- mean(end_vo2[(mid_point + 1):length(end_vo2)], na.rm = TRUE)
  vo2_delta <- second_half - first_half

  # Plateau if VO2 increase < threshold
  plateau_detected <- vo2_delta < threshold && vo2_delta >= 0

  list(detected = plateau_detected, delta = vo2_delta)
}


# assess_protocol_quality --------------------------------------------------

#' @rdname assess_protocol_quality
method(assess_protocol_quality, CpetData) <- function(x,
                                                       modality = "auto",
                                                       expected_slope = NULL,
                                                       ...) {
  breaths <- x@breaths

  # --- Determine Modality ---
  if (modality == "auto") {
    modality <- detect_modality(breaths, x@metadata@protocol)
  }

  # --- Expected VO2/Workload Slope ---
  if (is.null(expected_slope)) {
    expected_slope <- if (modality == "cycling") {
      10.0  # mL/min/W (Wasserman standard)
    } else if (modality == "treadmill") {
      NULL  # Treadmill uses metabolic equations, not simple slope
    } else {
      NULL
    }
  }

  # --- Test Duration ---
  test_duration_s <- max(breaths$time_s, na.rm = TRUE) - min(breaths$time_s, na.rm = TRUE)

  # --- Exercise Duration ---
  exercise_result <- calculate_exercise_duration(breaths)
  exercise_duration_s <- exercise_result$duration_s
  warmup_duration_s <- exercise_result$warmup_s

  duration_optimal <- !is.null(exercise_duration_s) &&
    exercise_duration_s >= 480 &&  # 8 min
    exercise_duration_s <= 720      # 12 min

  warmup_adequate <- !is.null(warmup_duration_s) && warmup_duration_s >= 120  # 2 min

  # --- VO2/Workload Analysis ---
  slope_result <- analyze_vo2_workload_relationship(breaths, expected_slope, modality)

  actual_vo2_slope <- slope_result$actual_slope
  slope_deviation_pct <- slope_result$deviation_pct
  slope_acceptable <- slope_result$acceptable
  r_squared <- slope_result$r_squared
  r_squared_acceptable <- !is.null(r_squared) && r_squared >= 0.90
  stage_details <- slope_result$stage_details

  # --- Stage Consistency ---
  stage_result <- analyze_stage_consistency(breaths)
  n_stages <- stage_result$n_stages
  stage_cv <- stage_result$cv
  stage_cv_acceptable <- !is.null(stage_cv) && stage_cv <= 10

  # --- Overall Score ---
  score_components <- c(
    if (!is.null(slope_acceptable) && slope_acceptable) 25 else if (!is.null(slope_acceptable)) 10 else 15,
    if (!is.null(r_squared_acceptable) && r_squared_acceptable) 25 else if (!is.null(r_squared)) 10 else 15,
    if (!is.null(stage_cv_acceptable) && stage_cv_acceptable) 15 else if (!is.null(stage_cv)) 5 else 10,
    if (!is.null(duration_optimal) && duration_optimal) 20 else 10,
    if (!is.null(warmup_adequate) && warmup_adequate) 15 else 5
  )
  overall_score <- sum(score_components)

  # --- Overall Rating ---
  overall_rating <- if (overall_score >= 40) {
    get_rating(overall_score)
  } else {
    "unable_to_assess"
  }

  ProtocolQuality(
    modality = modality,
    expected_vo2_slope = expected_slope,
    actual_vo2_slope = actual_vo2_slope,
    slope_deviation_pct = slope_deviation_pct,
    slope_acceptable = slope_acceptable,
    r_squared = r_squared,
    r_squared_acceptable = r_squared_acceptable,
    n_stages = n_stages,
    stage_cv = stage_cv,
    stage_cv_acceptable = stage_cv_acceptable,
    test_duration_s = test_duration_s,
    exercise_duration_s = exercise_duration_s,
    duration_optimal = duration_optimal,
    warmup_duration_s = warmup_duration_s,
    warmup_adequate = warmup_adequate,
    overall_rating = overall_rating,
    overall_score = overall_score,
    stage_details = stage_details
  )
}


#' Detect Exercise Modality
#'
#' @param breaths Breath data tibble
#' @param protocol Protocol string from metadata
#' @return Character: "cycling", "treadmill", or "unknown"
#' @keywords internal
detect_modality <- function(breaths, protocol) {
  protocol_lower <- tolower(protocol)

  # Check protocol name
  if (grepl("cycle|bike|ergometer|watt", protocol_lower)) {
    return("cycling")
  }
  if (grepl("treadmill|bruce|balke|walk|run", protocol_lower)) {
    return("treadmill")
  }

  # Check if power data exists (suggests cycling)
  if ("power_w" %in% names(breaths) && !all(is.na(breaths$power_w))) {
    power_values <- breaths$power_w[!is.na(breaths$power_w)]
    if (length(power_values) > 10 && max(power_values) > 20) {
      return("cycling")
    }
  }

  "unknown"
}


#' Calculate Exercise Duration
#'
#' @param breaths Breath data tibble
#' @return List with duration_s and warmup_s
#' @keywords internal
calculate_exercise_duration <- function(breaths) {
  # If phase column exists
  if ("phase" %in% names(breaths)) {
    phases <- tolower(breaths$phase)

    # Find exercise phase
    exercise_rows <- which(phases %in% c("exercise", "ex", "work", "load"))
    if (length(exercise_rows) > 0) {
      exercise_duration <- breaths$time_s[max(exercise_rows)] - breaths$time_s[min(exercise_rows)]
    } else {
      exercise_duration <- NULL
    }

    # Find warmup/rest phase
    warmup_rows <- which(phases %in% c("rest", "warmup", "warm-up", "baseline"))
    if (length(warmup_rows) > 0) {
      warmup_duration <- breaths$time_s[max(warmup_rows)] - breaths$time_s[min(warmup_rows)]
    } else {
      warmup_duration <- NULL
    }

    return(list(duration_s = exercise_duration, warmup_s = warmup_duration))
  }

  # Estimate from power data
  if ("power_w" %in% names(breaths) && !all(is.na(breaths$power_w))) {
    # Exercise = when power > 20W
    exercise_rows <- which(breaths$power_w > 20)
    if (length(exercise_rows) > 10) {
      exercise_duration <- breaths$time_s[max(exercise_rows)] - breaths$time_s[min(exercise_rows)]
      warmup_duration <- breaths$time_s[min(exercise_rows)] - min(breaths$time_s)
      return(list(duration_s = exercise_duration, warmup_s = warmup_duration))
    }
  }

  # Default: assume first 2 minutes is warmup
  total_duration <- max(breaths$time_s) - min(breaths$time_s)
  list(duration_s = total_duration - 120, warmup_s = 120)
}


#' Analyze VO2/Workload Relationship
#'
#' @param breaths Breath data tibble
#' @param expected_slope Expected slope (mL/min/W)
#' @param modality Exercise modality
#' @return List with actual_slope, deviation_pct, acceptable, r_squared, stage_details
#' @keywords internal
analyze_vo2_workload_relationship <- function(breaths, expected_slope, modality) {
  result <- list(
    actual_slope = NULL,
    deviation_pct = NULL,
    acceptable = NULL,
    r_squared = NULL,
    stage_details = NULL
  )

  # Need power data for cycling analysis
  if (modality != "cycling" || !"power_w" %in% names(breaths)) {
    return(result)
  }

  power <- breaths$power_w
  vo2 <- breaths$vo2_ml

  # Filter to exercise phase (power > 20W)
  exercise_idx <- which(power > 20 & !is.na(power) & !is.na(vo2))

  if (length(exercise_idx) < 30) {
    return(result)
  }

  exercise_power <- power[exercise_idx]
  exercise_vo2 <- vo2[exercise_idx]

  # Linear regression: VO2 = intercept + slope * Power
  fit <- tryCatch({
    lm(exercise_vo2 ~ exercise_power)
  }, error = function(e) NULL)

  if (is.null(fit)) {
    return(result)
  }

  result$actual_slope <- coef(fit)[2]
  result$r_squared <- summary(fit)$r.squared

  if (!is.null(expected_slope)) {
    result$deviation_pct <- 100 * (result$actual_slope - expected_slope) / expected_slope
    result$acceptable <- abs(result$deviation_pct) <= 15  # +/- 15% is acceptable
  }

  # Stage-by-stage analysis
  result$stage_details <- calculate_stage_expected_vo2(breaths, expected_slope)

  result
}


#' Calculate Expected VO2 per Stage
#'
#' @param breaths Breath data tibble
#' @param expected_slope Expected slope (mL/min/W)
#' @return Data frame with stage, power_w, expected_vo2, actual_vo2, deviation_pct
#' @keywords internal
calculate_stage_expected_vo2 <- function(breaths, expected_slope) {
  if (is.null(expected_slope) || !"power_w" %in% names(breaths)) {
    return(NULL)
  }

  # Group by power levels (rounded to nearest 25W)
  breaths_with_stage <- breaths |>
    dplyr::filter(!is.na(power_w), power_w > 0) |>
    dplyr::mutate(stage_power = round(power_w / 25) * 25) |>
    dplyr::filter(stage_power > 0)

  if (nrow(breaths_with_stage) == 0) {
    return(NULL)
  }

  # Summarize by stage
  stage_summary <- breaths_with_stage |>
    dplyr::group_by(stage_power) |>
    dplyr::summarize(
      actual_vo2 = mean(vo2_ml, na.rm = TRUE),
      n_breaths = dplyr::n(),
      .groups = "drop"
    ) |>
    dplyr::filter(n_breaths >= 5) |>  # Need at least 5 breaths per stage
    dplyr::mutate(
      # Expected VO2 = resting VO2 (250 mL/min) + slope * power
      expected_vo2 = 250 + expected_slope * stage_power,
      deviation_pct = 100 * (actual_vo2 - expected_vo2) / expected_vo2
    ) |>
    dplyr::rename(power_w = stage_power)

  as.data.frame(stage_summary)
}


#' Analyze Stage Consistency
#'
#' @param breaths Breath data tibble
#' @return List with n_stages and cv (coefficient of variation)
#' @keywords internal
analyze_stage_consistency <- function(breaths) {
  result <- list(n_stages = NULL, cv = NULL)

  if (!"power_w" %in% names(breaths) || all(is.na(breaths$power_w))) {
    return(result)
  }

  # Group by power levels
  breaths_with_stage <- breaths |>
    dplyr::filter(!is.na(power_w), power_w > 0) |>
    dplyr::mutate(stage_power = round(power_w / 25) * 25) |>
    dplyr::filter(stage_power > 0)

  if (nrow(breaths_with_stage) == 0) {
    return(result)
  }

  # Calculate CV within each stage
  stage_stats <- breaths_with_stage |>
    dplyr::group_by(stage_power) |>
    dplyr::summarize(
      n = dplyr::n(),
      mean_vo2 = mean(vo2_ml, na.rm = TRUE),
      sd_vo2 = sd(vo2_ml, na.rm = TRUE),
      cv = 100 * sd_vo2 / mean_vo2,
      .groups = "drop"
    ) |>
    dplyr::filter(n >= 5)

  if (nrow(stage_stats) == 0) {
    return(result)
  }

  result$n_stages <- as.integer(nrow(stage_stats))
  result$cv <- mean(stage_stats$cv, na.rm = TRUE)

  result
}


# assess_data_quality ------------------------------------------------------

#' @rdname assess_data_quality
method(assess_data_quality, CpetData) <- function(x,
                                                   aberrant_threshold = 3,
                                                   ...) {
  breaths <- x@breaths
  n_breaths <- nrow(breaths)

  # --- Aberrant Breath Detection ---
  aberrant_result <- detect_aberrant_breaths_detailed(breaths, aberrant_threshold)
  n_aberrant <- aberrant_result$count
  pct_aberrant <- 100 * n_aberrant / n_breaths
  aberrant_acceptable <- pct_aberrant <= 5
  artifact_timestamps <- aberrant_result$timestamps

  # --- Missing HR ---
  n_missing_hr <- NULL
  pct_missing_hr <- NULL
  hr_acceptable <- NULL

  if ("hr_bpm" %in% names(breaths)) {
    n_missing_hr <- as.integer(sum(is.na(breaths$hr_bpm)))
    pct_missing_hr <- 100 * n_missing_hr / n_breaths
    hr_acceptable <- pct_missing_hr <= 10
  }

  # --- Missing Power ---
  n_missing_power <- NULL
  pct_missing_power <- NULL

  if ("power_w" %in% names(breaths)) {
    n_missing_power <- as.integer(sum(is.na(breaths$power_w)))
    pct_missing_power <- 100 * n_missing_power / n_breaths
  }

  # --- Baseline Stability ---
  baseline_result <- assess_baseline_stability(breaths)
  baseline_vo2_cv <- baseline_result$cv
  baseline_stable <- !is.null(baseline_vo2_cv) && baseline_vo2_cv <= 10

  # --- Calibration Drift Detection ---
  drift_detected <- detect_calibration_drift(breaths)

  # --- Signal Quality Score ---
  # Based on aberrant breaths, missing data, and baseline stability
  signal_score <- calculate_signal_quality_score(
    pct_aberrant, pct_missing_hr, baseline_vo2_cv
  )

  signal_rating <- get_rating(signal_score)

  # --- Overall Score ---
  overall_score <- calculate_overall_data_quality_score(
    pct_aberrant, pct_missing_hr, pct_missing_power, baseline_vo2_cv, drift_detected
  )

  overall_rating <- get_rating(overall_score)

  # --- Recommendations ---
  recommendations <- generate_data_quality_recommendations(
    pct_aberrant, pct_missing_hr, pct_missing_power, baseline_vo2_cv, drift_detected
  )

  DataQualityReport(
    n_breaths = as.integer(n_breaths),
    n_aberrant = as.integer(n_aberrant),
    pct_aberrant = pct_aberrant,
    aberrant_acceptable = aberrant_acceptable,
    n_missing_hr = n_missing_hr,
    pct_missing_hr = pct_missing_hr,
    hr_acceptable = hr_acceptable,
    n_missing_power = n_missing_power,
    pct_missing_power = pct_missing_power,
    baseline_vo2_cv = baseline_vo2_cv,
    baseline_stable = baseline_stable,
    signal_quality_score = signal_score,
    signal_quality_rating = signal_rating,
    calibration_drift_detected = drift_detected,
    artifact_timestamps = artifact_timestamps,
    overall_score = overall_score,
    overall_rating = overall_rating,
    recommendations = recommendations
  )
}


#' Detect Aberrant Breaths (Detailed)
#'
#' @param breaths Breath data tibble
#' @param threshold SD threshold for outlier detection
#' @return List with count and timestamps of aberrant breaths
#' @keywords internal
detect_aberrant_breaths_detailed <- function(breaths, threshold = 3) {
  n <- nrow(breaths)

  if (n < 10) {
    return(list(count = 0L, timestamps = NULL))
  }

  k <- min(11, n %/% 4)
  if (k < 5) k <- 5

  local_mean <- zoo::rollmean(breaths$vo2_ml, k = k, fill = NA, align = "center")
  local_sd <- zoo::rollapply(breaths$vo2_ml, width = k, FUN = sd, fill = NA, align = "center")

  z_scores <- abs(breaths$vo2_ml - local_mean) / local_sd
  aberrant_idx <- which(z_scores > threshold)

  list(
    count = length(aberrant_idx),
    timestamps = if (length(aberrant_idx) > 0) breaths$time_s[aberrant_idx] else NULL
  )
}


#' Assess Baseline Stability
#'
#' @param breaths Breath data tibble
#' @return List with cv (coefficient of variation of resting VO2)
#' @keywords internal
assess_baseline_stability <- function(breaths) {
  result <- list(cv = NULL)

  # Look for rest phase
  if ("phase" %in% names(breaths)) {
    rest_data <- breaths |>
      dplyr::filter(tolower(phase) %in% c("rest", "baseline", "warmup"))

    if (nrow(rest_data) >= 10) {
      mean_vo2 <- mean(rest_data$vo2_ml, na.rm = TRUE)
      sd_vo2 <- sd(rest_data$vo2_ml, na.rm = TRUE)
      result$cv <- 100 * sd_vo2 / mean_vo2
      return(result)
    }
  }

  # Use first 2 minutes as proxy for baseline
  first_2min <- breaths |>
    dplyr::filter(time_s <= min(time_s) + 120)

  if (nrow(first_2min) >= 10) {
    mean_vo2 <- mean(first_2min$vo2_ml, na.rm = TRUE)
    sd_vo2 <- sd(first_2min$vo2_ml, na.rm = TRUE)
    result$cv <- 100 * sd_vo2 / mean_vo2
  }

  result
}


#' Detect Calibration Drift
#'
#' Checks for systematic changes in measurement over time that might
#' indicate calibration issues.
#'
#' @param breaths Breath data tibble
#' @return Logical indicating if drift was detected
#' @keywords internal
detect_calibration_drift <- function(breaths) {
  # Look at RER values - should not systematically change at rest
  if (!"phase" %in% names(breaths)) {
    return(FALSE)
  }

  rest_data <- breaths |>
    dplyr::filter(tolower(phase) %in% c("rest", "baseline"))

  if (nrow(rest_data) < 20) {
    return(FALSE)
  }

  # Check for trend in RER during rest
  fit <- tryCatch({
    lm(rer ~ time_s, data = rest_data)
  }, error = function(e) NULL)

  if (is.null(fit)) {
    return(FALSE)
  }

  # Significant drift if slope is significant and > 0.001 per second
  p_value <- summary(fit)$coefficients[2, 4]
  slope <- coef(fit)[2]

  abs(slope) > 0.001 && p_value < 0.05
}


#' Calculate Signal Quality Score
#'
#' @param pct_aberrant Percentage of aberrant breaths
#' @param pct_missing_hr Percentage of missing HR (or NULL)
#' @param baseline_cv Baseline VO2 CV (or NULL)
#' @return Numeric score 0-100
#' @keywords internal
calculate_signal_quality_score <- function(pct_aberrant, pct_missing_hr, baseline_cv) {
  score <- 100

  # Aberrant breaths penalty (max 40 points)
  if (!is.null(pct_aberrant)) {
    if (pct_aberrant > 10) {
      score <- score - 40
    } else if (pct_aberrant > 5) {
      score <- score - 20
    } else if (pct_aberrant > 2) {
      score <- score - 10
    }
  }

  # Missing HR penalty (max 30 points)
  if (!is.null(pct_missing_hr)) {
    if (pct_missing_hr > 20) {
      score <- score - 30
    } else if (pct_missing_hr > 10) {
      score <- score - 15
    } else if (pct_missing_hr > 5) {
      score <- score - 5
    }
  }

  # Baseline stability penalty (max 30 points)
  if (!is.null(baseline_cv)) {
    if (baseline_cv > 20) {
      score <- score - 30
    } else if (baseline_cv > 15) {
      score <- score - 20
    } else if (baseline_cv > 10) {
      score <- score - 10
    }
  }

  max(0, score)
}


#' Calculate Overall Data Quality Score
#'
#' @param pct_aberrant Percentage of aberrant breaths
#' @param pct_missing_hr Percentage of missing HR
#' @param pct_missing_power Percentage of missing power
#' @param baseline_cv Baseline VO2 CV
#' @param drift_detected Whether calibration drift was detected
#' @return Numeric score 0-100
#' @keywords internal
calculate_overall_data_quality_score <- function(pct_aberrant, pct_missing_hr,
                                                  pct_missing_power, baseline_cv,
                                                  drift_detected) {
  score <- 100

  # Aberrant breaths (max 30 points)
  if (!is.null(pct_aberrant)) {
    if (pct_aberrant > 10) {
      score <- score - 30
    } else if (pct_aberrant > 5) {
      score <- score - 15
    } else if (pct_aberrant > 2) {
      score <- score - 5
    }
  }

  # Missing HR (max 25 points)
  if (!is.null(pct_missing_hr)) {
    if (pct_missing_hr > 20) {
      score <- score - 25
    } else if (pct_missing_hr > 10) {
      score <- score - 15
    } else if (pct_missing_hr > 5) {
      score <- score - 5
    }
  }

  # Missing power (max 15 points)
  if (!is.null(pct_missing_power)) {
    if (pct_missing_power > 20) {
      score <- score - 15
    } else if (pct_missing_power > 10) {
      score <- score - 10
    } else if (pct_missing_power > 5) {
      score <- score - 5
    }
  }

  # Baseline stability (max 20 points)
  if (!is.null(baseline_cv)) {
    if (baseline_cv > 20) {
      score <- score - 20
    } else if (baseline_cv > 15) {
      score <- score - 15
    } else if (baseline_cv > 10) {
      score <- score - 10
    }
  }

  # Calibration drift (max 10 points)
  if (drift_detected) {
    score <- score - 10
  }

  max(0, score)
}


#' Generate Data Quality Recommendations
#'
#' @param pct_aberrant Percentage of aberrant breaths
#' @param pct_missing_hr Percentage of missing HR
#' @param pct_missing_power Percentage of missing power
#' @param baseline_cv Baseline VO2 CV
#' @param drift_detected Whether calibration drift was detected
#' @return List of recommendation strings
#' @keywords internal
generate_data_quality_recommendations <- function(pct_aberrant, pct_missing_hr,
                                                   pct_missing_power, baseline_cv,
                                                   drift_detected) {
  recommendations <- list()

  if (!is.null(pct_aberrant) && pct_aberrant > 5) {
    recommendations <- c(recommendations,
      "High percentage of aberrant breaths detected. Consider filtering outliers or reviewing mask seal.")
  }

  if (!is.null(pct_missing_hr) && pct_missing_hr > 10) {
    recommendations <- c(recommendations,
      "Significant HR data gaps. Verify HR monitor connection and electrode placement.")
  }

  if (!is.null(pct_missing_power) && pct_missing_power > 10) {
    recommendations <- c(recommendations,
      "Power data incomplete. Check ergometer connection and calibration.")
  }

  if (!is.null(baseline_cv) && baseline_cv > 15) {
    recommendations <- c(recommendations,
      "Unstable baseline VO2. Ensure adequate rest period and mask seal before exercise.")
  }

  if (drift_detected) {
    recommendations <- c(recommendations,
      "Potential calibration drift detected. Consider recalibrating before next test.")
  }

  if (length(recommendations) == 0) {
    recommendations <- list("Data quality is acceptable. No specific recommendations.")
  }

  recommendations
}


# assess_quality -----------------------------------------------------------

#' @rdname assess_quality
method(assess_quality, CpetData) <- function(x, rpe = NULL, lactate = NULL, ...) {
  # Get individual assessments
  exercise_criteria <- assess_maximal_criteria(x, rpe = rpe, lactate = lactate)
  protocol_quality <- assess_protocol_quality(x)
  data_quality <- assess_data_quality(x)

  # Calculate overall score (weighted average)
  # Exercise criteria: 40%, Protocol: 30%, Data: 30%
  exercise_score <- calculate_exercise_criteria_score(exercise_criteria)
  protocol_score <- protocol_quality@overall_score
  data_score <- data_quality@overall_score

  overall_score <- 0.40 * exercise_score + 0.30 * protocol_score + 0.30 * data_score

  # Determine grade
  overall_grade <- if (overall_score >= 90) {
    "A"
  } else if (overall_score >= 80) {
    "B"
  } else if (overall_score >= 70) {
    "C"
  } else if (overall_score >= 60) {
    "D"
  } else {
    "F"
  }

  # Determine if test is interpretable
  test_interpretable <- overall_score >= 60 && data_quality@overall_rating != "poor"

  # Generate summary text
  summary_text <- generate_quality_summary(
    exercise_criteria, protocol_quality, data_quality, overall_grade
  )

  QualityAssessment(
    exercise_criteria = exercise_criteria,
    protocol_quality = protocol_quality,
    data_quality = data_quality,
    overall_grade = overall_grade,
    overall_score = overall_score,
    test_interpretable = test_interpretable,
    summary_text = summary_text
  )
}


#' Calculate Exercise Criteria Score
#'
#' @param criteria ExerciseQualityCriteria object
#' @return Numeric score 0-100
#' @keywords internal
calculate_exercise_criteria_score <- function(criteria) {
  # Score based on determination and criteria met
  base_score <- switch(criteria@determination,
    "maximal" = 100,
    "likely_maximal" = 85,
    "submaximal" = 60,
    "indeterminate" = 40
  )

  # Bonus for high RER
  if (criteria@peak_rer >= 1.15) {
    base_score <- min(100, base_score + 5)
  }

  # Bonus for high HR percentage
  if (!is.null(criteria@hr_pct_predicted) && criteria@hr_pct_predicted >= 95) {
    base_score <- min(100, base_score + 5)
  }

  base_score
}


#' Generate Quality Summary Text
#'
#' @param exercise_criteria ExerciseQualityCriteria object
#' @param protocol_quality ProtocolQuality object
#' @param data_quality DataQualityReport object
#' @param grade Overall grade
#' @return Character summary
#' @keywords internal
generate_quality_summary <- function(exercise_criteria, protocol_quality,
                                      data_quality, grade) {
  parts <- character()

  # Exercise determination
  exercise_text <- switch(exercise_criteria@determination,
    "maximal" = "Test achieved maximal effort",
    "likely_maximal" = "Test likely achieved maximal effort",
    "submaximal" = "Test did not achieve maximal effort",
    "indeterminate" = "Unable to determine if maximal effort was achieved"
  )
  parts <- c(parts, paste0(exercise_text, " (", exercise_criteria@criteria_met,
                           "/", exercise_criteria@criteria_available, " criteria met)."))

  # Protocol quality
  parts <- c(parts, paste0("Protocol quality: ", protocol_quality@overall_rating, "."))

  # Data quality
  parts <- c(parts, paste0("Data quality: ", data_quality@overall_rating, "."))

  # Overall assessment
  interpretability <- if (grade %in% c("A", "B", "C")) {
    "Results can be interpreted with confidence."
  } else if (grade == "D") {
    "Results should be interpreted with caution."
  } else {
    "Results may not be reliable."
  }
  parts <- c(parts, interpretability)

  paste(parts, collapse = " ")
}

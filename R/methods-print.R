# S7 Print Methods for cardiometR
# Clean console output for all S7 classes
# Print methods are automatically registered by S7 and don't need @export

method(print, Participant) <- function(x, ...) {
  cli::cli_h3("Participant")
  cli::cli_dl(c(
    "ID" = x@id,
    "Name" = x@name,
    "Age" = paste(x@age, "years"),
    "Sex" = switch(x@sex, M = "Male", F = "Female", O = "Other"),
    "Height" = paste(x@height_cm, "cm"),
    "Weight" = paste(x@weight_kg, "kg")
  ))
  if (!is.null(x@sport)) {
    cli::cli_text("Sport: {x@sport}")
  }
  invisible(x)
}


method(print, CpetMetadata) <- function(x, ...) {
  cli::cli_h3("CPET Metadata")
  cli::cli_dl(c(
    "Test date" = as.character(x@test_date),
    "Device" = x@device,
    "Protocol" = x@protocol
  ))
  if (!is.null(x@technician)) {
    cli::cli_text("Technician: {x@technician}")
  }
  if (!is.null(x@calibration_date)) {
    cli::cli_text("Calibration: {as.character(x@calibration_date)}")
  }
  # Environmental conditions
  env_info <- c()
  if (!is.null(x@temperature_c) && !anyNA(x@temperature_c)) {
    env_info <- c(env_info, paste0(x@temperature_c, "\u00B0C"))
  }
  if (!is.null(x@pressure_mmhg) && !anyNA(x@pressure_mmhg)) {
    env_info <- c(env_info, paste0(x@pressure_mmhg, " mmHg"))
  }
  if (!is.null(x@humidity_pct) && !anyNA(x@humidity_pct)) {
    env_info <- c(env_info, paste0(x@humidity_pct, "% RH"))
  }
  if (length(env_info) > 0) {
    cli::cli_text("Environment: {paste(env_info, collapse = ', ')}")
  }
  invisible(x)
}


method(print, CpetData) <- function(x, ...) {
  cli::cli_h1("CpetData")

  # Participant summary
  cli::cli_text("Participant: {.strong {x@participant@name}} ({x@participant@id})")

  # Test info
  cli::cli_text("Test date: {as.character(x@metadata@test_date)}")
  cli::cli_text("Device: {x@metadata@device}")
  cli::cli_text("Protocol: {x@metadata@protocol}")

 # Data summary
  cli::cli_h3("Data")
  cli::cli_dl(c(
    "Breaths" = format(nrow(x@breaths), big.mark = ","),
    "Duration" = paste(round(max(x@breaths$time_s) / 60, 1), "min"),
    "Averaged" = if (x@is_averaged) paste("Yes (", x@averaging_window, "s)") else "No"
  ))

  # Available variables
  optional_vars <- c("hr_bpm", "power_w", "peto2_mmhg", "petco2_mmhg")
  present_vars <- optional_vars[optional_vars %in% names(x@breaths)]
  if (length(present_vars) > 0) {
    cli::cli_text("Optional data: {paste(present_vars, collapse = ', ')}")
  }

  # Stages info
  if (!is.null(x@stages)) {
    cli::cli_text("Stages: {nrow(x@stages)} defined")
  }

  invisible(x)
}


method(print, PeakValues) <- function(x, ...) {
  cli::cli_h3("Peak Values")
  cli::cli_text("(averaged over {x@averaging_s}s)")
  cli::cli_dl(c(
    "VO2 peak" = paste(round(x@vo2_peak, 0), "mL/min"),
    "VO2/kg peak" = paste(round(x@vo2_kg_peak, 1), "mL/kg/min"),
    "VE peak" = paste(round(x@ve_peak, 1), "L/min"),
    "RER peak" = round(x@rer_peak, 2)
  ))
  if (!is.null(x@hr_peak) && length(x@hr_peak) > 0 && !anyNA(x@hr_peak)) {
    cli::cli_text("HR peak: {round(x@hr_peak, 0)} bpm")
  }
  if (!is.null(x@power_peak) && length(x@power_peak) > 0 && !anyNA(x@power_peak)) {
    cli::cli_text("Power peak: {round(x@power_peak, 0)} W")
  }
  invisible(x)
}


method(print, Thresholds) <- function(x, ...) {
  cli::cli_h3("Ventilatory Thresholds")

  # VT1
  if (!is.null(x@vt1_vo2) && length(x@vt1_vo2) > 0 && !anyNA(x@vt1_vo2)) {
    cli::cli_text("{.strong VT1 (Aerobic Threshold)}")
    vt1_info <- c("VO2" = paste(round(x@vt1_vo2, 0), "mL/min"))
    if (!is.null(x@vt1_hr) && !anyNA(x@vt1_hr)) {
      vt1_info <- c(vt1_info, "HR" = paste(round(x@vt1_hr, 0), "bpm"))
    }
    if (!is.null(x@vt1_power) && !anyNA(x@vt1_power)) {
      vt1_info <- c(vt1_info, "Power" = paste(round(x@vt1_power, 0), "W"))
    }
    if (!is.null(x@vt1_method) && length(x@vt1_method) > 0 && !anyNA(x@vt1_method)) {
      vt1_info <- c(vt1_info, "Method" = x@vt1_method)
    }
    cli::cli_dl(vt1_info)
  } else {
    cli::cli_text("VT1: {.emph Not detected}")
  }

  # VT2
  if (!is.null(x@vt2_vo2) && length(x@vt2_vo2) > 0 && !anyNA(x@vt2_vo2)) {
    cli::cli_text("{.strong VT2 (Respiratory Compensation)}")
    vt2_info <- c("VO2" = paste(round(x@vt2_vo2, 0), "mL/min"))
    if (!is.null(x@vt2_hr) && !anyNA(x@vt2_hr)) {
      vt2_info <- c(vt2_info, "HR" = paste(round(x@vt2_hr, 0), "bpm"))
    }
    if (!is.null(x@vt2_power) && !anyNA(x@vt2_power)) {
      vt2_info <- c(vt2_info, "Power" = paste(round(x@vt2_power, 0), "W"))
    }
    if (!is.null(x@vt2_method) && length(x@vt2_method) > 0 && !anyNA(x@vt2_method)) {
      vt2_info <- c(vt2_info, "Method" = x@vt2_method)
    }
    cli::cli_dl(vt2_info)
  } else {
    cli::cli_text("VT2: {.emph Not detected}")
  }

  # Confidence
  if (!is.null(x@confidence) && length(x@confidence) > 0 && !anyNA(x@confidence)) {
    cli::cli_text("Confidence: {x@confidence}")
  }

  invisible(x)
}


method(print, ValidationReport) <- function(x, ...) {
  if (x@is_valid) {
    cli::cli_alert_success("Validation passed")
  } else {
    cli::cli_alert_danger("Validation failed")
  }

  # Errors
  if (length(x@errors) > 0) {
    cli::cli_text("{.strong Errors ({length(x@errors)})}")
    purrr::walk(x@errors, ~ cli::cli_alert_danger(.x))
  }

  # Warnings
  if (length(x@warnings) > 0) {
    cli::cli_text("{.strong Warnings ({length(x@warnings)})}")
    purrr::walk(x@warnings, ~ cli::cli_alert_warning(.x))
  }

  # Info
  if (length(x@info) > 0) {
    cli::cli_text("{.strong Info ({length(x@info)})}")
    purrr::walk(x@info, ~ cli::cli_alert_info(.x))
  }

  invisible(x)
}


method(print, CpetAnalysis) <- function(x, ...) {
  cli::cli_h1("CPET Analysis")

  # Participant
  cli::cli_text("Participant: {.strong {x@data@participant@name}}")
  cli::cli_text("Test date: {as.character(x@data@metadata@test_date)}")

  # Data summary
  cli::cli_text("Data: {nrow(x@data@breaths)} breaths, {round(max(x@data@breaths$time_s) / 60, 1)} min")

  # Components present
  components <- c()
  if (!is.null(x@peaks)) components <- c(components, "peaks")
  if (!is.null(x@thresholds)) components <- c(components, "thresholds")
  if (!is.null(x@stage_summary)) components <- c(components, paste0("stages (", nrow(x@stage_summary), ")"))
  if (!is.null(x@validation)) {
    val_status <- if (x@validation@is_valid) "valid" else "invalid"
    components <- c(components, paste0("validation (", val_status, ")"))
  }

  if (length(components) > 0) {
    cli::cli_text("Components: {paste(components, collapse = ', ')}")
  }

  # Peak VO2 if available
  if (!is.null(x@peaks)) {
    cli::cli_text("")
    cli::cli_alert_success("VO2 peak: {round(x@peaks@vo2_kg_peak, 1)} mL/kg/min")
  }

  invisible(x)
}


method(print, ReportConfig) <- function(x, ...) {
  cli::cli_h3("Report Configuration")
  cli::cli_dl(c(
    "Language" = switch(x@language, en = "English", fr = "French"),
    "Format" = x@output_format
  ))
  if (!is.null(x@institution)) {
    cli::cli_text("Institution: {x@institution}")
  }
  if (!is.null(x@technician)) {
    cli::cli_text("Technician: {x@technician}")
  }
  if (!is.null(x@template)) {
    cli::cli_text("Template: {x@template}")
  }
  invisible(x)
}


#' @rdname ExerciseQualityCriteria
method(print, ExerciseQualityCriteria) <- function(x, ...) {
  cli::cli_h3("Maximal Exercise Criteria (ACSM)")

  # Determination header
  det_style <- switch(x@determination,
    "maximal" = "success",
    "likely_maximal" = "info",
    "submaximal" = "warning",
    "indeterminate" = ""
  )
  det_text <- switch(x@determination,
    "maximal" = "MAXIMAL TEST",
    "likely_maximal" = "Likely Maximal",
    "submaximal" = "Submaximal",
    "indeterminate" = "Indeterminate"
  )

  if (det_style == "success") {
    cli::cli_alert_success(det_text)
  } else if (det_style == "warning") {
    cli::cli_alert_warning(det_text)
  } else {
    cli::cli_alert_info(det_text)
  }

  cli::cli_text("")

  # RER criterion
  rer_icon <- if (x@rer_met) "\u2713" else "\u2717"
  cli::cli_text("{rer_icon} RER >= 1.10: {round(x@peak_rer, 2)}")

  # HR criterion
  if (!is.null(x@hr_met)) {
    hr_icon <- if (x@hr_met) "\u2713" else "\u2717"
    cli::cli_text("{hr_icon} HR >= 85% predicted: {round(x@peak_hr, 0)} bpm ({round(x@hr_pct_predicted, 0)}%)")
  } else {
    cli::cli_text("\u25CB HR: Not available")
  }

  # Plateau criterion
  plateau_icon <- if (x@plateau_met) "\u2713" else "\u2717"
  if (x@vo2_plateau_detected) {
    cli::cli_text("{plateau_icon} VO2 plateau: \u0394 = {round(x@vo2_plateau_delta, 0)} mL/min")
  } else {
    cli::cli_text("{plateau_icon} VO2 plateau: Not detected")
  }

  # Optional criteria
  if (!is.null(x@rpe_met)) {
    rpe_icon <- if (x@rpe_met) "\u2713" else "\u2717"
    cli::cli_text("{rpe_icon} RPE >= 17: {x@rpe_reported}")
  }

  if (!is.null(x@lactate_met)) {
    lac_icon <- if (x@lactate_met) "\u2713" else "\u2717"
    cli::cli_text("{lac_icon} Lactate >= 8.0: {round(x@lactate_mmol, 1)} mmol/L")
  }

  cli::cli_text("")
  cli::cli_text("Criteria met: {x@criteria_met}/{x@criteria_available}")

  invisible(x)
}


#' @rdname ProtocolQuality
method(print, ProtocolQuality) <- function(x, ...) {
  cli::cli_h3("Protocol Quality Assessment")

  # Overall rating
  rating_style <- switch(x@overall_rating,
    "excellent" = "success",
    "good" = "info",
    "acceptable" = "warning",
    "poor" = "danger",
    ""
  )
  rating_text <- paste0(toupper(x@overall_rating), " (", round(x@overall_score, 0), "/100)")

  if (rating_style == "success") {
    cli::cli_alert_success(rating_text)
  } else if (rating_style == "warning") {
    cli::cli_alert_warning(rating_text)
  } else if (rating_style == "danger") {
    cli::cli_alert_danger(rating_text)
  } else {
    cli::cli_alert_info(rating_text)
  }

  cli::cli_text("")
  cli::cli_dl(c(
    "Modality" = x@modality,
    "Test duration" = paste(round(x@test_duration_s / 60, 1), "min")
  ))

  if (!is.null(x@exercise_duration_s)) {
    duration_status <- if (!is.null(x@duration_optimal) && x@duration_optimal) "\u2713" else "\u2717"
    cli::cli_text("Exercise duration: {round(x@exercise_duration_s / 60, 1)} min {duration_status}")
  }

  if (!is.null(x@actual_vo2_slope)) {
    slope_status <- if (!is.null(x@slope_acceptable) && x@slope_acceptable) "\u2713" else "\u2717"
    cli::cli_text("VO2/W slope: {round(x@actual_vo2_slope, 1)} mL/min/W {slope_status}")
    if (!is.null(x@expected_vo2_slope)) {
      cli::cli_text("  (expected: {x@expected_vo2_slope}, deviation: {sprintf('%+.1f', x@slope_deviation_pct)}%)")
    }
  }

  if (!is.null(x@r_squared)) {
    r2_status <- if (!is.null(x@r_squared_acceptable) && x@r_squared_acceptable) "\u2713" else "\u2717"
    cli::cli_text("R\u00B2: {round(x@r_squared, 3)} {r2_status}")
  }

  invisible(x)
}


#' @rdname DataQualityReport
method(print, DataQualityReport) <- function(x, ...) {
  cli::cli_h3("Data Quality Report")

  # Overall rating
  rating_style <- switch(x@overall_rating,
    "excellent" = "success",
    "good" = "info",
    "acceptable" = "warning",
    "poor" = "danger"
  )
  rating_text <- paste0(toupper(x@overall_rating), " (", round(x@overall_score, 0), "/100)")

  if (rating_style == "success") {
    cli::cli_alert_success(rating_text)
  } else if (rating_style == "warning") {
    cli::cli_alert_warning(rating_text)
  } else if (rating_style == "danger") {
    cli::cli_alert_danger(rating_text)
  } else {
    cli::cli_alert_info(rating_text)
  }

  cli::cli_text("")
  cli::cli_dl(c(
    "Total breaths" = format(x@n_breaths, big.mark = ","),
    "Aberrant breaths" = paste0(round(x@pct_aberrant, 1), "% (n=", x@n_aberrant, ")")
  ))

  if (!is.null(x@pct_missing_hr)) {
    hr_status <- if (!is.null(x@hr_acceptable) && x@hr_acceptable) "\u2713" else "\u2717"
    cli::cli_text("Missing HR: {round(x@pct_missing_hr, 1)}% {hr_status}")
  }

  if (!is.null(x@baseline_vo2_cv)) {
    baseline_status <- if (!is.null(x@baseline_stable) && x@baseline_stable) "\u2713" else "\u2717"
    cli::cli_text("Baseline CV: {round(x@baseline_vo2_cv, 1)}% {baseline_status}")
  }

  drift_status <- if (x@calibration_drift_detected) "\u2717 Detected" else "\u2713 Not detected"
  cli::cli_text("Calibration drift: {drift_status}")

  cli::cli_text("Signal quality: {x@signal_quality_rating} ({round(x@signal_quality_score, 0)}/100)")

  # Recommendations
  if (length(x@recommendations) > 0 && x@recommendations[[1]] != "Data quality is acceptable. No specific recommendations.") {
    cli::cli_text("")
    cli::cli_text("{.strong Recommendations:}")
    purrr::walk(x@recommendations, ~ cli::cli_alert_warning(.x))
  }

  invisible(x)
}


#' @rdname QualityAssessment
method(print, QualityAssessment) <- function(x, ...) {
  cli::cli_h1("Quality Assessment")

  # Overall grade
  grade_style <- switch(x@overall_grade,
    "A" = "success",
    "B" = "info",
    "C" = "warning",
    "D" = "warning",
    "F" = "danger"
  )
  grade_text <- paste0("Grade: ", x@overall_grade, " (", round(x@overall_score, 0), "/100)")

  if (grade_style == "success") {
    cli::cli_alert_success(grade_text)
  } else if (grade_style == "warning") {
    cli::cli_alert_warning(grade_text)
  } else if (grade_style == "danger") {
    cli::cli_alert_danger(grade_text)
  } else {
    cli::cli_alert_info(grade_text)
  }

  # Interpretability
  if (x@test_interpretable) {
    cli::cli_alert_success("Results are interpretable")
  } else {
    cli::cli_alert_warning("Results should be interpreted with caution")
  }

  cli::cli_text("")
  cli::cli_text("{.emph {x@summary_text}}")

  cli::cli_text("")
  cli::cli_rule()

  # Component summaries
  if (!is.null(x@exercise_criteria)) {
    det <- x@exercise_criteria@determination
    det_text <- switch(det,
      "maximal" = "Maximal",
      "likely_maximal" = "Likely maximal",
      "submaximal" = "Submaximal",
      "indeterminate" = "Indeterminate"
    )
    cli::cli_text("Exercise: {det_text} ({x@exercise_criteria@criteria_met}/{x@exercise_criteria@criteria_available} criteria)")
  }

  if (!is.null(x@protocol_quality)) {
    cli::cli_text("Protocol: {x@protocol_quality@overall_rating} ({round(x@protocol_quality@overall_score, 0)}/100)")
  }

  if (!is.null(x@data_quality)) {
    cli::cli_text("Data: {x@data_quality@overall_rating} ({round(x@data_quality@overall_score, 0)}/100)")
  }

  invisible(x)
}

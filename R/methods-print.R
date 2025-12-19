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
  if (!is.null(x@temperature_c)) env_info <- c(env_info, paste0(x@temperature_c, "\u00B0C"))
  if (!is.null(x@pressure_mmhg)) env_info <- c(env_info, paste0(x@pressure_mmhg, " mmHg"))
  if (!is.null(x@humidity_pct)) env_info <- c(env_info, paste0(x@humidity_pct, "% RH"))
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
  if (!is.null(x@hr_peak)) {
    cli::cli_text("HR peak: {round(x@hr_peak, 0)} bpm")
  }
  if (!is.null(x@power_peak)) {
    cli::cli_text("Power peak: {round(x@power_peak, 0)} W")
  }
  invisible(x)
}


method(print, Thresholds) <- function(x, ...) {
  cli::cli_h3("Ventilatory Thresholds")

  # VT1
  if (!is.null(x@vt1_vo2)) {
    cli::cli_text("{.strong VT1 (Aerobic Threshold)}")
    vt1_info <- c("VO2" = paste(round(x@vt1_vo2, 0), "mL/min"))
    if (!is.null(x@vt1_hr)) vt1_info <- c(vt1_info, "HR" = paste(round(x@vt1_hr, 0), "bpm"))
    if (!is.null(x@vt1_power)) vt1_info <- c(vt1_info, "Power" = paste(round(x@vt1_power, 0), "W"))
    if (!is.null(x@vt1_method)) vt1_info <- c(vt1_info, "Method" = x@vt1_method)
    cli::cli_dl(vt1_info)
  } else {
    cli::cli_text("VT1: {.emph Not detected}")
  }

  # VT2
  if (!is.null(x@vt2_vo2)) {
    cli::cli_text("{.strong VT2 (Respiratory Compensation)}")
    vt2_info <- c("VO2" = paste(round(x@vt2_vo2, 0), "mL/min"))
    if (!is.null(x@vt2_hr)) vt2_info <- c(vt2_info, "HR" = paste(round(x@vt2_hr, 0), "bpm"))
    if (!is.null(x@vt2_power)) vt2_info <- c(vt2_info, "Power" = paste(round(x@vt2_power, 0), "W"))
    if (!is.null(x@vt2_method)) vt2_info <- c(vt2_info, "Method" = x@vt2_method)
    cli::cli_dl(vt2_info)
  } else {
    cli::cli_text("VT2: {.emph Not detected}")
  }

  # Confidence
  if (!is.null(x@confidence)) {
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

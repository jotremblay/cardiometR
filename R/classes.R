# S7 Class Definitions for cardiometR
# All core data structures for CPET analysis

# Participant ----------------------------------------------------------------

#' Participant Class
#'
#' @description
#' Stores participant demographics and physical characteristics.
#'
#' @param id Unique participant identifier
#' @param name Participant name
#' @param age Age in years
#' @param sex Sex: "M" (male), "F" (female), or "O" (other)
#' @param height_cm Height in centimeters
#' @param weight_kg Weight in kilograms
#' @param sport Optional sport or physical activity (NULL if not specified)
#'
#' @return A Participant S7 object
#'
#' @examples
#' participant <- Participant(
#'   id = "P001",
#'   name = "Jean Dupont",
#'   age = 35,
#'   sex = "M",
#'   height_cm = 175,
#'   weight_kg = 72
#' )
#'
#' @export
Participant <- new_class("Participant",
  properties = list(
    id = class_character,
    name = class_character,
    age = new_property(class_numeric,
      validator = function(value) {
        if (value < 0 || value > 120) {
          return("age must be between 0 and 120 years")
        }
        NULL
      }
    ),
    sex = new_property(class_character,
      validator = function(value) {
        if (!value %in% c("M", "F", "O")) {
          return("sex must be 'M' (male), 'F' (female), or 'O' (other)")
        }
        NULL
      }
    ),
    height_cm = new_property(class_numeric,
      validator = function(value) {
        if (value < 50 || value > 250) {
          return("height_cm must be between 50 and 250 cm")
        }
        NULL
      }
    ),
    weight_kg = new_property(class_numeric,
      validator = function(value) {
        if (value < 10 || value > 300) {
          return("weight_kg must be between 10 and 300 kg")
        }
        NULL
      }
    ),
    sport = class_character | NULL,
    date_of_birth = class_Date | NULL
  )
)


# CpetMetadata ---------------------------------------------------------------

#' CPET Metadata Class
#'
#' @description
#' Stores test metadata including calibration and environmental conditions.
#'
#' @param test_date Date of the CPET test
#' @param device Device/metabolic cart name (e.g., "COSMED Quark CPET")
#' @param protocol Exercise protocol name (e.g., "Bruce", "Ramp")
#' @param calibration_date Optional date of last calibration
#' @param temperature_c Optional ambient temperature in Celsius
#' @param pressure_mmhg Optional barometric pressure in mmHg
#' @param humidity_pct Optional relative humidity percentage
#' @param technician Optional name of technician who conducted the test
#'
#' @return A CpetMetadata S7 object
#'
#' @examples
#' metadata <- CpetMetadata(
#'   test_date = Sys.Date(),
#'   device = "COSMED Quark CPET",
#'   protocol = "Incremental ramp"
#' )
#'
#' @export
CpetMetadata <- new_class("CpetMetadata",
  properties = list(
    test_date = class_Date,
    device = class_character,
    protocol = class_character,
    calibration_date = class_Date | NULL,
    temperature_c = new_property(class_numeric | NULL,
      validator = function(value) {
        if (!is.null(value) && length(value) > 0 && !anyNA(value) && (value < 10 || value > 40)) {
          return("temperature_c should be between 10 and 40 degrees Celsius")
        }
        NULL
      }
    ),
    pressure_mmhg = new_property(class_numeric | NULL,
      validator = function(value) {
        if (!is.null(value) && length(value) > 0 && !anyNA(value) && (value < 600 || value > 900)) {
          return("pressure_mmhg should be between 600 and 900 mmHg")
        }
        NULL
      }
    ),
    humidity_pct = new_property(class_numeric | NULL,
      validator = function(value) {
        if (!is.null(value) && length(value) > 0 && !anyNA(value) && (value < 0 || value > 100)) {
          return("humidity_pct must be between 0 and 100")
        }
        NULL
      }
    ),
    technician = class_character | NULL
  )
)


# CpetData -------------------------------------------------------------------

#' CPET Data Class
#'
#' @description
#' Main container for CPET breath-by-breath data with participant and metadata.
#'
#' @param participant A Participant S7 object
#' @param metadata A CpetMetadata S7 object
#' @param breaths A data.frame/tibble of breath-by-breath measurements.
#'   Required columns: time_s, vo2_ml, vco2_ml, ve_l, rer.
#'   Optional columns: hr_bpm, power_w, bf, vt_l, peto2_mmhg, petco2_mmhg,
#'   ve_vo2, ve_vco2.
#' @param stages Optional data.frame of stage annotations
#' @param is_averaged Logical indicating if data has been averaged
#' @param averaging_window Numeric window size in seconds (if averaged)
#'
#' @return A CpetData S7 object
#'
#' @examples
#' \dontrun{
#' data <- CpetData(
#'   participant = participant,
#'   metadata = metadata,
#'   breaths = breath_data,
#'   is_averaged = FALSE
#' )
#' }
#'
#' @export
CpetData <- new_class("CpetData",
  properties = list(
    participant = Participant,
    metadata = CpetMetadata,
    breaths = class_data.frame,
    stages = class_data.frame | NULL,
    is_averaged = class_logical,
    averaging_window = class_numeric | NULL
  ),
  validator = function(self) {
    errors <- character()

    # Check required columns in breaths
    required_cols <- c("time_s", "vo2_ml", "vco2_ml", "ve_l", "rer")
    missing <- setdiff(required_cols, names(self@breaths))
    if (length(missing) > 0) {
      errors <- c(errors,
        paste("breaths missing required columns:", paste(missing, collapse = ", "))
      )
    }

    # Check averaging consistency
    if (self@is_averaged && is.null(self@averaging_window)) {
      errors <- c(errors,
        "averaging_window is required when is_averaged is TRUE"
      )
    }

    # Check time is monotonically increasing
    if ("time_s" %in% names(self@breaths) && nrow(self@breaths) > 1) {
      if (any(diff(self@breaths$time_s) < 0)) {
        errors <- c(errors, "time_s must be monotonically increasing")
      }
    }

    if (length(errors) > 0) {
      return(paste(errors, collapse = "; "))
    }
    NULL
  }
)


# PeakValues -----------------------------------------------------------------

#' Peak Values Class
#'
#' @description
#' Container for peak/maximal values from a CPET test.
#'
#' @param vo2_peak Peak oxygen uptake in mL/min
#' @param vo2_kg_peak Peak oxygen uptake relative to body weight in mL/kg/min
#' @param ve_peak Peak minute ventilation in L/min
#' @param rer_peak Peak respiratory exchange ratio
#' @param averaging_s Averaging window used to determine peaks (seconds)
#' @param hr_peak Optional peak heart rate in bpm
#' @param power_peak Optional peak power output in watts
#'
#' @return A PeakValues S7 object
#'
#' @export
PeakValues <- new_class("PeakValues",
  properties = list(
    vo2_peak = new_property(class_numeric,
      validator = function(value) {
        if (length(value) > 0 && !anyNA(value) && (value < 0 || value > 8000)) {
          return("vo2_peak should be between 0 and 8000 mL/min")
        }
        NULL
      }
    ),
    vo2_kg_peak = new_property(class_numeric,
      validator = function(value) {
        if (length(value) > 0 && !anyNA(value) && (value < 0 || value > 100)) {
          return("vo2_kg_peak should be between 0 and 100 mL/kg/min")
        }
        NULL
      }
    ),
    ve_peak = new_property(class_numeric,
      validator = function(value) {
        if (length(value) > 0 && !anyNA(value) && (value < 0 || value > 250)) {
          return("ve_peak should be between 0 and 250 L/min")
        }
        NULL
      }
    ),
    rer_peak = new_property(class_numeric,
      validator = function(value) {
        if (length(value) > 0 && !anyNA(value) && (value < 0.5 || value > 2.0)) {
          return("rer_peak should be between 0.5 and 2.0")
        }
        NULL
      }
    ),
    averaging_s = class_numeric,
    hr_peak = new_property(class_numeric | NULL,
      validator = function(value) {
        if (!is.null(value) && length(value) > 0 && !anyNA(value) && (value < 30 || value > 250)) {
          return("hr_peak should be between 30 and 250 bpm")
        }
        NULL
      }
    ),
    power_peak = new_property(class_numeric | NULL,
      validator = function(value) {
        if (!is.null(value) && length(value) > 0 && !anyNA(value) && (value < 0 || value > 600)) {
          return("power_peak should be between 0 and 600 W")
        }
        NULL
      }
    )
  )
)


# Thresholds -----------------------------------------------------------------

#' Ventilatory Thresholds Class
#'
#' @description
#' Container for ventilatory threshold detection results (VT1 and VT2).
#'
#' @param vt1_vo2 VO2 at VT1 (aerobic threshold) in mL/min
#' @param vt1_hr Heart rate at VT1 in bpm
#' @param vt1_power Power output at VT1 in watts
#' @param vt1_method Detection method used for VT1
#' @param vt2_vo2 VO2 at VT2 (respiratory compensation point) in mL/min
#' @param vt2_hr Heart rate at VT2 in bpm
#' @param vt2_power Power output at VT2 in watts
#' @param vt2_method Detection method used for VT2
#' @param confidence Confidence level: "high", "moderate", "low", or "unable"
#'
#' @return A Thresholds S7 object
#'
#' @export
Thresholds <- new_class("Thresholds",
  properties = list(
    vt1_vo2 = class_numeric | NULL,
    vt1_hr = class_numeric | NULL,
    vt1_power = class_numeric | NULL,
    vt1_method = class_character | NULL,
    vt2_vo2 = class_numeric | NULL,
    vt2_hr = class_numeric | NULL,
    vt2_power = class_numeric | NULL,
    vt2_method = class_character | NULL,
    confidence = new_property(class_character | NULL,
      validator = function(value) {
        if (!is.null(value) && length(value) > 0 && !value %in% c("high", "moderate", "low", "unable")) {
          return("confidence must be 'high', 'moderate', 'low', or 'unable'")
        }
        NULL
      }
    )
  )
)


# ValidationReport -----------------------------------------------------------

#' Validation Report Class
#'
#' @description
#' Container for data validation results including errors, warnings, and info.
#'
#' @param is_valid Logical indicating if validation passed (no errors)
#' @param errors List of error messages (validation failures)
#' @param warnings List of warning messages (potential issues)
#' @param info List of informational messages
#'
#' @return A ValidationReport S7 object
#'
#' @export
ValidationReport <- new_class("ValidationReport",
  properties = list(
    is_valid = class_logical,
    errors = class_list,
    warnings = class_list,
    info = class_list
  ),
  validator = function(self) {
    # Skip validation if is_valid is not set (zero-length)
    if (length(self@is_valid) == 0) return(NULL)

    # is_valid should be TRUE only if there are no errors
    if (self@is_valid && length(self@errors) > 0) {
      return("is_valid cannot be TRUE when errors exist")
    }
    if (!self@is_valid && length(self@errors) == 0) {
      return("is_valid should be TRUE when no errors exist")
    }
    NULL
  }
)


# CpetAnalysis ---------------------------------------------------------------

#' CPET Analysis Class
#'
#' @description
#' Complete analysis result containing processed data, peaks, thresholds,
#' and validation results.
#'
#' @param data A CpetData S7 object (processed/averaged)
#' @param peaks Optional PeakValues S7 object
#' @param thresholds Optional Thresholds S7 object
#' @param stage_summary Optional data.frame summarizing values by stage
#' @param validation Optional ValidationReport S7 object
#'
#' @return A CpetAnalysis S7 object
#'
#' @export
CpetAnalysis <- new_class("CpetAnalysis",
  properties = list(
    data = CpetData,
    peaks = PeakValues | NULL,
    thresholds = Thresholds | NULL,
    stage_summary = class_data.frame | NULL,
    validation = ValidationReport | NULL
  )
)


# ReportConfig ---------------------------------------------------------------

#' Report Configuration Class
#'
#' @description
#' Configuration options for PDF report generation.
#'
#' @param language Report language: "en" (English) or "fr" (French)
#' @param institution Optional institution name for report header
#' @param logo_path Optional path to logo image file
#' @param technician Optional technician name
#' @param output_format Output format (default "pdf")
#' @param template Optional path to custom Typst template
#'
#' @return A ReportConfig S7 object
#'
#' @examples
#' config <- ReportConfig(
#'   language = "fr",
#'   institution = "UCLouvain",
#'   technician = "Dr. Smith"
#' )
#'
#' @export
ReportConfig <- new_class("ReportConfig",
  properties = list(
    language = new_property(class_character,
      default = "en",
      validator = function(value) {
        if (!value %in% c("en", "fr")) {
          return("language must be 'en' (English) or 'fr' (French)")
        }
        NULL
      }
    ),
    institution = class_character | NULL,
    lab_name = class_character | NULL,
    logo_path = class_character | NULL,
    technician = class_character | NULL,
    output_format = new_property(class_character, default = "pdf"),
    template = class_character | NULL
  )
)

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
#' @param date_of_birth Optional date of birth (NULL if not specified)
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


# PreTestConditions ----------------------------------------------------------

#' Pre-Test Conditions Class
#'
#' @description
#' Stores pre-test physiological state and preparation details for contextualizing
#' CPET results. Includes nutritional state, fatigue level, medication use, and
#' caffeine intake.
#'
#' @param nutritional_state Nutritional state: "fed" or "fasted" (NULL if unknown)
#' @param last_meal_hours Hours since last meal (NULL if not recorded)
#' @param fatigue_state Fatigue state: "rested" or "fatigued" (NULL if unknown)
#' @param medications_taken Logical indicating if medications were taken (default FALSE)
#' @param medication_names Character vector of medication names (NULL if none)
#' @param caffeine_intake Logical indicating caffeine/stimulant intake (NULL if unknown)
#' @param caffeine_mg Caffeine amount in mg (NULL if not recorded)
#' @param notes Optional free-text notes about pre-test conditions
#'
#' @return A PreTestConditions S7 object
#'
#' @examples
#' conditions <- PreTestConditions(
#'   nutritional_state = "fasted",
#'   last_meal_hours = 3,
#'   fatigue_state = "rested",
#'   caffeine_intake = TRUE,
#'   caffeine_mg = 200
#' )
#'
#' @export
PreTestConditions <- new_class("PreTestConditions",
  properties = list(
    nutritional_state = new_property(class_character | NULL,
      validator = function(value) {
        if (!is.null(value) && length(value) > 0 && !value %in% c("fed", "fasted")) {
          return("nutritional_state must be 'fed' or 'fasted'")
        }
        NULL
      }
    ),
    last_meal_hours = new_property(class_numeric | NULL,
      validator = function(value) {
        if (!is.null(value) && length(value) > 0 && !anyNA(value) && (value < 0 || value > 48)) {
          return("last_meal_hours should be between 0 and 48 hours")
        }
        NULL
      }
    ),
    fatigue_state = new_property(class_character | NULL,
      validator = function(value) {
        if (!is.null(value) && length(value) > 0 && !value %in% c("rested", "fatigued")) {
          return("fatigue_state must be 'rested' or 'fatigued'")
        }
        NULL
      }
    ),
    medications_taken = new_property(class_logical, default = FALSE),
    medication_names = class_character | NULL,
    caffeine_intake = class_logical | NULL,
    caffeine_mg = new_property(class_numeric | NULL,
      validator = function(value) {
        if (!is.null(value) && length(value) > 0 && !anyNA(value) && (value < 0 || value > 1000)) {
          return("caffeine_mg should be between 0 and 1000 mg")
        }
        NULL
      }
    ),
    notes = class_character | NULL
  )
)


# ProtocolConfig -------------------------------------------------------------

#' Protocol Configuration Class
#'
#' @description
#' Detailed exercise protocol parameters including intensity progression,
#' stage timing, and equipment information.
#'
#' @param modality Exercise modality: "cycling", "treadmill", or "other"
#' @param starting_intensity Starting intensity (watts for cycling, km/h for treadmill)
#' @param increment_size Intensity increment per stage
#' @param stage_duration_s Duration of each stage in seconds
#' @param starting_grade Starting grade/slope percentage (treadmill only)
#' @param grade_increment Grade increment per stage in percentage points (treadmill only)
#' @param equipment_model Ergometer/treadmill model name
#' @param analyzer_model Metabolic analyzer model name
#'
#' @return A ProtocolConfig S7 object
#'
#' @examples
#' # Cycling protocol
#' protocol <- ProtocolConfig(
#'   modality = "cycling",
#'   starting_intensity = 50,
#'   increment_size = 25,
#'   stage_duration_s = 180,
#'   equipment_model = "Lode Excalibur Sport",
#'   analyzer_model = "COSMED Quark CPET"
#' )
#'
#' # Treadmill protocol
#' protocol <- ProtocolConfig(
#'   modality = "treadmill",
#'   starting_intensity = 8,
#'   increment_size = 1,
#'   stage_duration_s = 120,
#'   starting_grade = 0.5,
#'   grade_increment = 0,
#'   equipment_model = "HP Cosmos Pulsar"
#' )
#'
#' @export
ProtocolConfig <- new_class("ProtocolConfig",
  properties = list(
    modality = new_property(class_character,
      default = "cycling",
      validator = function(value) {
        if (!value %in% c("cycling", "treadmill", "other")) {
          return("modality must be 'cycling', 'treadmill', or 'other'")
        }
        NULL
      }
    ),
    starting_intensity = class_numeric | NULL,
    increment_size = class_numeric | NULL,
    stage_duration_s = new_property(class_numeric | NULL,
      validator = function(value) {
        if (!is.null(value) && length(value) > 0 && !anyNA(value) && (value < 30 || value > 600)) {
          return("stage_duration_s should be between 30 and 600 seconds")
        }
        NULL
      }
    ),
    starting_grade = class_numeric | NULL,
    grade_increment = class_numeric | NULL,
    equipment_model = class_character | NULL,
    analyzer_model = class_character | NULL
  )
)


# EconomyMetrics -------------------------------------------------------------

#' Economy and Efficiency Metrics Class
#'
#' @description
#' Container for running economy and cycling efficiency calculations.
#' Stores the calculated metric along with the reference conditions (stage, speed, or power)
#' at which it was measured.
#'
#' @param modality Exercise modality: "cycling" or "treadmill"
#' @param gross_efficiency Cycling gross efficiency as percentage (typical range 18-25%)
#' @param running_economy Running economy in mL O2/kg/km (typical range 150-250)
#' @param reference_stage Stage number at which economy was calculated
#' @param reference_speed Speed at which economy was calculated in km/h (treadmill)
#' @param reference_power Power at which efficiency was calculated in watts (cycling)
#'
#' @return An EconomyMetrics S7 object
#'
#' @examples
#' # Cycling efficiency
#' economy <- EconomyMetrics(
#'   modality = "cycling",
#'   gross_efficiency = 22.5,
#'   reference_stage = 4,
#'   reference_power = 200
#' )
#'
#' # Running economy
#' economy <- EconomyMetrics(
#'   modality = "treadmill",
#'   running_economy = 195,
#'   reference_stage = 3,
#'   reference_speed = 12
#' )
#'
#' @export
EconomyMetrics <- new_class("EconomyMetrics",
  properties = list(
    modality = new_property(class_character,
      default = "cycling",
      validator = function(value) {
        if (length(value) > 0 && !value %in% c("cycling", "treadmill")) {
          return("modality must be 'cycling' or 'treadmill'")
        }
        NULL
      }
    ),
    gross_efficiency = new_property(class_numeric | NULL,
      validator = function(value) {
        if (!is.null(value) && length(value) > 0 && !anyNA(value) && (value < 10 || value > 35)) {
          return("gross_efficiency should be between 10 and 35%")
        }
        NULL
      }
    ),
    running_economy = new_property(class_numeric | NULL,
      validator = function(value) {
        if (!is.null(value) && length(value) > 0 && !anyNA(value) && (value < 100 || value > 350)) {
          return("running_economy should be between 100 and 350 mL/kg/km")
        }
        NULL
      }
    ),
    reference_stage = class_numeric | NULL,
    reference_speed = class_numeric | NULL,
    reference_power = class_numeric | NULL
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
#' @param pre_test_conditions Optional PreTestConditions S7 object
#' @param protocol_config Optional ProtocolConfig S7 object
#' @param economy_metrics Optional EconomyMetrics S7 object
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
    validation = ValidationReport | NULL,
    # Additional context and metrics (optional)
    pre_test_conditions = PreTestConditions | NULL,
    protocol_config = ProtocolConfig | NULL,
    economy_metrics = EconomyMetrics | NULL
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
#' @param lab_name Optional laboratory name
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
      default = "fr",
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


# ExerciseQualityCriteria --------------------------------------------------

#' Exercise Quality Criteria Class
#'
#' @description
#' Assessment of ACSM maximal exercise criteria for determining if a CPET test
#' achieved true maximal effort. Based on ACSM Guidelines for Exercise Testing
#' and Prescription (11th edition).
#'
#' @param peak_rer Peak respiratory exchange ratio achieved
#' @param rer_met Logical indicating if RER criterion was met (>= 1.10)
#' @param peak_hr Peak heart rate achieved in bpm
#' @param predicted_hr_max Age-predicted maximum heart rate (220 - age)
#' @param hr_pct_predicted Percentage of predicted max HR achieved
#' @param hr_met Logical indicating if HR criterion was met (>= 85% predicted)
#' @param vo2_plateau_detected Logical indicating if VO2 plateau was detected
#' @param vo2_plateau_delta Change in VO2 (mL/min) during plateau assessment
#' @param plateau_met Logical indicating if plateau criterion was met (< 150 mL/min increase)
#' @param rpe_reported Optional reported RPE on 6-20 Borg scale
#' @param rpe_met Optional logical indicating if RPE criterion was met (>= 17)
#' @param lactate_mmol Optional blood lactate concentration in mmol/L
#' @param lactate_met Optional logical indicating if lactate criterion was met (>= 8.0)
#' @param criteria_met Number of criteria met
#' @param criteria_available Number of criteria that could be assessed
#' @param is_maximal Logical indicating if test is considered maximal (>= 3 criteria)
#' @param determination Character description: "maximal", "likely_maximal", "submaximal", "indeterminate"
#'
#' @return An ExerciseQualityCriteria S7 object
#'
#' @export
ExerciseQualityCriteria <- new_class("ExerciseQualityCriteria",
  properties = list(
    # RER criterion
    peak_rer = new_property(class_numeric,
      validator = function(value) {
        if (length(value) > 0 && !anyNA(value) && (value < 0.5 || value > 2.0)) {
          return("peak_rer should be between 0.5 and 2.0")
        }
        NULL
      }
    ),
    rer_met = class_logical,

    # HR criterion
    peak_hr = class_numeric | NULL,
    predicted_hr_max = class_numeric | NULL,
    hr_pct_predicted = class_numeric | NULL,
    hr_met = class_logical | NULL,

    # VO2 plateau criterion
    vo2_plateau_detected = class_logical,
    vo2_plateau_delta = class_numeric | NULL,
    plateau_met = class_logical,

    # RPE criterion (optional - user reported)
    rpe_reported = new_property(class_numeric | NULL,
      validator = function(value) {
        if (!is.null(value) && length(value) > 0 && !anyNA(value) && (value < 6 || value > 20)) {
          return("rpe_reported should be between 6 and 20 (Borg scale)")
        }
        NULL
      }
    ),
    rpe_met = class_logical | NULL,

    # Lactate criterion (optional - lab measured)
    lactate_mmol = new_property(class_numeric | NULL,
      validator = function(value) {
        if (!is.null(value) && length(value) > 0 && !anyNA(value) && (value < 0 || value > 30)) {
          return("lactate_mmol should be between 0 and 30 mmol/L")
        }
        NULL
      }
    ),
    lactate_met = class_logical | NULL,

    # Summary
    criteria_met = class_integer,
    criteria_available = class_integer,
    is_maximal = class_logical,
    determination = new_property(class_character,
      validator = function(value) {
        valid <- c("maximal", "likely_maximal", "submaximal", "indeterminate")
        if (!value %in% valid) {
          return(paste("determination must be one of:", paste(valid, collapse = ", ")))
        }
        NULL
      }
    )
  )
)


# ProtocolQuality ----------------------------------------------------------

#' Protocol Quality Class
#'
#' @description
#' Assessment of exercise protocol quality based on the relationship between
#' oxygen uptake and workload. Evaluates whether the observed VO2/workload
#' relationship matches expected physiological responses.
#'
#' @param modality Exercise modality: "cycling" or "treadmill"
#' @param expected_vo2_slope Expected VO2/workload slope (mL/min/W for cycling, mL/kg/min/kph for treadmill)
#' @param actual_vo2_slope Observed VO2/workload slope from linear regression
#' @param slope_deviation_pct Percentage deviation from expected slope
#' @param slope_acceptable Logical indicating if slope is within acceptable range (+/- 15%)
#' @param r_squared R-squared value from VO2 vs workload regression
#' @param r_squared_acceptable Logical indicating if R² >= 0.90
#' @param n_stages Number of exercise stages analyzed
#' @param stage_cv Coefficient of variation of steady-state VO2 within stages (%)
#' @param stage_cv_acceptable Logical indicating if CV <= 10%
#' @param test_duration_s Total test duration in seconds
#' @param exercise_duration_s Exercise phase duration in seconds
#' @param duration_optimal Logical indicating if exercise duration is 8-12 minutes
#' @param warmup_duration_s Warm-up phase duration in seconds
#' @param warmup_adequate Logical indicating if warm-up >= 2 minutes
#' @param overall_rating Overall protocol quality: "excellent", "good", "acceptable", "poor"
#' @param overall_score Numeric score 0-100
#' @param stage_details Data frame with expected vs actual VO2 per stage
#'
#' @return A ProtocolQuality S7 object
#'
#' @export
ProtocolQuality <- new_class("ProtocolQuality",
  properties = list(
    modality = new_property(class_character,
      validator = function(value) {
        if (!value %in% c("cycling", "treadmill", "unknown")) {
          return("modality must be 'cycling', 'treadmill', or 'unknown'")
        }
        NULL
      }
    ),

    # VO2/workload relationship
    expected_vo2_slope = class_numeric | NULL,
    actual_vo2_slope = class_numeric | NULL,
    slope_deviation_pct = class_numeric | NULL,
    slope_acceptable = class_logical | NULL,
    r_squared = new_property(class_numeric | NULL,
      validator = function(value) {
        if (!is.null(value) && length(value) > 0 && !anyNA(value) && (value < 0 || value > 1)) {
          return("r_squared must be between 0 and 1")
        }
        NULL
      }
    ),
    r_squared_acceptable = class_logical | NULL,

    # Stage consistency
    n_stages = class_integer | NULL,
    stage_cv = class_numeric | NULL,
    stage_cv_acceptable = class_logical | NULL,

    # Duration metrics
    test_duration_s = class_numeric,
    exercise_duration_s = class_numeric | NULL,
    duration_optimal = class_logical | NULL,
    warmup_duration_s = class_numeric | NULL,
    warmup_adequate = class_logical | NULL,

    # Overall assessment
    overall_rating = new_property(class_character,
      validator = function(value) {
        valid <- c("excellent", "good", "acceptable", "poor", "unable_to_assess")
        if (!value %in% valid) {
          return(paste("overall_rating must be one of:", paste(valid, collapse = ", ")))
        }
        NULL
      }
    ),
    overall_score = new_property(class_numeric,
      validator = function(value) {
        if (length(value) > 0 && !anyNA(value) && (value < 0 || value > 100)) {
          return("overall_score must be between 0 and 100")
        }
        NULL
      }
    ),

    # Stage-by-stage details
    stage_details = class_data.frame | NULL
  )
)


# DataQualityReport --------------------------------------------------------

#' Data Quality Report Class
#'
#' @description
#' Comprehensive assessment of breath-by-breath data quality including signal
#' quality, missing data, aberrant breaths, and equipment artifacts.
#'
#' @param n_breaths Total number of breaths in dataset
#' @param n_aberrant Number of aberrant breaths detected
#' @param pct_aberrant Percentage of aberrant breaths
#' @param aberrant_acceptable Logical indicating if aberrant rate <= 5%
#' @param n_missing_hr Number of missing HR values
#' @param pct_missing_hr Percentage of missing HR data
#' @param hr_acceptable Logical indicating if missing HR <= 10%
#' @param n_missing_power Number of missing power values
#' @param pct_missing_power Percentage of missing power data
#' @param baseline_vo2_cv Coefficient of variation of resting VO2 (%)
#' @param baseline_stable Logical indicating if baseline CV <= 10%
#' @param signal_quality_score Signal quality score 0-100
#' @param signal_quality_rating Signal quality: "excellent", "good", "acceptable", "poor"
#' @param calibration_drift_detected Logical indicating potential calibration drift
#' @param artifact_timestamps Vector of timestamps with detected artifacts
#' @param overall_score Overall data quality score 0-100
#' @param overall_rating Overall rating: "excellent", "good", "acceptable", "poor"
#' @param recommendations List of recommendations for data interpretation
#'
#' @return A DataQualityReport S7 object
#'
#' @export
DataQualityReport <- new_class("DataQualityReport",
  properties = list(
    # Breath counts
    n_breaths = class_integer,
    n_aberrant = class_integer,
    pct_aberrant = class_numeric,
    aberrant_acceptable = class_logical,

    # Missing HR data
    n_missing_hr = class_integer | NULL,
    pct_missing_hr = class_numeric | NULL,
    hr_acceptable = class_logical | NULL,

    # Missing power data
    n_missing_power = class_integer | NULL,
    pct_missing_power = class_numeric | NULL,

    # Baseline stability
    baseline_vo2_cv = class_numeric | NULL,
    baseline_stable = class_logical | NULL,

    # Signal quality
    signal_quality_score = new_property(class_numeric,
      validator = function(value) {
        if (length(value) > 0 && !anyNA(value) && (value < 0 || value > 100)) {
          return("signal_quality_score must be between 0 and 100")
        }
        NULL
      }
    ),
    signal_quality_rating = new_property(class_character,
      validator = function(value) {
        valid <- c("excellent", "good", "acceptable", "poor")
        if (!value %in% valid) {
          return(paste("signal_quality_rating must be one of:", paste(valid, collapse = ", ")))
        }
        NULL
      }
    ),

    # Calibration
    calibration_drift_detected = class_logical,
    artifact_timestamps = class_numeric | NULL,

    # Overall assessment
    overall_score = new_property(class_numeric,
      validator = function(value) {
        if (length(value) > 0 && !anyNA(value) && (value < 0 || value > 100)) {
          return("overall_score must be between 0 and 100")
        }
        NULL
      }
    ),
    overall_rating = new_property(class_character,
      validator = function(value) {
        valid <- c("excellent", "good", "acceptable", "poor")
        if (!value %in% valid) {
          return(paste("overall_rating must be one of:", paste(valid, collapse = ", ")))
        }
        NULL
      }
    ),
    recommendations = class_list
  )
)


# QualityAssessment --------------------------------------------------------

#' Quality Assessment Class
#'
#' @description
#' Complete quality assessment combining exercise criteria, protocol quality,
#' and data quality into a unified report.
#'
#' @param exercise_criteria ExerciseQualityCriteria S7 object
#' @param protocol_quality ProtocolQuality S7 object
#' @param data_quality DataQualityReport S7 object
#' @param overall_grade Overall test grade: "A", "B", "C", "D", "F"
#' @param overall_score Combined quality score 0-100
#' @param test_interpretable Logical indicating if test results can be reliably interpreted
#' @param summary_text Brief text summary of quality assessment
#'
#' @return A QualityAssessment S7 object
#'
#' @export
QualityAssessment <- new_class("QualityAssessment",
  properties = list(
    exercise_criteria = ExerciseQualityCriteria | NULL,
    protocol_quality = ProtocolQuality | NULL,
    data_quality = DataQualityReport | NULL,
    overall_grade = new_property(class_character,
      validator = function(value) {
        if (!value %in% c("A", "B", "C", "D", "F")) {
          return("overall_grade must be 'A', 'B', 'C', 'D', or 'F'")
        }
        NULL
      }
    ),
    overall_score = new_property(class_numeric,
      validator = function(value) {
        if (length(value) > 0 && !anyNA(value) && (value < 0 || value > 100)) {
          return("overall_score must be between 0 and 100")
        }
        NULL
      }
    ),
    test_interpretable = class_logical,
    summary_text = class_character
  )
)

# S7 Generic Function Definitions for cardiometR
# Core operations for CPET data analysis

#' Average CPET Data
#'
#' Apply averaging to breath-by-breath CPET data.
#'
#' @param x A CpetData object
#' @param ... Additional arguments passed to methods, including:
#'   \describe{
#'     \item{method}{Averaging method: "time" (time-based windows), "breath"
#'       (fixed number of breaths), or "rolling" (rolling/moving average)}
#'     \item{window}{Window size: seconds for "time" method, number of breaths
#'       for "breath" method, seconds for "rolling" method}
#'   }
#'
#' @return A new CpetData object with averaged data and is_averaged = TRUE
#'
#' @examples
#' \dontrun{
#' # 30-second time-based averaging
#' averaged <- average(cpet_data, method = "time", window = 30)
#'
#' # 10-breath averaging
#' averaged <- average(cpet_data, method = "breath", window = 10)
#'
#' # 30-second rolling average
#' averaged <- average(cpet_data, method = "rolling", window = 30)
#' }
#'
#' @export
average <- new_generic("average", "x")


#' Validate CPET Data
#'
#' Perform quality control checks on CPET data according to ATS/ACCP guidelines.
#'
#' @param x A CpetData object to validate
#' @param ... Additional arguments passed to methods
#'
#' @return A ValidationReport S7 object containing errors, warnings, and info
#'
#' @details
#' Validation checks include:
#' - Physiological plausibility of values
#' - Rest phase adequacy
#' - Aberrant breath detection
#' - Data completeness
#' - Calibration data presence
#'
#' @examples
#' \dontrun{
#' validation <- validate(cpet_data)
#' if (validation@is_valid) {
#'   message("Data passed validation")
#' } else {
#'   message("Validation errors: ", validation@errors)
#' }
#' }
#'
#' @export
validate <- new_generic("validate", "x")


#' Detect Ventilatory Thresholds
#'
#' Detect VT1 (aerobic threshold) and VT2 (respiratory compensation point)
#' using multiple methods.
#'
#' @param x A CpetData object (should be averaged)
#' @param ... Additional arguments passed to methods, including:
#'   \describe{
#'     \item{methods}{Character vector of detection methods to use. Options:
#'       "V-slope", "VE_VO2", "VE_VCO2", "PETO2", "PETCO2"}
#'   }
#'
#' @return A Thresholds S7 object with detected threshold values
#'
#' @details
#' Detection methods:
#' - **V-slope**: VCO2 vs VO2 breakpoint analysis (Beaver et al., 1986)
#' - **VE_VO2**: VE/VO2 nadir and subsequent rise
#' - **VE_VCO2**: VE/VCO2 rise for VT2
#' - **PETO2**: End-tidal O2 rise
#' - **PETCO2**: End-tidal CO2 plateau/decline
#'
#' @examples
#' \dontrun{
#' thresholds <- detect_thresholds(averaged_data, methods = c("V-slope", "VE_VO2"))
#' print(thresholds@vt1_vo2)
#' }
#'
#' @export
detect_thresholds <- new_generic("detect_thresholds", "x")


#' Find Peak Values
#'
#' Determine peak/maximal values from CPET data.
#'
#' @param x A CpetData object
#' @param ... Additional arguments passed to methods, including:
#'   \describe{
#'     \item{averaging}{Seconds to average for peak determination (default 30)}
#'   }
#'
#' @return A PeakValues S7 object containing peak VO2, HR, VE, RER, etc.
#'
#' @details
#' Peak values are determined using a rolling average of the specified window
#' size to reduce breath-by-breath variability. The recommended default is
#' 30 seconds per ATS/ACCP guidelines.
#'
#' @examples
#' \dontrun{
#' peaks <- find_peaks(cpet_data, averaging = 30)
#' print(peaks@vo2_peak)
#' print(peaks@vo2_kg_peak)
#' }
#'
#' @export
find_peaks <- new_generic("find_peaks", "x")


#' Extract Exercise Stages
#'
#' Identify and annotate exercise stages from a CPET protocol.
#'
#' @param x A CpetData object
#' @param ... Additional arguments passed to methods, including:
#'   \describe{
#'     \item{protocol}{Protocol type: "step" or "ramp"}
#'     \item{stage_duration}{Duration of each stage in seconds (for step protocols)}
#'   }
#'
#' @return A CpetData object with stages property populated
#'
#' @examples
#' \dontrun{
#' # Step protocol with 3-minute stages
#' data_with_stages <- extract_stages(cpet_data, protocol = "step", stage_duration = 180)
#' }
#'
#' @export
extract_stages <- new_generic("extract_stages", "x")


#' Summarize Data by Stage
#'
#' Create summary statistics for each exercise stage.
#'
#' @param x A CpetData object with stages defined
#' @param ... Additional arguments passed to methods, including:
#'   \describe{
#'     \item{method}{Summarization method: "last30s" (last 30 seconds of stage),
#'       "last60s", "mean" (stage mean), or "peak" (stage maximum)}
#'   }
#'
#' @return A tibble with one row per stage and summary statistics
#'
#' @examples
#' \dontrun{
#' stage_summary <- summarize_stages(data_with_stages, method = "last30s")
#' }
#'
#' @export
summarize_stages <- new_generic("summarize_stages", "x")

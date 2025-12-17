# S7 Generic Function Definitions for cardiometR
# Core operations for CPET data analysis

#' Average CPET Data
#'
#' Apply averaging to breath-by-breath CPET data.
#'
#' @param x A CpetData object
#' @param method Averaging method: "time" (time-based windows), "breath"
#'   (fixed number of breaths), or "rolling" (rolling/moving average)
#' @param window Window size: seconds for "time" method, number of breaths
#'   for "breath" method, seconds for "rolling" method
#' @param ... Additional arguments passed to methods
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
#' \dontrun
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
#' @param methods Character vector of detection methods to use. Options:
#'   "V-slope", "VE_VO2", "VE_VCO2", "PETO2", "PETCO2"
#' @param ... Additional arguments passed to methods
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
#' @param averaging Seconds to average for peak determination (default 30)
#' @param ... Additional arguments passed to methods
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
#' @param protocol Protocol type: "step" or "ramp"
#' @param stage_duration Duration of each stage in seconds (for step protocols)
#' @param ... Additional arguments passed to methods
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
#' @param method Summarization method: "last30s" (last 30 seconds of stage),
#'   "last60s", "mean" (stage mean), or "peak" (stage maximum)
#' @param ... Additional arguments passed to methods
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


#' Assess Maximal Exercise Criteria
#'
#' Evaluate ACSM criteria for determining if a CPET test achieved maximal effort.
#'
#' @param x A CpetData object or CpetAnalysis object
#' @param rpe Optional RPE value reported by participant (6-20 Borg scale)
#' @param lactate Optional blood lactate concentration in mmol/L
#' @param rer_threshold RER threshold for maximal effort (default 1.10)
#' @param hr_threshold_pct HR threshold as percentage of predicted max (default 85)
#' @param plateau_threshold Maximum VO2 increase to indicate plateau (default 150 mL/min)
#' @param ... Additional arguments passed to methods
#'
#' @return An ExerciseQualityCriteria S7 object
#'
#' @details
#' Criteria assessed (based on ACSM Guidelines, 11th edition):
#' - **RER >= 1.10**: Primary criterion for maximal effort
#' - **HR >= 85% predicted max**: Using 220 - age formula
#' - **VO2 plateau**: < 150 mL/min increase despite increased workload
#' - **RPE >= 17**: If provided (Borg 6-20 scale)
#' - **Blood lactate >= 8.0 mmol/L**: If provided
#'
#' A test is considered maximal if >= 3 criteria are met.
#'
#' @examples
#' \dontrun{
#' criteria <- assess_maximal_criteria(cpet_data)
#' if (criteria@is_maximal) {
#'   message("Test achieved maximal effort")
#' }
#'
#' # With optional RPE and lactate
#' criteria <- assess_maximal_criteria(cpet_data, rpe = 19, lactate = 10.2)
#' }
#'
#' @export
assess_maximal_criteria <- new_generic("assess_maximal_criteria", "x")


#' Assess Protocol Quality
#'
#' Evaluate the quality of the exercise protocol based on the VO2/workload
#' relationship and test parameters.
#'
#' @param x A CpetData object with stages defined
#' @param modality Exercise modality: "cycling", "treadmill", or "auto" (default)
#' @param expected_slope Expected VO2/W slope (NULL = use default for modality)
#' @param ... Additional arguments passed to methods
#'
#' @return A ProtocolQuality S7 object
#'
#' @details
#' For cycling, the expected VO2/workload slope is approximately 10 mL/min/W
#' (range 9-11 mL/min/W is acceptable). This reflects the oxygen cost of
#' external work on a cycle ergometer.
#'
#' Quality indicators:
#' - **Slope deviation**: Actual vs expected VO2/W relationship

#' - **R-squared**: Linearity of the VO2/workload relationship (>0.95 excellent)
#' - **Stage CV**: Consistency of steady-state values within stages (<10% good)
#' - **Test duration**: Exercise phase 8-12 minutes is optimal
#' - **Warm-up**: Adequate preparation period (>= 2 minutes)
#'
#' @examples
#' \dontrun{
#' protocol <- assess_protocol_quality(cpet_data)
#' print(protocol@overall_rating)
#' print(protocol@stage_details)
#' }
#'
#' @export
assess_protocol_quality <- new_generic("assess_protocol_quality", "x")


#' Assess Data Quality
#'
#' Evaluate breath-by-breath data quality including signal quality,
#' missing data, and aberrant breaths.
#'
#' @param x A CpetData object
#' @param aberrant_threshold SD threshold for aberrant breath detection (default 3)
#' @param ... Additional arguments passed to methods
#'
#' @return A DataQualityReport S7 object
#'
#' @details
#' Quality factors assessed:
#' - **Aberrant breaths**: Percentage of outlier breaths (>3 SD from local mean)
#' - **Missing HR**: Percentage of missing heart rate values
#' - **Missing power**: Percentage of missing power values
#' - **Baseline stability**: CV of resting VO2
#' - **Signal quality**: Overall assessment of measurement quality
#' - **Calibration drift**: Detection of systematic changes over time
#'
#' Scoring rubric (0-100):
#' - 90-100: Excellent (<2% aberrant, <5% missing, stable baseline)
#' - 75-89: Good (<5% aberrant, <10% missing)
#' - 60-74: Acceptable (<10% aberrant, <20% missing)
#' - <60: Poor (>10% aberrant or >20% missing)
#'
#' @examples
#' \dontrun{
#' quality <- assess_data_quality(cpet_data)
#' print(quality@overall_score)
#' print(quality@recommendations)
#' }
#'
#' @export
assess_data_quality <- new_generic("assess_data_quality", "x")


#' Compute Overall Quality Assessment
#'
#' Combine exercise criteria, protocol quality, and data quality into
#' a comprehensive quality assessment.
#'
#' @param x A CpetData or CpetAnalysis object
#' @param rpe Optional RPE value for maximal criteria assessment
#' @param lactate Optional lactate value for maximal criteria assessment
#' @param ... Additional arguments passed to methods
#'
#' @return A QualityAssessment S7 object
#'
#' @details
#' The overall grade is determined by:
#' - **A (90-100)**: Excellent quality, maximal test, reliable results
#' - **B (75-89)**: Good quality, likely maximal, results interpretable
#' - **C (60-74)**: Acceptable quality, some limitations noted
#' - **D (50-59)**: Poor quality, interpret with caution
#' - **F (<50)**: Unacceptable quality, results unreliable
#'
#' The assessment combines:
#' - Exercise criteria (40% weight): Was maximal effort achieved?
#' - Protocol quality (30% weight): Was the protocol appropriate?
#' - Data quality (30% weight): Is the data reliable?
#'
#' @examples
#' \dontrun{
#' assessment <- assess_quality(cpet_data)
#' print(assessment@overall_grade)
#' print(assessment@summary_text)
#' }
#'
#' @export
assess_quality <- new_generic("assess_quality", "x")

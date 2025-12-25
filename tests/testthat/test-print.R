# tests/testthat/test-print.R
# Tests for S7 print methods
# Focus on: returns invisibly, no errors, correct class handling

# =============================================================================
# Participant Print Method
# =============================================================================

test_that("print.Participant returns object invisibly", {
  p <- Participant(
    id = "P001",
    name = "John Doe",
    age = 35,
    sex = "M",
    height_cm = 180,
    weight_kg = 75
  )

  result <- expect_invisible(print(p))
  expect_identical(result, p)
})

test_that("print.Participant works with optional sport", {
  p <- Participant(
    id = "P001",
    name = "John Doe",
    age = 35,
    sex = "M",
    height_cm = 180,
    weight_kg = 75,
    sport = "cycling"
  )

  expect_no_error(print(p))
})

test_that("print.Participant works without sport", {
  p <- Participant(
    id = "P001",
    name = "John Doe",
    age = 35,
    sex = "M",
    height_cm = 180,
    weight_kg = 75
  )

  expect_no_error(print(p))
})


# =============================================================================
# CpetMetadata Print Method
# =============================================================================

test_that("print.CpetMetadata returns object invisibly", {
  m <- CpetMetadata(
    test_date = as.Date("2024-01-15"),
    device = "COSMED Quark CPET",
    protocol = "Incremental ramp"
  )

  result <- expect_invisible(print(m))
  expect_identical(result, m)
})

test_that("print.CpetMetadata works with all optional fields", {
  m <- CpetMetadata(
    test_date = as.Date("2024-01-15"),
    device = "COSMED Quark CPET",
    protocol = "Bruce",
    technician = "Dr. Smith",
    calibration_date = as.Date("2024-01-14"),
    temperature_c = 22.5,
    pressure_mmhg = 760,
    humidity_pct = 45
  )

  expect_no_error(print(m))
})

test_that("print.CpetMetadata works with minimal fields", {
  m <- CpetMetadata(
    test_date = as.Date("2024-01-15"),
    device = "COSMED Quark CPET",
    protocol = "Ramp"
  )

  expect_no_error(print(m))
})


# =============================================================================
# CpetData Print Method
# =============================================================================

test_that("print.CpetData returns object invisibly", {
  # Use helper from helper-mock-data.R - extract breaths tibble from list
  mock_data <- create_mock_breath_data(n_breaths = 100)

  p <- Participant(
    id = "T001",
    name = "Test Subject",
    age = 30,
    sex = "M",
    height_cm = 175,
    weight_kg = 70
  )

  m <- CpetMetadata(
    test_date = Sys.Date(),
    device = "Mock Device",
    protocol = "Test Protocol"
  )

  cpet <- CpetData(
    participant = p,
    metadata = m,
    breaths = mock_data$breaths,
    is_averaged = FALSE
  )

  result <- expect_invisible(print(cpet))
  expect_identical(result, cpet)
})


# =============================================================================
# PeakValues Print Method
# =============================================================================

test_that("print.PeakValues returns object invisibly", {
  peaks <- PeakValues(
    vo2_peak = 3500,
    vo2_kg_peak = 50.0,
    ve_peak = 120.5,
    rer_peak = 1.15,
    averaging_s = 30,
    hr_peak = 185,
    power_peak = 300
  )

  result <- expect_invisible(print(peaks))
  expect_identical(result, peaks)
})

test_that("print.PeakValues works without optional fields", {
  peaks <- PeakValues(
    vo2_peak = 3500,
    vo2_kg_peak = 50.0,
    ve_peak = 120.5,
    rer_peak = 1.15,
    averaging_s = 30
  )

  expect_no_error(print(peaks))
})


# =============================================================================
# Thresholds Print Method
# =============================================================================

test_that("print.Thresholds returns object invisibly", {
  th <- Thresholds(
    vt1_vo2 = 2500,
    vt1_hr = 150,
    vt1_power = 200,
    vt1_method = "V-slope",
    vt2_vo2 = 3200,
    vt2_hr = 175,
    vt2_power = 280,
    vt2_method = "VE/VCO2",
    confidence = "high"
  )

  result <- expect_invisible(print(th))
  expect_identical(result, th)
})

test_that("print.Thresholds works with no thresholds detected", {
  th <- Thresholds(
    confidence = "unable"
  )

  expect_no_error(print(th))
})

test_that("print.Thresholds works with only VT1", {
  th <- Thresholds(
    vt1_vo2 = 2500,
    vt1_method = "V-slope",
    confidence = "moderate"
  )

  expect_no_error(print(th))
})


# =============================================================================
# ValidationReport Print Method
# =============================================================================

test_that("print.ValidationReport returns object invisibly", {
  vr <- ValidationReport(
    is_valid = TRUE,
    errors = list(),
    warnings = list(),
    info = list("Data loaded successfully")
  )

  result <- expect_invisible(print(vr))
  expect_identical(result, vr)
})

test_that("print.ValidationReport works with errors", {
  vr <- ValidationReport(
    is_valid = FALSE,
    errors = list("Missing required column: vo2_ml", "Invalid data format"),
    warnings = list(),
    info = list()
  )

  expect_no_error(print(vr))
})

test_that("print.ValidationReport works with warnings and info", {
  vr <- ValidationReport(
    is_valid = TRUE,
    errors = list(),
    warnings = list("HR data has 5% missing values"),
    info = list("300 breaths loaded", "Test duration: 10 minutes")
  )

  expect_no_error(print(vr))
})


# =============================================================================
# CpetAnalysis Print Method
# =============================================================================

test_that("print.CpetAnalysis returns object invisibly", {
  # Create minimal CpetData - extract breaths tibble from list
  mock_data <- create_mock_breath_data(n_breaths = 100)

  p <- Participant(
    id = "T001",
    name = "Test Subject",
    age = 30,
    sex = "M",
    height_cm = 175,
    weight_kg = 70
  )

  m <- CpetMetadata(
    test_date = Sys.Date(),
    device = "Mock Device",
    protocol = "Test Protocol"
  )

  cpet <- CpetData(
    participant = p,
    metadata = m,
    breaths = mock_data$breaths,
    is_averaged = FALSE
  )

  peaks <- PeakValues(
    vo2_peak = 3500,
    vo2_kg_peak = 50.0,
    ve_peak = 120.5,
    rer_peak = 1.15,
    averaging_s = 30
  )

  validation <- ValidationReport(
    is_valid = TRUE,
    errors = list(),
    warnings = list(),
    info = list("Test data loaded successfully")
  )

  analysis <- CpetAnalysis(
    data = cpet,
    peaks = peaks,
    validation = validation
  )

  result <- expect_invisible(print(analysis))
  expect_identical(result, analysis)
})


# =============================================================================
# ReportConfig Print Method
# =============================================================================

test_that("print.ReportConfig returns object invisibly", {
  config <- ReportConfig(
    language = "fr",
    institution = "Centre EPIC",
    lab_name = "Exercise Physiology Lab",
    technician = "Dr. Smith"
  )

  result <- expect_invisible(print(config))
  expect_identical(result, config)
})

test_that("print.ReportConfig works with minimal config", {
  config <- ReportConfig(language = "en")

  expect_no_error(print(config))
})


# =============================================================================
# ExerciseQualityCriteria Print Method
# =============================================================================

test_that("print.ExerciseQualityCriteria returns object invisibly", {
  criteria <- ExerciseQualityCriteria(
    peak_rer = 1.18,
    rer_met = TRUE,
    peak_hr = 185,
    predicted_hr_max = 190,
    hr_pct_predicted = 97.4,
    hr_met = TRUE,
    vo2_plateau_detected = FALSE,
    vo2_plateau_delta = 200,
    plateau_met = FALSE,
    rpe_reported = 18,
    rpe_met = TRUE,
    lactate_mmol = 9.5,
    lactate_met = TRUE,
    criteria_met = 4L,
    criteria_available = 5L,
    is_maximal = TRUE,
    determination = "maximal"
  )

  result <- expect_invisible(print(criteria))
  expect_identical(result, criteria)
})

test_that("print.ExerciseQualityCriteria works with submaximal determination", {
  # Note: Print method requires explicit NULL for optional fields
  # S7 union types may default to zero-length vectors instead of NULL
  criteria <- ExerciseQualityCriteria(
    peak_rer = 1.05,
    rer_met = FALSE,
    peak_hr = 160,
    predicted_hr_max = 190,
    hr_pct_predicted = 84.2,
    hr_met = FALSE,
    vo2_plateau_detected = FALSE,
    vo2_plateau_delta = NULL,
    plateau_met = FALSE,
    rpe_reported = NULL,
    rpe_met = NULL,
    lactate_mmol = NULL,
    lactate_met = NULL,
    criteria_met = 0L,
    criteria_available = 3L,
    is_maximal = FALSE,
    determination = "submaximal"
  )

  expect_no_error(print(criteria))
})


# =============================================================================
# ProtocolQuality Print Method
# =============================================================================

test_that("print.ProtocolQuality returns object invisibly", {
  pq <- ProtocolQuality(
    modality = "cycling",
    expected_vo2_slope = 10.0,
    actual_vo2_slope = 9.5,
    slope_deviation_pct = -5,
    slope_acceptable = TRUE,
    r_squared = 0.95,
    r_squared_acceptable = TRUE,
    n_stages = 8L,
    stage_cv = 5.5,
    stage_cv_acceptable = TRUE,
    test_duration_s = 720,
    exercise_duration_s = 600,
    duration_optimal = TRUE,
    warmup_duration_s = 120,
    warmup_adequate = TRUE,
    overall_rating = "excellent",
    overall_score = 95
  )

  result <- expect_invisible(print(pq))
  expect_identical(result, pq)
})

test_that("print.ProtocolQuality works with unknown modality", {
  # Note: Print method requires explicit NULL for optional fields
  pq <- ProtocolQuality(
    modality = "unknown",
    expected_vo2_slope = NULL,
    actual_vo2_slope = NULL,
    slope_deviation_pct = NULL,
    slope_acceptable = NULL,
    r_squared = NULL,
    r_squared_acceptable = NULL,
    n_stages = NULL,
    stage_cv = NULL,
    stage_cv_acceptable = NULL,
    test_duration_s = 720,
    exercise_duration_s = NULL,
    duration_optimal = NULL,
    warmup_duration_s = NULL,
    warmup_adequate = NULL,
    overall_rating = "unable_to_assess",
    overall_score = 50
  )

  expect_no_error(print(pq))
})


# =============================================================================
# DataQualityReport Print Method
# =============================================================================

test_that("print.DataQualityReport returns object invisibly", {
  dq <- DataQualityReport(
    n_breaths = 300L,
    n_aberrant = 5L,
    pct_aberrant = 1.67,
    aberrant_acceptable = TRUE,
    n_missing_hr = 10L,
    pct_missing_hr = 3.33,
    hr_acceptable = TRUE,
    n_missing_power = 0L,
    pct_missing_power = 0,
    baseline_vo2_cv = 8.5,
    baseline_stable = TRUE,
    signal_quality_score = 90,
    signal_quality_rating = "excellent",
    calibration_drift_detected = FALSE,
    overall_score = 88,
    overall_rating = "good",
    recommendations = list("Data quality is acceptable.")
  )

  result <- expect_invisible(print(dq))
  expect_identical(result, dq)
})

test_that("print.DataQualityReport works with excellent quality", {
  # Note: Print method requires hr_acceptable if pct_missing_hr is set
  dq <- DataQualityReport(
    n_breaths = 100L,
    n_aberrant = 0L,
    pct_aberrant = 0,
    aberrant_acceptable = TRUE,
    n_missing_hr = 0L,
    pct_missing_hr = 0,
    hr_acceptable = TRUE,
    baseline_vo2_cv = 5.0,
    baseline_stable = TRUE,
    signal_quality_score = 100,
    signal_quality_rating = "excellent",
    calibration_drift_detected = FALSE,
    overall_score = 100,
    overall_rating = "excellent",
    recommendations = list()
  )

  expect_no_error(print(dq))
})


# =============================================================================
# QualityAssessment Print Method
# =============================================================================

test_that("print.QualityAssessment returns object invisibly", {
  exercise <- ExerciseQualityCriteria(
    peak_rer = 1.18,
    rer_met = TRUE,
    vo2_plateau_detected = FALSE,
    plateau_met = FALSE,
    criteria_met = 2L,
    criteria_available = 3L,
    is_maximal = TRUE,
    determination = "likely_maximal"
  )

  protocol <- ProtocolQuality(
    modality = "cycling",
    test_duration_s = 720,
    overall_rating = "good",
    overall_score = 82
  )

  data_qual <- DataQualityReport(
    n_breaths = 300L,
    n_aberrant = 5L,
    pct_aberrant = 1.67,
    aberrant_acceptable = TRUE,
    signal_quality_score = 90,
    signal_quality_rating = "excellent",
    calibration_drift_detected = FALSE,
    overall_score = 88,
    overall_rating = "good",
    recommendations = list()
  )

  qa <- QualityAssessment(
    exercise_criteria = exercise,
    protocol_quality = protocol,
    data_quality = data_qual,
    overall_grade = "B",
    overall_score = 85,
    test_interpretable = TRUE,
    summary_text = "Test quality is good."
  )

  result <- expect_invisible(print(qa))
  expect_identical(result, qa)
})

test_that("print.QualityAssessment works with low grade", {
  # Note: QualityAssessment requires sub-objects with valid defaults
  exercise <- ExerciseQualityCriteria(
    peak_rer = 1.05,
    rer_met = FALSE,
    peak_hr = 150,
    predicted_hr_max = 190,
    hr_pct_predicted = 78.9,
    hr_met = FALSE,
    vo2_plateau_detected = FALSE,
    vo2_plateau_delta = 0,
    plateau_met = FALSE,
    criteria_met = 0L,
    criteria_available = 3L,
    is_maximal = FALSE,
    determination = "submaximal"
  )

  protocol <- ProtocolQuality(
    modality = "unknown",
    test_duration_s = 720,
    exercise_duration_s = 600,
    duration_optimal = FALSE,
    overall_rating = "poor",
    overall_score = 40
  )

  data_qual <- DataQualityReport(
    n_breaths = 100L,
    n_aberrant = 10L,
    pct_aberrant = 10,
    aberrant_acceptable = FALSE,
    n_missing_hr = 20L,
    pct_missing_hr = 20,
    hr_acceptable = FALSE,
    signal_quality_score = 50,
    signal_quality_rating = "poor",
    calibration_drift_detected = TRUE,
    overall_score = 40,
    overall_rating = "poor",
    recommendations = list("Data quality needs improvement.")
  )

  qa <- QualityAssessment(
    exercise_criteria = exercise,
    protocol_quality = protocol,
    data_quality = data_qual,
    overall_grade = "D",
    overall_score = 45,
    test_interpretable = FALSE,
    summary_text = "Test quality is poor."
  )

  expect_no_error(print(qa))
})

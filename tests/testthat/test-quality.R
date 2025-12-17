# Tests for Quality Assessment Methods

# Helper to create CpetData object for testing
create_test_cpet_data <- function(n_breaths = 300,
                                   include_hr = TRUE,
                                   include_power = TRUE,
                                   peak_rer = 1.15,
                                   include_phase = FALSE) {
  mock <- create_mock_breath_data(
    n_breaths = n_breaths,
    include_hr = include_hr,
    include_power = include_power
  )

  breaths <- mock$breaths

  # Adjust RER to achieve target peak
  if (!is.null(peak_rer)) {
    # Scale RER values to hit the peak
    current_max <- max(breaths$rer, na.rm = TRUE)
    breaths$rer <- breaths$rer * (peak_rer / current_max)
    # Also adjust VCO2 to be consistent
    breaths$vco2_ml <- breaths$vo2_ml * breaths$rer
  }

  # Add phase column if requested
  if (include_phase) {
    n <- nrow(breaths)
    breaths$phase <- c(
      rep("REST", floor(n * 0.1)),
      rep("WARMUP", floor(n * 0.1)),
      rep("EXERCISE", floor(n * 0.7)),
      rep("RECOVERY", n - floor(n * 0.1) - floor(n * 0.1) - floor(n * 0.7))
    )
  }

  participant <- Participant(
    id = mock$participant$id,
    name = mock$participant$name,
    age = mock$participant$age,
    sex = mock$participant$sex,
    height_cm = mock$participant$height_cm,
    weight_kg = mock$participant$weight_kg
  )

  metadata <- CpetMetadata(
    test_date = mock$metadata$test_date,
    device = mock$metadata$device,
    protocol = mock$metadata$protocol,
    temperature_c = mock$metadata$temperature_c,
    pressure_mmhg = mock$metadata$pressure_mmhg
  )

  CpetData(
    participant = participant,
    metadata = metadata,
    breaths = breaths,
    is_averaged = FALSE
  )
}


# =============================================================================
# ExerciseQualityCriteria Class Tests
# =============================================================================

test_that("ExerciseQualityCriteria class validates correctly", {
  # Valid construction
  criteria <- ExerciseQualityCriteria(
    peak_rer = 1.15,
    rer_met = TRUE,
    peak_hr = 185,
    predicted_hr_max = 190,
    hr_pct_predicted = 97.4,
    hr_met = TRUE,
    vo2_plateau_detected = TRUE,
    vo2_plateau_delta = 120,
    plateau_met = TRUE,
    criteria_met = 3L,
    criteria_available = 3L,
    is_maximal = TRUE,
    determination = "maximal"
  )

  expect_true(is_s7_class(criteria, "ExerciseQualityCriteria"))
  expect_equal(criteria@peak_rer, 1.15)
  expect_true(criteria@is_maximal)
  expect_equal(criteria@determination, "maximal")
})

test_that("ExerciseQualityCriteria validates RER range", {
  expect_error(
    ExerciseQualityCriteria(
      peak_rer = 2.5,  # Invalid
      rer_met = TRUE,
      vo2_plateau_detected = FALSE,
      plateau_met = FALSE,
      criteria_met = 1L,
      criteria_available = 1L,
      is_maximal = FALSE,
      determination = "submaximal"
    ),
    "peak_rer"
  )
})

test_that("ExerciseQualityCriteria validates determination values", {
  expect_error(
    ExerciseQualityCriteria(
      peak_rer = 1.15,
      rer_met = TRUE,
      vo2_plateau_detected = FALSE,
      plateau_met = FALSE,
      criteria_met = 1L,
      criteria_available = 1L,
      is_maximal = FALSE,
      determination = "invalid_value"
    ),
    "determination"
  )
})


# =============================================================================
# ProtocolQuality Class Tests
# =============================================================================

test_that("ProtocolQuality class validates correctly", {
  protocol <- ProtocolQuality(
    modality = "cycling",
    expected_vo2_slope = 10.0,
    actual_vo2_slope = 10.2,
    slope_deviation_pct = 2.0,
    slope_acceptable = TRUE,
    r_squared = 0.97,
    r_squared_acceptable = TRUE,
    test_duration_s = 720,
    exercise_duration_s = 600,
    duration_optimal = TRUE,
    overall_rating = "excellent",
    overall_score = 95
  )

  expect_true(is_s7_class(protocol, "ProtocolQuality"))
  expect_equal(protocol@modality, "cycling")
  expect_equal(protocol@overall_rating, "excellent")
})

test_that("ProtocolQuality validates modality values", {
  expect_error(
    ProtocolQuality(
      modality = "swimming",  # Invalid
      test_duration_s = 600,
      overall_rating = "good",
      overall_score = 80
    ),
    "modality"
  )
})

test_that("ProtocolQuality validates R-squared range", {
  expect_error(
    ProtocolQuality(
      modality = "cycling",
      r_squared = 1.5,  # Invalid
      test_duration_s = 600,
      overall_rating = "good",
      overall_score = 80
    ),
    "r_squared"
  )
})


# =============================================================================
# DataQualityReport Class Tests
# =============================================================================

test_that("DataQualityReport class validates correctly", {
  report <- DataQualityReport(
    n_breaths = 300L,
    n_aberrant = 5L,
    pct_aberrant = 1.67,
    aberrant_acceptable = TRUE,
    n_missing_hr = 0L,
    pct_missing_hr = 0.0,
    hr_acceptable = TRUE,
    signal_quality_score = 95,
    signal_quality_rating = "excellent",
    calibration_drift_detected = FALSE,
    overall_score = 92,
    overall_rating = "excellent",
    recommendations = list("Data quality is acceptable.")
  )

  expect_true(is_s7_class(report, "DataQualityReport"))
  expect_equal(report@n_breaths, 300L)
  expect_equal(report@overall_rating, "excellent")
})

test_that("DataQualityReport validates score range", {
  expect_error(
    DataQualityReport(
      n_breaths = 100L,
      n_aberrant = 5L,
      pct_aberrant = 5.0,
      aberrant_acceptable = TRUE,
      signal_quality_score = 150,  # Invalid
      signal_quality_rating = "good",
      calibration_drift_detected = FALSE,
      overall_score = 80,
      overall_rating = "good",
      recommendations = list()
    ),
    "signal_quality_score"
  )
})


# =============================================================================
# QualityAssessment Class Tests
# =============================================================================

test_that("QualityAssessment class validates grade", {
  expect_error(
    QualityAssessment(
      overall_grade = "X",  # Invalid
      overall_score = 85,
      test_interpretable = TRUE,
      summary_text = "Test summary"
    ),
    "overall_grade"
  )
})


# =============================================================================
# assess_maximal_criteria Method Tests
# =============================================================================

test_that("assess_maximal_criteria identifies maximal test", {
  cpet <- create_test_cpet_data(peak_rer = 1.18)

  result <- assess_maximal_criteria(cpet)

  expect_true(is_s7_class(result, "ExerciseQualityCriteria"))
  expect_true(result@rer_met)
  expect_true(result@peak_rer >= 1.10)
  expect_equal(result@determination, "maximal")
})

test_that("assess_maximal_criteria identifies submaximal test", {
  cpet <- create_test_cpet_data(peak_rer = 1.02)

  result <- assess_maximal_criteria(cpet)

  expect_false(result@rer_met)
  expect_false(result@is_maximal)
  expect_true(result@determination %in% c("submaximal", "indeterminate"))
})

test_that("assess_maximal_criteria handles HR criterion", {
  cpet <- create_test_cpet_data(include_hr = TRUE)

  result <- assess_maximal_criteria(cpet)

  expect_false(is.null(result@peak_hr))
  expect_false(is.null(result@hr_pct_predicted))
  expect_false(is.null(result@hr_met))
})

test_that("assess_maximal_criteria handles missing HR", {
  cpet <- create_test_cpet_data(include_hr = FALSE)

  result <- assess_maximal_criteria(cpet)

  expect_null(result@peak_hr)
  expect_null(result@hr_met)
})

test_that("assess_maximal_criteria accepts optional RPE", {
  cpet <- create_test_cpet_data()

  # With RPE >= 17
  result_high_rpe <- assess_maximal_criteria(cpet, rpe = 19)
  expect_true(result_high_rpe@rpe_met)
  expect_equal(result_high_rpe@rpe_reported, 19)

  # With RPE < 17
  result_low_rpe <- assess_maximal_criteria(cpet, rpe = 14)
  expect_false(result_low_rpe@rpe_met)
})

test_that("assess_maximal_criteria accepts optional lactate", {
  cpet <- create_test_cpet_data()

  # With lactate >= 8.0
  result_high_lac <- assess_maximal_criteria(cpet, lactate = 10.5)
  expect_true(result_high_lac@lactate_met)
  expect_equal(result_high_lac@lactate_mmol, 10.5)

  # With lactate < 8.0
  result_low_lac <- assess_maximal_criteria(cpet, lactate = 5.0)
  expect_false(result_low_lac@lactate_met)
})


# =============================================================================
# assess_protocol_quality Method Tests
# =============================================================================

test_that("assess_protocol_quality returns valid ProtocolQuality", {
  cpet <- create_test_cpet_data(include_power = TRUE)

  result <- assess_protocol_quality(cpet)

  expect_true(is_s7_class(result, "ProtocolQuality"))
  expect_true(result@test_duration_s > 0)
  expect_true(result@overall_score >= 0 && result@overall_score <= 100)
  expect_true(result@overall_rating %in% c("excellent", "good", "acceptable", "poor", "unable_to_assess"))
})

test_that("assess_protocol_quality detects cycling modality", {
  cpet <- create_test_cpet_data(include_power = TRUE)

  result <- assess_protocol_quality(cpet, modality = "auto")

  expect_equal(result@modality, "cycling")
})

test_that("assess_protocol_quality analyzes VO2/W slope", {
  cpet <- create_test_cpet_data(include_power = TRUE)

  result <- assess_protocol_quality(cpet)

  # Should have slope analysis for cycling
  expect_false(is.null(result@actual_vo2_slope))
  expect_false(is.null(result@r_squared))
})


# =============================================================================
# assess_data_quality Method Tests
# =============================================================================

test_that("assess_data_quality returns valid DataQualityReport", {
  cpet <- create_test_cpet_data()

  result <- assess_data_quality(cpet)

  expect_true(is_s7_class(result, "DataQualityReport"))
  expect_equal(result@n_breaths, nrow(cpet@breaths))
  expect_true(result@overall_score >= 0 && result@overall_score <= 100)
})

test_that("assess_data_quality detects aberrant breaths", {
  cpet <- create_test_cpet_data()

  # Add some aberrant breaths
  cpet@breaths$vo2_ml[50:55] <- 10000  # Very high values

  result <- assess_data_quality(cpet)

  expect_true(result@n_aberrant > 0)
  expect_true(result@pct_aberrant > 0)
})

test_that("assess_data_quality handles missing HR", {
  cpet <- create_test_cpet_data(include_hr = TRUE)

  # Set some HR values to NA
  cpet@breaths$hr_bpm[1:30] <- NA

  result <- assess_data_quality(cpet)

  expect_true(result@pct_missing_hr > 0)
})

test_that("assess_data_quality generates recommendations", {
  cpet <- create_test_cpet_data()

  result <- assess_data_quality(cpet)

  expect_true(length(result@recommendations) > 0)
})


# =============================================================================
# assess_quality Method Tests
# =============================================================================

test_that("assess_quality returns valid QualityAssessment", {
  cpet <- create_test_cpet_data()

  result <- assess_quality(cpet)

  expect_true(is_s7_class(result, "QualityAssessment"))
  expect_true(is_s7_class(result@exercise_criteria, "ExerciseQualityCriteria"))
  expect_true(is_s7_class(result@protocol_quality, "ProtocolQuality"))
  expect_true(is_s7_class(result@data_quality, "DataQualityReport"))
})

test_that("assess_quality computes overall grade", {
  cpet <- create_test_cpet_data(peak_rer = 1.20)

  result <- assess_quality(cpet)

  expect_true(result@overall_grade %in% c("A", "B", "C", "D", "F"))
  expect_true(result@overall_score >= 0 && result@overall_score <= 100)
})

test_that("assess_quality generates summary text", {
  cpet <- create_test_cpet_data()

  result <- assess_quality(cpet)

  expect_true(nchar(result@summary_text) > 0)
})

test_that("assess_quality passes optional criteria to exercise assessment", {
  cpet <- create_test_cpet_data()

  result <- assess_quality(cpet, rpe = 18, lactate = 9.5)

  expect_equal(result@exercise_criteria@rpe_reported, 18)
  expect_equal(result@exercise_criteria@lactate_mmol, 9.5)
})

test_that("assess_quality determines interpretability", {
  # Good quality test
  cpet_good <- create_test_cpet_data(peak_rer = 1.18)
  result_good <- assess_quality(cpet_good)

  # Should be interpretable with good data
  # (exact result depends on all quality components)
  expect_true(is.logical(result_good@test_interpretable))
})


# =============================================================================
# Edge Cases and Error Handling
# =============================================================================
test_that("quality methods handle minimal data", {
  # Create minimal data
  minimal_breaths <- create_minimal_breaths(50)
  minimal_breaths$hr_bpm <- rep(120, 50)
  minimal_breaths$power_w <- seq(0, 100, length.out = 50)

  participant <- Participant(
    id = "MIN001",
    name = "Minimal",
    age = 25,
    sex = "F",
    height_cm = 165,
    weight_kg = 60
  )

  metadata <- CpetMetadata(
    test_date = Sys.Date(),
    device = "Test",
    protocol = "Test"
  )

  cpet <- CpetData(
    participant = participant,
    metadata = metadata,
    breaths = minimal_breaths,
    is_averaged = FALSE
  )

  # Should not error with minimal data
  expect_no_error(assess_maximal_criteria(cpet))
  expect_no_error(assess_protocol_quality(cpet))
  expect_no_error(assess_data_quality(cpet))
  expect_no_error(assess_quality(cpet))
})

test_that("quality methods handle data without power", {
  cpet <- create_test_cpet_data(include_power = FALSE)

  # Should still work without power data
  result <- assess_protocol_quality(cpet)

  expect_true(is_s7_class(result, "ProtocolQuality"))
  expect_equal(result@modality, "unknown")
})

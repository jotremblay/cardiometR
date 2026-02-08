# Tests for S7 method implementations
# Note: is_s7_class helper defined in helper-mock-data.R

# Helper to create test data
create_test_cpet_data <- function() {
  participant <- Participant(
    id = "TEST001",
    name = "Test Subject",
    age = 30,
    sex = "M",
    height_cm = 175,
    weight_kg = 70
  )

  metadata <- CpetMetadata(
    test_date = Sys.Date(),
    device = "Test Device",
    protocol = "Incremental"
  )

  # Create realistic breath data
  n <- 100
  time_s <- seq(0, by = 10, length.out = n)
  intensity <- pmin(time_s / max(time_s), 1)

  breaths <- tibble::tibble(
    time_s = time_s,
    vo2_ml = 300 + intensity * 2700 + rnorm(n, 0, 30),
    vco2_ml = 250 + intensity * 3000 + rnorm(n, 0, 30),
    ve_l = 10 + intensity * 120 + rnorm(n, 0, 2),
    rer = vco2_ml / vo2_ml,
    hr_bpm = 70 + intensity * 120 + rnorm(n, 0, 2),
    power_w = round(intensity * 300),
    bf = 12 + intensity * 38,
    vt_l = ve_l / bf,
    phase = dplyr::case_when(
      time_s < 120 ~ "REST",
      time_s < 300 ~ "WARMUP",
      TRUE ~ "EXERCISE"
    )
  )

  CpetData(
    participant = participant,
    metadata = metadata,
    breaths = breaths,
    is_averaged = FALSE
  )
}


# average() tests ----------------------------------------------------------

test_that("average() with time method works", {
  data <- create_test_cpet_data()
  averaged <- average(data, method = "time", window = 30)

  expect_true(is_s7_class(averaged, "CpetData"))
  expect_true(averaged@is_averaged)
  expect_equal(averaged@averaging_window, 30)
  expect_lt(nrow(averaged@breaths), nrow(data@breaths))
})

test_that("average() with breath method works", {
  data <- create_test_cpet_data()
  averaged <- average(data, method = "breath", window = 5)

  expect_true(is_s7_class(averaged, "CpetData"))
  expect_true(averaged@is_averaged)
  expect_equal(averaged@averaging_window, 5)
})

test_that("average() with rolling method works", {
  data <- create_test_cpet_data()
  averaged <- average(data, method = "rolling", window = 30)

  expect_true(is_s7_class(averaged, "CpetData"))
  expect_true(averaged@is_averaged)
  # Rolling average keeps same approximate row count (minus edges)
})

test_that("average() preserves required columns", {
  data <- create_test_cpet_data()
  averaged <- average(data, method = "time", window = 30)

  required_cols <- c("time_s", "vo2_ml", "vco2_ml", "ve_l", "rer")
  expect_true(all(required_cols %in% names(averaged@breaths)))
})

test_that("average() warns when re-averaging", {
  data <- create_test_cpet_data()
  averaged_once <- average(data, method = "time", window = 30)

  expect_warning(
    average(averaged_once, method = "time", window = 60),
    "already averaged"
  )
})


# validate() tests ---------------------------------------------------------

test_that("validate() returns ValidationReport", {
  data <- create_test_cpet_data()
  validation <- validate(data)

  expect_true(is_s7_class(validation, "ValidationReport"))
  expect_type(validation@is_valid, "logical")
  expect_type(validation@errors, "list")
  expect_type(validation@warnings, "list")
  expect_type(validation@info, "list")
})

test_that("validate() passes for valid data", {
  data <- create_test_cpet_data()
  validation <- validate(data)

  # Should pass (no errors) for valid test data
  expect_true(validation@is_valid)
  expect_equal(length(validation@errors), 0)
})

test_that("validate() detects insufficient data", {
  participant <- Participant(
    id = "X", name = "X", age = 30, sex = "M",
    height_cm = 175, weight_kg = 70
  )
  metadata <- CpetMetadata(
    test_date = Sys.Date(), device = "X", protocol = "X"
  )

  # Only 5 breaths - should fail
  breaths <- tibble::tibble(
    time_s = 1:5,
    vo2_ml = rep(300, 5),
    vco2_ml = rep(250, 5),
    ve_l = rep(10, 5),
    rer = rep(0.83, 5)
  )

  data <- CpetData(
    participant = participant,
    metadata = metadata,
    breaths = breaths,
    is_averaged = FALSE
  )

  validation <- validate(data)

  expect_false(validation@is_valid)
  expect_true(any(grepl("fewer than 10", validation@errors)))
})


# find_peaks() tests -------------------------------------------------------

test_that("find_peaks() returns PeakValues", {
  data <- create_test_cpet_data()
  peaks <- find_peaks(data, averaging = 30)

  expect_true(is_s7_class(peaks, "PeakValues"))
})

test_that("find_peaks() calculates reasonable values", {
  data <- create_test_cpet_data()
  peaks <- find_peaks(data, averaging = 30)

  # Peak VO2 should be higher than minimum
  min_vo2 <- min(data@breaths$vo2_ml)
  expect_gt(peaks@vo2_peak, min_vo2)

  # VO2/kg should be VO2 / weight
  expected_vo2_kg <- peaks@vo2_peak / data@participant@weight_kg
  expect_equal(peaks@vo2_kg_peak, expected_vo2_kg, tolerance = 0.1)

  # Peak RER should be positive
  expect_gt(peaks@rer_peak, 0)
  expect_lt(peaks@rer_peak, 2)
})

test_that("find_peaks() includes HR when available", {
  data <- create_test_cpet_data()
  peaks <- find_peaks(data, averaging = 30)

  expect_false(is.null(peaks@hr_peak))
  expect_gt(peaks@hr_peak, 60)
})

test_that("find_peaks() includes power when available", {
  data <- create_test_cpet_data()
  peaks <- find_peaks(data, averaging = 30)

  expect_false(is.null(peaks@power_peak))
  expect_gte(peaks@power_peak, 0)
})


# extract_stages() tests ---------------------------------------------------

test_that("extract_stages() adds stage column", {
  data <- create_test_cpet_data()
  with_stages <- extract_stages(data, protocol = "step", stage_duration = 180)

  expect_true("stage" %in% names(with_stages@breaths))
  expect_false(is.null(with_stages@stages))
})

test_that("extract_stages() uses phase column when available", {
  data <- create_test_cpet_data()
  with_stages <- extract_stages(data, protocol = "step", stage_duration = 180)

  # Should have created stages based on phase
  expect_gt(length(unique(with_stages@breaths$stage)), 1)
})

test_that("extract_stages() keeps rest at stage 0 when using power", {
  data <- create_test_cpet_data()
  data@breaths$phase <- NULL

  # Force clear rest period at zero power
  data@breaths$power_w[1:10] <- 0
  data@breaths$power_w[11:20] <- 50
  data@breaths$power_w[21:30] <- 75

  with_stages <- extract_stages(data, protocol = "step", stage_duration = 180)

  expect_true(all(with_stages@breaths$stage[1:10] == 0))
  expect_true(any(with_stages@breaths$stage > 0))
})


# summarize_stages() tests -------------------------------------------------

test_that("summarize_stages() returns tibble", {
  data <- create_test_cpet_data()
  with_stages <- extract_stages(data, protocol = "step", stage_duration = 180)
  summary <- summarize_stages(with_stages, method = "last30s")

  expect_s3_class(summary, "tbl_df")
  expect_true("stage" %in% names(summary))
  expect_true("vo2_ml" %in% names(summary))
})

test_that("summarize_stages() with different methods work", {
  data <- create_test_cpet_data()
  with_stages <- extract_stages(data, protocol = "step", stage_duration = 180)

  expect_no_error(summarize_stages(with_stages, method = "last30s"))
  expect_no_error(summarize_stages(with_stages, method = "last60s"))
  expect_no_error(summarize_stages(with_stages, method = "mean"))
  expect_no_error(summarize_stages(with_stages, method = "peak"))
})

test_that("summarize_stages() errors without stages", {
  data <- create_test_cpet_data()

  expect_error(
    summarize_stages(data, method = "last30s"),
    "No stages defined"
  )
})

test_that("detect_thresholds() returns Thresholds object", {
  data <- create_test_cpet_data()

  thresholds <- detect_thresholds(data, methods = c("v_slope", "ve_vo2", "ve_vco2"))

  expect_true(is_s7_class(thresholds, "Thresholds"))
  expect_true(!is.null(thresholds@confidence))
})

test_that("detect_thresholds() keeps VT2 above VT1 when both detected", {
  n <- 200
  power <- seq(0, 200, length.out = n)
  time_s <- seq(0, 600, length.out = n)

  vo2_ml <- 300 + 10 * power
  vco2_ml <- ifelse(
    power <= 120,
    250 + 9 * power,
    250 + 9 * 120 + 12 * (power - 120)
  )

  ve_vo2_ratio <- 30 - 5 * exp(-((power - 80)^2) / (2 * 15^2)) + 0.05 * pmax(power - 80, 0)
  ve_vco2_ratio <- 30 - 4 * exp(-((power - 120)^2) / (2 * 15^2)) + 0.06 * pmax(power - 120, 0)

  ve_l <- ve_vo2_ratio * vo2_ml / 1000

  breaths <- tibble::tibble(
    time_s = time_s,
    vo2_ml = vo2_ml,
    vco2_ml = vco2_ml,
    ve_l = ve_l,
    rer = vco2_ml / vo2_ml,
    power_w = power
  )

  participant <- Participant(
    id = "TEST002",
    name = "Threshold Test",
    age = 30,
    sex = "M",
    height_cm = 175,
    weight_kg = 70
  )

  metadata <- CpetMetadata(
    test_date = Sys.Date(),
    device = "Test Device",
    protocol = "Incremental ramp"
  )

  data <- CpetData(
    participant = participant,
    metadata = metadata,
    breaths = breaths,
    is_averaged = FALSE
  )

  thresholds <- detect_thresholds(
    data,
    methods = c("v_slope", "ve_vo2", "ve_vco2"),
    window_s = 30
  )

  expect_true(!is.na(thresholds@vt1_vo2))
  if (!is.na(thresholds@vt2_vo2)) {
    expect_gt(thresholds@vt2_vo2, thresholds@vt1_vo2)
  }
})


# Integration test ---------------------------------------------------------

test_that("Full analysis workflow works", {
  data <- create_test_cpet_data()

  # Validate
  validation <- validate(data)
  expect_true(validation@is_valid)

  # Average
  averaged <- average(data, method = "rolling", window = 30)
  expect_true(averaged@is_averaged)

  # Find peaks
  peaks <- find_peaks(averaged, averaging = 30)
  expect_true(is_s7_class(peaks, "PeakValues"))

  # Extract and summarize stages
  with_stages <- extract_stages(averaged, protocol = "step", stage_duration = 180)
  summary <- summarize_stages(with_stages, method = "last30s")
  expect_gt(nrow(summary), 0)

  # Create complete analysis
  analysis <- CpetAnalysis(
    data = averaged,
    peaks = peaks,
    validation = validation,
    stage_summary = summary
  )

  expect_true(is_s7_class(analysis, "CpetAnalysis"))
})

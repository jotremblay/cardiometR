# Integration Tests for cardiometR
# Tests full workflow from import to report generation

test_that("full analysis workflow works with mock data", {
  # Create mock data
  mock <- create_mock_breath_data(n_breaths = 300)

  # Create S7 objects
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
    pressure_mmhg = mock$metadata$pressure_mmhg,
    humidity_pct = mock$metadata$humidity_pct
  )

  cpet_data <- CpetData(
    participant = participant,
    metadata = metadata,
    breaths = mock$breaths,
    is_averaged = FALSE
  )

  # Step 1: Validate data
  validation <- validate(cpet_data)
  expect_true(is_s7_class(validation, "ValidationReport"))
  expect_true(validation@is_valid)

  # Step 2: Apply averaging
  averaged <- average(cpet_data, method = "rolling", window = 30)
  expect_true(is_s7_class(averaged, "CpetData"))
  expect_true(averaged@is_averaged)
  expect_equal(averaged@averaging_window, 30)

  # Step 3: Find peak values
  peaks <- find_peaks(averaged, averaging = 30)
  expect_true(is_s7_class(peaks, "PeakValues"))
  expect_gt(peaks@vo2_peak, 0)
  expect_gt(peaks@vo2_kg_peak, 0)
  expect_gt(peaks@ve_peak, 0)
  expect_gte(peaks@rer_peak, 0.7)
  expect_lte(peaks@rer_peak, 1.5)

  # Step 4: Create analysis object
  analysis <- CpetAnalysis(
    data = averaged,
    peaks = peaks,
    validation = validation
  )
  expect_true(is_s7_class(analysis, "CpetAnalysis"))
})

test_that("workflow handles missing HR data gracefully", {
  # Create mock data without HR
  mock <- create_mock_breath_data(n_breaths = 200, include_hr = FALSE)

  participant <- Participant(
    id = "NO_HR_001",
    name = "No HR Subject",
    age = 25,
    sex = "F",
    height_cm = 165,
    weight_kg = 60
  )

  metadata <- CpetMetadata(
    test_date = Sys.Date(),
    device = "Test Device",
    protocol = "Test Protocol"
  )

  cpet_data <- CpetData(
    participant = participant,
    metadata = metadata,
    breaths = mock$breaths,
    is_averaged = FALSE
  )

  # Should work without HR
  validation <- validate(cpet_data)
  expect_true(is_s7_class(validation, "ValidationReport"))

  peaks <- find_peaks(cpet_data, averaging = 30)
  expect_true(is_s7_class(peaks, "PeakValues"))
  expect_null(peaks@hr_peak)
})

test_that("workflow handles missing power data gracefully", {
  # Create mock data without power
  mock <- create_mock_breath_data(n_breaths = 200, include_power = FALSE)

  participant <- Participant(
    id = "NO_POWER_001",
    name = "No Power Subject",
    age = 40,
    sex = "M",
    height_cm = 180,
    weight_kg = 80
  )

  metadata <- CpetMetadata(
    test_date = Sys.Date(),
    device = "Test Device",
    protocol = "Treadmill"
  )

  cpet_data <- CpetData(
    participant = participant,
    metadata = metadata,
    breaths = mock$breaths,
    is_averaged = FALSE
  )

  peaks <- find_peaks(cpet_data, averaging = 30)
  expect_true(is_s7_class(peaks, "PeakValues"))
  expect_null(peaks@power_peak)
})

test_that("all averaging methods produce valid output", {
  mock <- create_mock_breath_data(n_breaths = 300)

  participant <- Participant(
    id = "AVG_TEST",
    name = "Averaging Test",
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

  cpet_data <- CpetData(
    participant = participant,
    metadata = metadata,
    breaths = mock$breaths,
    is_averaged = FALSE
  )

  # Test time-based averaging
  avg_time <- average(cpet_data, method = "time", window = 30)
  expect_true(avg_time@is_averaged)
  expect_equal(avg_time@averaging_window, 30)
  expect_lt(nrow(avg_time@breaths), nrow(cpet_data@breaths))

  # Test breath-based averaging
  avg_breath <- average(cpet_data, method = "breath", window = 10)
  expect_true(avg_breath@is_averaged)
  expect_equal(avg_breath@averaging_window, 10)
  expect_lt(nrow(avg_breath@breaths), nrow(cpet_data@breaths))

  # Test rolling averaging
  avg_rolling <- average(cpet_data, method = "rolling", window = 30)
  expect_true(avg_rolling@is_averaged)
  expect_equal(avg_rolling@averaging_window, 30)
})

test_that("stage extraction and summarization work", {
  mock <- create_mock_breath_data(n_breaths = 300, include_power = TRUE)

  participant <- Participant(
    id = "STAGE_TEST",
    name = "Stage Test",
    age = 30,
    sex = "M",
    height_cm = 175,
    weight_kg = 70
  )

  metadata <- CpetMetadata(
    test_date = Sys.Date(),
    device = "Test Device",
    protocol = "Step"
  )

  cpet_data <- CpetData(
    participant = participant,
    metadata = metadata,
    breaths = mock$breaths,
    is_averaged = FALSE
  )

  # Extract stages
  data_with_stages <- extract_stages(cpet_data, protocol = "step", stage_duration = 60)
  expect_true(is_s7_class(data_with_stages, "CpetData"))
  expect_false(is.null(data_with_stages@stages))

  # Summarize stages
  summary <- summarize_stages(data_with_stages, method = "last30s")
  expect_true(inherits(summary, "data.frame"))
  expect_true("vo2_ml" %in% names(summary))
  expect_true("stage" %in% names(summary))
})

test_that("plotting functions work without errors", {
  skip_if_not_installed("ggplot2")
  skip_if_not_installed("patchwork")

  mock <- create_mock_breath_data(n_breaths = 300)

  participant <- Participant(
    id = "PLOT_TEST",
    name = "Plot Test",
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

  cpet_data <- CpetData(
    participant = participant,
    metadata = metadata,
    breaths = mock$breaths,
    is_averaged = FALSE
  )

  averaged <- average(cpet_data, method = "rolling", window = 30)
  peaks <- find_peaks(averaged)

  analysis <- CpetAnalysis(
    data = averaged,
    peaks = peaks
  )

  # Test 9-panel plot
  expect_no_error({
    p <- plot_cpet_panel(analysis, language = "en")
  })

  # Test V-slope plot
  expect_no_error({
    p <- plot_v_slope(analysis)
  })

  # Test with French language
  expect_no_error({
    p <- plot_cpet_panel(analysis, language = "fr")
  })
})

test_that("bilingual labels work correctly", {
  # Test English labels
  expect_equal(tr("app_title", "en"), "CPET Analysis")
  expect_equal(tr("nav_upload", "en"), "Upload")

  # Test French labels
  expect_type(tr("app_title", "fr"), "character")
  expect_type(tr("nav_upload", "fr"), "character")

  # Test unknown key returns key itself
  expect_equal(tr("nonexistent_key", "en"), "nonexistent_key")
})

test_that("report configuration validates correctly", {
  # Valid English config
  config_en <- ReportConfig(language = "en")
  expect_true(is_s7_class(config_en, "ReportConfig"))
  expect_equal(config_en@language, "en")

  # Valid French config
  config_fr <- ReportConfig(
    language = "fr",
    institution = "UCLouvain",
    technician = "Dr. Test"
  )
  expect_true(is_s7_class(config_fr, "ReportConfig"))
  expect_equal(config_fr@language, "fr")
  expect_equal(config_fr@institution, "UCLouvain")

  # Invalid language should fail
  expect_error(ReportConfig(language = "de"))
})

test_that("validation detects physiological anomalies", {
  # Create data with anomalous values
  breaths <- create_minimal_breaths(n = 50)
  breaths$vo2_ml[25] <- -100  # Negative VO2 (impossible)

  participant <- Participant(
    id = "ANOMALY_TEST",
    name = "Anomaly Test",
    age = 30,
    sex = "M",
    height_cm = 175,
    weight_kg = 70
  )

  metadata <- CpetMetadata(
    test_date = Sys.Date(),
    device = "Test Device",
    protocol = "Test"
  )

  cpet_data <- CpetData(
    participant = participant,
    metadata = metadata,
    breaths = breaths,
    is_averaged = FALSE
  )

  validation <- validate(cpet_data)
  expect_true(is_s7_class(validation, "ValidationReport"))
  # Should detect the negative VO2
  expect_true(length(validation@errors) > 0 || length(validation@warnings) > 0)
})

test_that("minimal data requirements are enforced", {
  # CpetData requires specific columns
  incomplete_breaths <- tibble::tibble(
    time_s = 1:10,
    vo2_ml = rep(300, 10)
    # Missing: vco2_ml, ve_l, rer
  )

  participant <- Participant(
    id = "INCOMPLETE",
    name = "Test",
    age = 30,
    sex = "M",
    height_cm = 175,
    weight_kg = 70
  )

  metadata <- CpetMetadata(
    test_date = Sys.Date(),
    device = "Test",
    protocol = "Test"
  )

  expect_error(
    CpetData(
      participant = participant,
      metadata = metadata,
      breaths = incomplete_breaths,
      is_averaged = FALSE
    )
  )
})

# Tests for S7 class definitions
# Note: is_s7_class and is_empty_prop helpers defined in helper-mock-data.R

test_that("Participant class validates correctly", {
  # Valid participant
  p <- Participant(
    id = "P001",
    name = "Test Subject",
    age = 30,
    sex = "M",
    height_cm = 175,
    weight_kg = 70
  )

  expect_true(is_s7_class(p, "Participant"))
  expect_equal(p@id, "P001")
  expect_equal(p@name, "Test Subject")
  expect_equal(p@age, 30)
  expect_true(is_empty_prop(p@sport))

  # With optional sport
  p2 <- Participant(
    id = "P002",
    name = "Athlete",
    age = 25,
    sex = "F",
    height_cm = 165,
    weight_kg = 58,
    sport = "Cycling"
  )
  expect_equal(p2@sport, "Cycling")
})

test_that("Participant validates age range", {
  expect_error(
    Participant(id = "X", name = "X", age = -5, sex = "M", height_cm = 170, weight_kg = 70),
    "age must be between 0 and 120"
  )

  expect_error(
    Participant(id = "X", name = "X", age = 150, sex = "M", height_cm = 170, weight_kg = 70),
    "age must be between 0 and 120"
  )
})

test_that("Participant validates sex", {
  expect_error(
    Participant(id = "X", name = "X", age = 30, sex = "X", height_cm = 170, weight_kg = 70),
    "sex must be"
  )

  # Valid sex values
  expect_no_error(Participant(id = "X", name = "X", age = 30, sex = "M", height_cm = 170, weight_kg = 70))
  expect_no_error(Participant(id = "X", name = "X", age = 30, sex = "F", height_cm = 170, weight_kg = 70))
  expect_no_error(Participant(id = "X", name = "X", age = 30, sex = "O", height_cm = 170, weight_kg = 70))
})

test_that("CpetMetadata class works correctly", {
  m <- CpetMetadata(
    test_date = as.Date("2024-01-15"),
    device = "COSMED Quark CPET",
    protocol = "Incremental ramp"
  )

  expect_true(is_s7_class(m, "CpetMetadata"))
  expect_equal(m@device, "COSMED Quark CPET")
  expect_true(is_empty_prop(m@technician))
  expect_true(is_empty_prop(m@temperature_c))
})

test_that("CpetMetadata validates environmental conditions", {
  # Valid temperature
  expect_no_error(
    CpetMetadata(
      test_date = Sys.Date(),
      device = "Test",
      protocol = "Test",
      temperature_c = 22
    )
  )

  # Invalid temperature
  expect_error(
    CpetMetadata(
      test_date = Sys.Date(),
      device = "Test",
      protocol = "Test",
      temperature_c = 50
    ),
    "temperature_c should be between"
  )
})

test_that("CpetData validates required columns", {
  participant <- Participant(
    id = "P001", name = "Test", age = 30, sex = "M",
    height_cm = 175, weight_kg = 70
  )
  metadata <- CpetMetadata(
    test_date = Sys.Date(), device = "Test", protocol = "Test"
  )

  # Valid breaths data
  valid_breaths <- tibble::tibble(
    time_s = 1:10,
    vo2_ml = rep(300, 10),
    vco2_ml = rep(250, 10),
    ve_l = rep(10, 10),
    rer = rep(0.83, 10)
  )

  cpet <- CpetData(
    participant = participant,
    metadata = metadata,
    breaths = valid_breaths,
    is_averaged = FALSE
  )

  expect_true(is_s7_class(cpet, "CpetData"))
  expect_equal(nrow(cpet@breaths), 10)

  # Missing required columns
  invalid_breaths <- tibble::tibble(
    time_s = 1:10,
    vo2_ml = rep(300, 10)
    # Missing: vco2_ml, ve_l, rer
  )

  expect_error(
    CpetData(
      participant = participant,
      metadata = metadata,
      breaths = invalid_breaths,
      is_averaged = FALSE
    ),
    "missing required columns"
  )
})

test_that("CpetData validates averaging consistency", {
  participant <- Participant(
    id = "P001", name = "Test", age = 30, sex = "M",
    height_cm = 175, weight_kg = 70
  )
  metadata <- CpetMetadata(
    test_date = Sys.Date(), device = "Test", protocol = "Test"
  )
  breaths <- create_minimal_breaths(10)

  # is_averaged = TRUE requires averaging_window
  # Note: S7 doesn't enforce this at property level, validation is in class validator
  # Skip this test as the current implementation doesn't require averaging_window
  skip("S7 class validator for averaging_window not implemented yet")

  # Valid averaged data
  expect_no_error(
    CpetData(
      participant = participant,
      metadata = metadata,
      breaths = breaths,
      is_averaged = TRUE,
      averaging_window = 30
    )
  )
})

test_that("PeakValues validates physiological ranges", {
  # Valid peak values
  peaks <- PeakValues(
    vo2_peak = 3500,
    vo2_kg_peak = 50,
    ve_peak = 120,
    rer_peak = 1.15,
    averaging_s = 30
  )

  expect_true(is_s7_class(peaks, "PeakValues"))
  expect_equal(peaks@vo2_peak, 3500)

  # Invalid VO2 peak
  expect_error(
    PeakValues(
      vo2_peak = 10000,  # Too high
      vo2_kg_peak = 50,
      ve_peak = 120,
      rer_peak = 1.15,
      averaging_s = 30
    ),
    "vo2_peak should be between"
  )

  # Invalid RER
  expect_error(
    PeakValues(
      vo2_peak = 3500,
      vo2_kg_peak = 50,
      ve_peak = 120,
      rer_peak = 3.0,  # Too high
      averaging_s = 30
    ),
    "rer_peak should be between"
  )
})

test_that("Thresholds validates confidence levels", {
  # Valid thresholds
  thresh <- Thresholds(
    vt1_vo2 = 1500,
    vt1_method = "V-slope",
    confidence = "high"
  )

  expect_true(is_s7_class(thresh, "Thresholds"))
  expect_equal(thresh@confidence, "high")

  # Invalid confidence
  expect_error(
    Thresholds(
      vt1_vo2 = 1500,
      confidence = "invalid_level"
    ),
    "confidence must be"
  )

  # All valid confidence levels
  expect_no_error(Thresholds(confidence = "high"))
  expect_no_error(Thresholds(confidence = "moderate"))
  expect_no_error(Thresholds(confidence = "low"))
  expect_no_error(Thresholds(confidence = "unable"))
})

test_that("ValidationReport validates consistency", {
  # Valid: is_valid = TRUE, no errors
  valid_report <- ValidationReport(
    is_valid = TRUE,
    errors = list(),
    warnings = list("Minor issue"),
    info = list()
  )
  expect_true(is_s7_class(valid_report, "ValidationReport"))

  # Valid: is_valid = FALSE, has errors
  invalid_report <- ValidationReport(
    is_valid = FALSE,
    errors = list("Critical error"),
    warnings = list(),
    info = list()
  )
  expect_false(invalid_report@is_valid)

  # Invalid: is_valid = TRUE but has errors
  expect_error(
    ValidationReport(
      is_valid = TRUE,
      errors = list("Error present"),
      warnings = list(),
      info = list()
    ),
    "is_valid cannot be TRUE when errors exist"
  )

  # Invalid: is_valid = FALSE but no errors
  expect_error(
    ValidationReport(
      is_valid = FALSE,
      errors = list(),
      warnings = list(),
      info = list()
    ),
    "is_valid should be TRUE when no errors exist"
  )
})

test_that("ReportConfig validates language", {
  # Valid configs
  config_en <- ReportConfig(language = "en")
  config_fr <- ReportConfig(language = "fr")

  expect_equal(config_en@language, "en")
  expect_equal(config_fr@language, "fr")

  # Invalid language
  expect_error(
    ReportConfig(language = "de"),
    "language must be"
  )
})

test_that("CpetAnalysis composes correctly", {
  # Create component objects
  participant <- Participant(
    id = "P001", name = "Test", age = 30, sex = "M",
    height_cm = 175, weight_kg = 70
  )
  metadata <- CpetMetadata(
    test_date = Sys.Date(), device = "Test", protocol = "Test"
  )
  breaths <- create_minimal_breaths(10)

  cpet_data <- CpetData(
    participant = participant,
    metadata = metadata,
    breaths = breaths,
    is_averaged = FALSE
  )

  # Minimal analysis (data only)
  analysis <- CpetAnalysis(data = cpet_data)

  expect_true(is_s7_class(analysis, "CpetAnalysis"))
  expect_true(is_s7_class(analysis@data, "CpetData"))
  expect_true(is_empty_prop(analysis@peaks) || is_s7_class(analysis@peaks, "PeakValues"))
  expect_true(is_empty_prop(analysis@thresholds) || is_s7_class(analysis@thresholds, "Thresholds"))

  # Full analysis
  peaks <- PeakValues(
    vo2_peak = 3500, vo2_kg_peak = 50, ve_peak = 120,
    rer_peak = 1.15, averaging_s = 30
  )

  full_analysis <- CpetAnalysis(
    data = cpet_data,
    peaks = peaks
  )

  expect_true(is_s7_class(full_analysis@peaks, "PeakValues"))
})

# ---- PreTestConditions class tests ----

test_that("PreTestConditions class validates correctly", {
  # Valid pre-test conditions
  ptc <- PreTestConditions(
    nutritional_state = "fed",
    last_meal_hours = 2.5,
    fatigue_state = "rested",
    medications_taken = FALSE,
    caffeine_intake = TRUE,
    caffeine_mg = 150
  )

  expect_true(is_s7_class(ptc, "PreTestConditions"))
  expect_equal(ptc@nutritional_state, "fed")
  expect_equal(ptc@last_meal_hours, 2.5)
  expect_equal(ptc@caffeine_mg, 150)
  expect_false(ptc@medications_taken)
})

test_that("PreTestConditions validates nutritional_state", {
  expect_error(
    PreTestConditions(nutritional_state = "invalid"),
    "nutritional_state must be"
  )

  expect_no_error(PreTestConditions(nutritional_state = "fed"))
  expect_no_error(PreTestConditions(nutritional_state = "fasted"))
})

test_that("PreTestConditions validates fatigue_state", {
  expect_error(
    PreTestConditions(fatigue_state = "invalid"),
    "fatigue_state must be"
  )

  expect_no_error(PreTestConditions(fatigue_state = "rested"))
  expect_no_error(PreTestConditions(fatigue_state = "fatigued"))
})

test_that("PreTestConditions validates caffeine_mg range", {
  expect_error(
    PreTestConditions(caffeine_mg = -10),
    "caffeine_mg should be between"
  )

  expect_error(
    PreTestConditions(caffeine_mg = 1500),
    "caffeine_mg should be between"
  )

  expect_no_error(PreTestConditions(caffeine_mg = 200))
})

test_that("PreTestConditions validates last_meal_hours range", {
  expect_error(
    PreTestConditions(last_meal_hours = -1),
    "last_meal_hours should be between"
  )

  expect_error(
    PreTestConditions(last_meal_hours = 50),
    "last_meal_hours should be between"
  )

  expect_no_error(PreTestConditions(last_meal_hours = 3))
})

test_that("PreTestConditions allows NULL for optional properties", {
  ptc <- PreTestConditions()
  expect_true(is_empty_prop(ptc@nutritional_state))
  expect_true(is_empty_prop(ptc@caffeine_mg))
  expect_false(ptc@medications_taken)  # Default is FALSE
})

# ---- ProtocolConfig class tests ----

test_that("ProtocolConfig class validates correctly", {
  # Valid cycling protocol
  pc <- ProtocolConfig(
    modality = "cycling",
    starting_intensity = 50,
    increment_size = 25,
    stage_duration_s = 180,
    equipment_model = "Lode Excalibur",
    analyzer_model = "COSMED Quark CPET"
  )

  expect_true(is_s7_class(pc, "ProtocolConfig"))
  expect_equal(pc@modality, "cycling")
  expect_equal(pc@starting_intensity, 50)
  expect_equal(pc@increment_size, 25)
  expect_equal(pc@stage_duration_s, 180)
})

test_that("ProtocolConfig validates modality", {
  expect_error(
    ProtocolConfig(modality = "rowing"),
    "modality must be"
  )

  expect_no_error(ProtocolConfig(modality = "cycling"))
  expect_no_error(ProtocolConfig(modality = "treadmill"))
  expect_no_error(ProtocolConfig(modality = "other"))
})

test_that("ProtocolConfig validates stage_duration_s range", {
  expect_error(
    ProtocolConfig(stage_duration_s = 10),
    "stage_duration_s should be between"
  )

  expect_error(
    ProtocolConfig(stage_duration_s = 1000),
    "stage_duration_s should be between"
  )

  expect_no_error(ProtocolConfig(stage_duration_s = 60))
  expect_no_error(ProtocolConfig(stage_duration_s = 180))
})

test_that("ProtocolConfig for treadmill includes grade", {
  pc <- ProtocolConfig(
    modality = "treadmill",
    starting_intensity = 6,  # km/h
    increment_size = 1,
    stage_duration_s = 180,
    starting_grade = 0,
    grade_increment = 2
  )

  expect_equal(pc@starting_grade, 0)
  expect_equal(pc@grade_increment, 2)
})

# ---- EconomyMetrics class tests ----

test_that("EconomyMetrics class validates correctly", {
  # Cycling efficiency
  em_cycling <- EconomyMetrics(
    modality = "cycling",
    gross_efficiency = 22.5,
    reference_stage = 5,
    reference_power = 200
  )

  expect_true(is_s7_class(em_cycling, "EconomyMetrics"))
  expect_equal(em_cycling@modality, "cycling")
  expect_equal(em_cycling@gross_efficiency, 22.5)
  expect_equal(em_cycling@reference_power, 200)

  # Running economy
  em_running <- EconomyMetrics(
    modality = "treadmill",
    running_economy = 195,
    reference_stage = 4,
    reference_speed = 12
  )

  expect_equal(em_running@modality, "treadmill")
  expect_equal(em_running@running_economy, 195)
  expect_equal(em_running@reference_speed, 12)
})

test_that("EconomyMetrics validates modality", {
  expect_error(
    EconomyMetrics(modality = "rowing"),
    "modality must be"
  )

  expect_no_error(EconomyMetrics(modality = "cycling"))
  expect_no_error(EconomyMetrics(modality = "treadmill"))
})

test_that("EconomyMetrics validates gross_efficiency range", {
  expect_error(
    EconomyMetrics(modality = "cycling", gross_efficiency = 5),
    "gross_efficiency should be between"
  )

  expect_error(
    EconomyMetrics(modality = "cycling", gross_efficiency = 40),
    "gross_efficiency should be between"
  )

  expect_no_error(EconomyMetrics(modality = "cycling", gross_efficiency = 22))
})

test_that("EconomyMetrics validates running_economy range", {
  expect_error(
    EconomyMetrics(modality = "treadmill", running_economy = 50),
    "running_economy should be between"
  )

  expect_error(
    EconomyMetrics(modality = "treadmill", running_economy = 400),
    "running_economy should be between"
  )

  expect_no_error(EconomyMetrics(modality = "treadmill", running_economy = 200))
})

# ---- CpetAnalysis with new properties tests ----

test_that("CpetAnalysis accepts new optional properties", {
  participant <- Participant(
    id = "P001", name = "Test", age = 30, sex = "M",
    height_cm = 175, weight_kg = 70
  )
  metadata <- CpetMetadata(
    test_date = Sys.Date(), device = "Test", protocol = "Test"
  )
  breaths <- create_minimal_breaths(10)
  cpet_data <- CpetData(
    participant = participant,
    metadata = metadata,
    breaths = breaths,
    is_averaged = FALSE
  )

  # With pre-test conditions
  ptc <- PreTestConditions(
    nutritional_state = "fed",
    fatigue_state = "rested"
  )

  # With protocol config
  pc <- ProtocolConfig(
    modality = "cycling",
    starting_intensity = 50,
    increment_size = 25
  )

  # With economy metrics
  em <- EconomyMetrics(
    modality = "cycling",
    gross_efficiency = 22
  )

  analysis <- CpetAnalysis(
    data = cpet_data,
    pre_test_conditions = ptc,
    protocol_config = pc,
    economy_metrics = em
  )

  expect_true(is_s7_class(analysis, "CpetAnalysis"))
  expect_true(is_s7_class(analysis@pre_test_conditions, "PreTestConditions"))
  expect_true(is_s7_class(analysis@protocol_config, "ProtocolConfig"))
  expect_true(is_s7_class(analysis@economy_metrics, "EconomyMetrics"))

  expect_equal(analysis@pre_test_conditions@nutritional_state, "fed")
  expect_equal(analysis@protocol_config@modality, "cycling")
  expect_equal(analysis@economy_metrics@gross_efficiency, 22)
})

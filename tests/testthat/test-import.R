# Tests for COSMED import functions
# Note: is_s7_class helper defined in helper-mock-data.R

test_that("read_cosmed imports valid xlsx file", {
  skip_if_not_installed("readxl")

  # Use example file from inst/extdata
  example_file <- system.file("extdata", "example_cosmed.xlsx", package = "cardiometR")

  # Skip if example file not found (package not installed)
  skip_if(example_file == "", "Example file not found")

  data <- read_cosmed(example_file)

  # Check class
  expect_true(is_s7_class(data, "CpetData"))

  # Check participant
  expect_true(is_s7_class(data@participant, "Participant"))
  expect_type(data@participant@id, "character")
  expect_type(data@participant@name, "character")
  expect_true(data@participant@sex %in% c("M", "F", "O"))

  # Check metadata
  expect_true(is_s7_class(data@metadata, "CpetMetadata"))
  expect_s3_class(data@metadata@test_date, "Date")

  # Check breaths
  expect_s3_class(data@breaths, "data.frame")
  expect_true(nrow(data@breaths) > 0)

  # Check required columns exist
  required_cols <- c("time_s", "vo2_ml", "vco2_ml", "ve_l", "rer")
  expect_true(all(required_cols %in% names(data@breaths)))

  # Example data has regular 10s intervals → detected as time-averaged
  expect_true(data@is_averaged)
  expect_equal(data@averaging_window, 10)
})


test_that("read_cosmed parses participant correctly", {
  skip_if_not_installed("readxl")

  example_file <- system.file("extdata", "example_cosmed.xlsx", package = "cardiometR")
  skip_if(example_file == "", "Example file not found")

  data <- read_cosmed(example_file)

  # Check participant fields are populated
  expect_true(nchar(data@participant@id) > 0)
  expect_true(nchar(data@participant@name) > 0)
  expect_true(data@participant@age > 0 && data@participant@age < 120)
  expect_true(data@participant@height_cm > 50 && data@participant@height_cm < 250)
  expect_true(data@participant@weight_kg > 10 && data@participant@weight_kg < 300)
})


test_that("read_cosmed parses metadata correctly", {
  skip_if_not_installed("readxl")

  example_file <- system.file("extdata", "example_cosmed.xlsx", package = "cardiometR")
  skip_if(example_file == "", "Example file not found")

  data <- read_cosmed(example_file)

  # Check metadata fields
  expect_true(nchar(data@metadata@device) > 0)
  expect_true(nchar(data@metadata@protocol) > 0)

  # Environmental conditions should be reasonable if present
  if (!is.null(data@metadata@temperature_c)) {
    expect_true(data@metadata@temperature_c > 10 && data@metadata@temperature_c < 40)
  }
  if (!is.null(data@metadata@pressure_mmhg)) {
    expect_true(data@metadata@pressure_mmhg > 600 && data@metadata@pressure_mmhg < 900)
  }
})


test_that("read_cosmed converts time correctly", {
  skip_if_not_installed("readxl")

  example_file <- system.file("extdata", "example_cosmed.xlsx", package = "cardiometR")
  skip_if(example_file == "", "Example file not found")

  data <- read_cosmed(example_file)

  # Time should be in seconds, not Excel day fractions
  # Typical CPET test is 8-30 minutes = 480-1800 seconds
  expect_true(max(data@breaths$time_s) > 60)  # At least 1 minute
  expect_true(max(data@breaths$time_s) < 7200)  # Less than 2 hours

  # Time should be monotonically increasing
  time_diffs <- diff(data@breaths$time_s)
  expect_true(all(time_diffs >= 0))
})


test_that("read_cosmed parses physiological data correctly", {
  skip_if_not_installed("readxl")

  example_file <- system.file("extdata", "example_cosmed.xlsx", package = "cardiometR")
  skip_if(example_file == "", "Example file not found")

  data <- read_cosmed(example_file)

  # VO2 should be in mL/min range
  expect_true(all(data@breaths$vo2_ml > 0))
  expect_true(all(data@breaths$vo2_ml < 8000))

  # VCO2 should be in similar range
  expect_true(all(data@breaths$vco2_ml > 0))
  expect_true(all(data@breaths$vco2_ml < 8000))

  # RER should be physiological
  expect_true(all(data@breaths$rer > 0.5))
  expect_true(all(data@breaths$rer < 2.0))

  # VE should be positive
  expect_true(all(data@breaths$ve_l > 0))
})


test_that("read_cosmed handles phase column", {
  skip_if_not_installed("readxl")

  example_file <- system.file("extdata", "example_cosmed.xlsx", package = "cardiometR")
  skip_if(example_file == "", "Example file not found")

  data <- read_cosmed(example_file)

  # Phase should be character if present
  if ("phase" %in% names(data@breaths)) {
    expect_type(data@breaths$phase, "character")
  }
})


test_that("read_cosmed errors on missing file", {
  expect_error(
    read_cosmed("nonexistent_file.xlsx"),
    "File not found"
  )
})


test_that("read_cpet auto-detects COSMED format", {
  skip_if_not_installed("readxl")

  example_file <- system.file("extdata", "example_cosmed.xlsx", package = "cardiometR")
  skip_if(example_file == "", "Example file not found")

  # Should auto-detect and use read_cosmed
  data <- read_cpet(example_file)

  expect_true(is_s7_class(data, "CpetData"))
})


test_that("read_cpet with explicit format works", {
  skip_if_not_installed("readxl")

  example_file <- system.file("extdata", "example_cosmed.xlsx", package = "cardiometR")
  skip_if(example_file == "", "Example file not found")

  # Explicit format
  data <- read_cpet(example_file, format = "cosmed")

  expect_true(is_s7_class(data, "CpetData"))
})

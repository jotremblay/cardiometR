---
description: Generate testthat tests for cardiometR CPET functions
argument-hint: [function-name]
allowed-tools: Read, Write, Glob
---

# CPET Test Generator

Generate testthat tests for a cardiometR function with CPET-specific test cases.

## Function to Test
- **Name**: $1
- **Context**: $ARGUMENTS

## Instructions

1. Read the function implementation to understand its signature and behavior
2. Read existing tests in `tests/testthat/` for patterns
3. Create `tests/testthat/test-$1.R`

## Test File Template

```r
# tests/testthat/test-$1.R

test_that("$1 returns expected type", {
  # Setup: Create mock CPET data
  mock_data <- create_mock_cpet_data()

  # Execute
 result <- $1(mock_data)

  # Assert
  expect_s3_class(result, "expected_class")
  # Or for S7:
  # expect_true(inherits(result, "S7_class_name"))
})

test_that("$1 handles edge cases", {
  # Test with minimal data
  minimal_data <- create_mock_cpet_data(n_breaths = 10)
  expect_no_error($1(minimal_data))

  # Test with missing optional fields
  no_hr_data <- create_mock_cpet_data(include_hr = FALSE)
  expect_no_error($1(no_hr_data))
})

test_that("$1 validates input", {
  # Test with invalid input
  expect_error($1(NULL), "expected error message")
  expect_error($1(data.frame()), "expected error message")
})

test_that("$1 produces physiologically valid results", {
  mock_data <- create_mock_cpet_data()
  result <- $1(mock_data)

  # Check physiological ranges
  # VO2 peak: typically 1000-6000 mL/min
  # HR peak: typically 60-220 bpm
  # RER max: typically 0.7-1.3
})
```

## Mock Data Helper

Create or use `tests/testthat/helper-mock-data.R`:

```r
#' Create mock CPET breath-by-breath data
#'
#' @param n_breaths Number of breaths to generate
#' @param include_hr Include heart rate data
#' @param include_power Include power/workload data
#' @return A CpetData S7 object
create_mock_cpet_data <- function(n_breaths = 300,
                                   include_hr = TRUE,
                                   include_power = TRUE) {
  # Generate realistic breath-by-breath data
  time_s <- seq(0, by = 2, length.out = n_breaths)

  # Simulate incremental exercise
  intensity <- pmin(time_s / max(time_s), 1)

  breaths <- tibble::tibble(
    time_s = time_s,
    vo2_ml = 300 + intensity * 2500 + rnorm(n_breaths, 0, 50),
    vco2_ml = 250 + intensity * 2800 + rnorm(n_breaths, 0, 50),
    ve_l = 10 + intensity * 120 + rnorm(n_breaths, 0, 3),
    rer = vco2_ml / vo2_ml,
    bf = 12 + intensity * 40 + rnorm(n_breaths, 0, 2),
    vt_l = ve_l / bf
  )

  if (include_hr) {
    breaths$hr_bpm <- 70 + intensity * 120 + rnorm(n_breaths, 0, 3)
  }

  if (include_power) {
    breaths$power_w <- intensity * 300
  }

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
    device = "Mock COSMED",
    protocol = "Incremental"
  )

  CpetData(
    participant = participant,
    metadata = metadata,
    breaths = breaths,
    is_averaged = FALSE
  )
}
```

## Physiological Reference Values

Use these for validation tests:
- Resting VO2: 150-400 mL/min (3.5 mL/kg/min × weight)
- Resting RER: 0.70-0.90
- Max HR: ~220 - age (± 10-15 bpm)
- Peak RER: > 1.10 indicates maximal effort
- VE/VO2 at VT1: typically 20-25
- VE/VCO2 at VT2: typically 30-35

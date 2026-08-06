# Tests for resting values + rest-exclusion in derived metrics

test_that("resting values are computed from a mock with leading rest", {
  a <- create_mock_cpet_analysis(with_rest = TRUE)
  expect_true(has_resting(a))

  r <- a@resting
  expect_gt(r$duration_s, 160)
  expect_lt(r$duration_s, 200)
  expect_gt(r$vo2_rest, 300)
  expect_lt(r$vo2_rest, 420)
  expect_gt(r$hr_rest, 60)
  expect_lt(r$hr_rest, 80)
  expect_length(a@vt1_range, 2)
})

test_that("resting values come from the rest phase, not the warmup", {
  example_file <- system.file("extdata", "example_cosmed.xlsx",
                              package = "cardiometR")
  skip_if(example_file == "", "Example COSMED file not found")

  data <- extract_stages(read_cpet(example_file, quiet = TRUE))
  resting <- compute_resting_values(data)
  weight_kg <- data@participant@weight_kg

  # extract_stages() gives stage 0 to rest, warmup AND recovery alike, so
  # averaging the last minute of the leading stage-0 block landed at the end
  # of the warmup. That reported 2672 mL/min and 124 bpm on this file, four
  # times the true resting value, and it was wrong in every language.
  rest_rows <- data@breaths[!is.na(data@breaths$phase) &
                              data@breaths$phase == "rest", ]
  expect_lt(abs(resting$vo2_rest - mean(rest_rows$vo2_ml)),
            0.15 * mean(rest_rows$vo2_ml))

  # Seated on the ergometer before a test, not supine and fasted, so this
  # runs above the 3.5 mL/kg/min of a true basal measurement. Two to three
  # METs is the plausible band; nine is exercise.
  vo2_kg_rest <- resting$vo2_rest / weight_kg
  expect_gt(vo2_kg_rest, 3)
  expect_lt(vo2_kg_rest, 12)

  expect_lt(resting$hr_rest, 110)
  expect_lt(resting$rer_rest, 1)

  # The reported duration must describe the rest block, not everything that
  # happened before exercise.
  expect_lt(resting$duration_s, 200)
})

test_that("resting falls back to stage 0 when there is no phase column", {
  example_file <- system.file("extdata", "example_cosmed.xlsx",
                              package = "cardiometR")
  skip_if(example_file == "", "Example COSMED file not found")

  data <- extract_stages(read_cpet(example_file, quiet = TRUE))
  data@breaths$phase <- NULL

  # Carts that record no phase still get an answer, just a rougher one.
  resting <- compute_resting_values(data)
  expect_false(is.null(resting))
  expect_true(is.finite(resting$vo2_rest))
})

test_that("no rest block leaves resting NULL and thresholds intact", {
  a <- create_mock_cpet_analysis(with_rest = FALSE)
  expect_false(has_resting(a))
  expect_null(a@resting)
  expect_length(a@vt1_range, 2)
})

test_that("rest does not drag down the VO2-Power slope", {
  a_rest    <- create_mock_cpet_analysis(with_rest = TRUE)
  a_no_rest <- create_mock_cpet_analysis(with_rest = FALSE)

  s_rest    <- a_rest@vo2_power_slope$slope
  s_no_rest <- a_no_rest@vo2_power_slope$slope

  expect_true(is.finite(s_rest) && is.finite(s_no_rest))
  expect_lt(abs(s_rest - s_no_rest) / s_no_rest, 0.10)
})

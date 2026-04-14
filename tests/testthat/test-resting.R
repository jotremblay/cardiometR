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

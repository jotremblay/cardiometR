# Tests for z_score() / percentile_from_z()

test_that("estimated SD path: (high - low) / 3.29", {
  stratum <- list(
    vo2max_typical = 50,
    vo2max_low     = 40,
    vo2max_high    = 60
  )

  res_zero <- z_score(50, stratum, metric = "vo2max")
  expect_equal(res_zero$z, 0, tolerance = 1e-8)
  expect_equal(res_zero$percentile, 50, tolerance = 1e-6)
  expect_equal(res_zero$sd_source, "estimated")

  expected_sd <- (60 - 40) / 3.29
  expect_equal(expected_sd, 6.079, tolerance = 1e-3)

  res_high <- z_score(62, stratum, metric = "vo2max")
  expect_gt(res_high$z, 0)
  expect_gt(res_high$percentile, 80)
  expect_equal(res_high$sd_source, "estimated")
})

test_that("tabulated SD path is preferred when available", {
  stratum <- list(
    vo2max_typical = 50,
    vo2max_low     = 40,
    vo2max_high    = 60,
    vo2max_sd      = 5
  )
  res <- z_score(55, stratum, metric = "vo2max")
  expect_equal(res$sd_source, "tabulated")
  expect_equal(res$z, 1, tolerance = 1e-8)
})

test_that("percentile_from_z is monotone and bounded", {
  expect_equal(percentile_from_z(0), 50, tolerance = 1e-6)
  expect_gt(percentile_from_z(1), percentile_from_z(0))
  expect_lt(percentile_from_z(-1), percentile_from_z(0))
})

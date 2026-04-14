# Tests for Kuipers MAP/PPO and VO2-Power slope

test_that("compute_map_kuipers handles partial last stage (50% of 100W increment)", {
  ss <- tibble::tibble(
    stage      = 1:4,
    duration_s = c(180, 180, 180, 90),  # last stage half-complete vs typical 180s
    power_w    = c(100, 200, 300, 400)
  )
  res <- compute_map_kuipers(ss)

  expect_equal(res$map_watts, 300)
  expect_equal(res$ppo_watts, 350)
  expect_equal(res$kuipers_fraction, 0.5)
})

test_that("compute_map_kuipers: complete last stage -> map == ppo == last power", {
  ss <- tibble::tibble(
    stage      = 1:3,
    duration_s = c(180, 180, 180),
    power_w    = c(100, 200, 300)
  )
  res <- compute_map_kuipers(ss)

  expect_equal(res$map_watts, 300)
  expect_equal(res$ppo_watts, 300)
  expect_equal(res$map_watts, res$ppo_watts)
  expect_gte(res$kuipers_fraction, 0.98)
})

test_that("fit_vo2_power_slope recovers slope of 11 mL/min/W", {
  set.seed(123)
  power <- rep(seq(50, 250, by = 25), each = 8)
  n <- length(power)
  vo2 <- 200 + 11 * power + stats::rnorm(n, 0, 10)
  bdf <- tibble::tibble(power_w = power, vo2_ml = vo2)

  ss <- tibble::tibble(stage = 1:5, duration_s = rep(180, 5),
                      power_w = c(50, 100, 150, 200, 250))

  fit <- fit_vo2_power_slope(bdf, ss, vt2_power = 250)

  expect_true(fit$slope_ci_low <= 11 && fit$slope_ci_high >= 11)
  expect_gt(fit$r_squared, 0.95)
})

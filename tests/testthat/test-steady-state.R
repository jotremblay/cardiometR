# Tests for per-stage steady-state gating (Achten & Jeukendrup 2004)

test_that("steady-state TRUE for 4-min stage with low HR/RER drift", {
  # Stage 1: 4 min (240s), HR drift ~1 bpm, RER drift ~0.01, RER stays < 1
  set.seed(7)
  t1 <- seq(0, 240, by = 2)
  n1 <- length(t1)
  s1 <- tibble::tibble(
    time_s = t1,
    stage  = 1L,
    hr_bpm = 130 + (t1 / 240) * 1,             # +1 bpm linear drift
    rer    = 0.85 + (t1 / 240) * 0.01          # +0.01 RER drift, max < 1.0
  )

  ss <- tibble::tibble(stage = 1L, duration_s = 240, power_w = 150)
  res <- check_steady_state(s1, ss)

  expect_equal(nrow(res), 1L)
  expect_true(res$steady_state_ok[1])
  expect_true(res$duration_ok[1])
  expect_true(res$rer_sub_unity[1])
})

test_that("steady-state FALSE for too-short (2 min) stage", {
  t2 <- seq(0, 120, by = 2)
  s2 <- tibble::tibble(
    time_s = t2,
    stage  = 2L,
    hr_bpm = rep(140, length(t2)),
    rer    = rep(0.90, length(t2))
  )
  ss <- tibble::tibble(stage = 2L, duration_s = 120, power_w = 200)
  res <- check_steady_state(s2, ss)

  expect_equal(nrow(res), 1L)
  expect_false(res$duration_ok[1])
  expect_false(res$steady_state_ok[1])
})

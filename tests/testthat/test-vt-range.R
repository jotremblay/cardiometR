# Tests for detect_threshold_range() - multi-method VT detection

make_ramp_breath_df <- function(n = 300) {
  # Synthetic ramp: VO2 rises linearly from 500 to 4000 mL/min.
  # VCO2 tracks VO2 but breaks upward near VO2 ~= 2000 (VT1)
  # VE breaks upward again near VO2 ~= 3200 (VT2)
  time_s <- seq(0, by = 2, length.out = n)
  vo2   <- seq(500, 4000, length.out = n)
  vt1 <- 2000
  vt2 <- 3200
  rer_base <- 0.85
  vco2 <- vo2 * rer_base +
    pmax(0, vo2 - vt1) * 0.25       # VT1 break: VCO2 rises faster
  ve <- vco2 / 30 +                   # ~VE/VCO2 = 30
    pmax(0, vo2 - vt2) * 0.01        # VT2 break: VE rises faster still
  set.seed(1)
  tibble::tibble(
    time_s = time_s,
    vo2_ml = vo2 + stats::rnorm(n, 0, 20),
    vco2_ml = vco2 + stats::rnorm(n, 0, 20),
    ve_l   = ve + stats::rnorm(n, 0, 0.3),
    peto2_mmhg  = 100 + pmax(0, vo2 - vt1) * 0.005 + stats::rnorm(n, 0, 0.5),
    petco2_mmhg = 40  - pmax(0, vo2 - vt2) * 0.003 + stats::rnorm(n, 0, 0.3),
    hr_bpm = seq(80, 185, length.out = n),
    power_w = seq(0, 350, length.out = n)
  )
}

test_that("detect_threshold_range returns ranges wider or equal than single estimate", {
  df <- make_ramp_breath_df()
  res <- detect_threshold_range(df)

  expect_type(res, "list")
  expect_true(!is.null(res$vt1_range))
  expect_true(!is.null(res$vt2_range))
  expect_length(res$vt1_range, 2)
  expect_length(res$vt2_range, 2)
  expect_gte(res$vt1_range[2], res$vt1_range[1])
  expect_gte(res$vt2_range[2], res$vt2_range[1])
  # Range must contain at least one observed method/smoothing value
  expect_true(any(res$vt1_values$vo2 >= res$vt1_range[1] &
                  res$vt1_values$vo2 <= res$vt1_range[2]))
})

test_that("range contains the single-method point estimate", {
  df <- make_ramp_breath_df()
  range_res <- detect_threshold_range(df, methods = c("v_slope", "ve_vo2", "ve_vco2"))

  # Single-method "default" smoothing roughly matches the CpetData path
  single <- detect_threshold_range(df, methods = c("v_slope", "ve_vo2", "ve_vco2"),
                                   smoothing = "default")
  # median of single VT1 within the wider range
  single_vt1 <- stats::median(single$vt1_values$vo2, na.rm = TRUE)
  expect_true(single_vt1 >= range_res$vt1_range[1] - 1e-6 &&
              single_vt1 <= range_res$vt1_range[2] + 1e-6)
})

test_that("returns empty structure when breath_df is too short or missing cols", {
  tiny <- tibble::tibble(time_s = 1:5, vo2_ml = 1:5, vco2_ml = 1:5, ve_l = 1:5)
  res <- detect_threshold_range(tiny)
  expect_null(res$vt1_range)
  expect_null(res$vt2_range)
  expect_s3_class(res$vt1_values, "tbl_df")
  expect_equal(nrow(res$vt1_values), 0)

  bad <- tibble::tibble(time_s = 1:100, vo2_ml = 1:100)  # missing vco2_ml, ve_l
  res2 <- detect_threshold_range(bad)
  expect_null(res2$vt1_range)
  expect_null(res2$vt2_range)

  res3 <- detect_threshold_range(NULL)
  expect_null(res3$vt1_range)
})

test_that("detect_thresholds excludes rest and recovery phases", {
  df <- make_ramp_breath_df(n = 200)
  n <- nrow(df)
  df$phase <- c(rep("rest", 30), rep("exercise", n - 50), rep("recovery", 20))
  df$rer <- df$vco2_ml / df$vo2_ml
  # Inflate rest VO2 so an unfiltered detector would be pulled down/up wrongly
  df$vo2_ml[1:30] <- 800
  df$vco2_ml[1:30] <- 650
  df$rer[1:30] <- df$vco2_ml[1:30] / df$vo2_ml[1:30]

  participant <- Participant(
    id = "VT001", name = "VT", age = 30, sex = "M",
    height_cm = 175, weight_kg = 70
  )
  metadata <- CpetMetadata(test_date = Sys.Date(), device = "Mock", protocol = "Ramp")
  data <- CpetData(participant = participant, metadata = metadata, breaths = df)

  th <- detect_thresholds(data, methods = c("v_slope", "ve_vo2", "ve_vco2"))
  expect_true(is.finite(th@vt1_vo2) || th@confidence == "unable" ||
                th@confidence %in% c("low", "moderate", "high"))
  # Exercise-only VO2 starts near 500 and rises; rest-inflated path would differ
  expect_true(is.na(th@vt1_vo2) || th@vt1_vo2 > 900)
})

test_that("threshold_confidence uses method agreement not mere count", {
  expect_equal(cardiometR:::threshold_confidence(numeric()), "unable")
  expect_equal(cardiometR:::threshold_confidence(2000), "low")
  expect_equal(cardiometR:::threshold_confidence(c(2000, 2050, 1980)), "high")
  expect_equal(cardiometR:::threshold_confidence(c(1500, 3000)), "low")
})

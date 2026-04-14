# Tests for visualization functions
# Note: is_s7_class helper defined in helper-mock-data.R

# Create test data for plots
create_plot_test_data <- function() {
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
    device = "Test Device",
    protocol = "Incremental"
  )

  # Create realistic breath data with all variables
  n <- 100
  time_s <- seq(0, by = 10, length.out = n)
  intensity <- pmin(time_s / max(time_s), 1)

  set.seed(42)
  breaths <- tibble::tibble(
    time_s = time_s,
    vo2_ml = 300 + intensity * 2700 + rnorm(n, 0, 30),
    vco2_ml = 250 + intensity * 3000 + rnorm(n, 0, 30),
    ve_l = 10 + intensity * 120 + rnorm(n, 0, 2),
    rer = vco2_ml / vo2_ml,
    hr_bpm = 70 + intensity * 120 + rnorm(n, 0, 2),
    power_w = round(intensity * 300),
    bf = 12 + intensity * 38,
    vt_l = ve_l / bf,
    peto2_mmhg = 100 + intensity * 15 + rnorm(n, 0, 2),
    petco2_mmhg = 40 - intensity * 5 + rnorm(n, 0, 1),
    phase = dplyr::case_when(
      time_s < 120 ~ "REST",
      time_s < 300 ~ "WARMUP",
      TRUE ~ "EXERCISE"
    )
  )

  CpetData(
    participant = participant,
    metadata = metadata,
    breaths = breaths,
    is_averaged = FALSE
  )
}


# plot_cpet_panel() tests --------------------------------------------------

test_that("plot_cpet_panel returns a patchwork object", {
  skip_if_not_installed("ggplot2")
  skip_if_not_installed("patchwork")

  data <- create_plot_test_data()
  p <- plot_cpet_panel(data)

  expect_s3_class(p, "patchwork")
})

test_that("plot_cpet_panel works with CpetAnalysis object", {
  skip_if_not_installed("ggplot2")
  skip_if_not_installed("patchwork")

  data <- create_plot_test_data()

  analysis <- CpetAnalysis(data = data)
  p <- plot_cpet_panel(analysis)

  expect_s3_class(p, "patchwork")
})

test_that("plot_cpet_panel supports different time axes", {
  skip_if_not_installed("ggplot2")
  skip_if_not_installed("patchwork")

  data <- create_plot_test_data()

  p_seconds <- plot_cpet_panel(data, time_axis = "seconds")
  p_minutes <- plot_cpet_panel(data, time_axis = "minutes")
  p_auto <- plot_cpet_panel(data, time_axis = "auto")

  expect_s3_class(p_seconds, "patchwork")
  expect_s3_class(p_minutes, "patchwork")
  expect_s3_class(p_auto, "patchwork")
})

test_that("plot_cpet_panel supports French language", {
  skip_if_not_installed("ggplot2")
  skip_if_not_installed("patchwork")

  data <- create_plot_test_data()
  p <- plot_cpet_panel(data, language = "fr")

  expect_s3_class(p, "patchwork")
})

test_that("plot_cpet_panel works without power data", {
  skip_if_not_installed("ggplot2")
  skip_if_not_installed("patchwork")

  data <- create_plot_test_data()
  data@breaths$power_w <- NULL

  p <- plot_cpet_panel(data)

  expect_s3_class(p, "patchwork")
})


# plot_v_slope() tests -----------------------------------------------------

test_that("plot_v_slope returns a ggplot object", {
  skip_if_not_installed("ggplot2")

  data <- create_plot_test_data()
  p <- plot_v_slope(data)

  expect_s3_class(p, "ggplot")
})

test_that("plot_v_slope shows identity line when requested", {
  skip_if_not_installed("ggplot2")

  data <- create_plot_test_data()

  p_with <- plot_v_slope(data, show_identity = TRUE)
  p_without <- plot_v_slope(data, show_identity = FALSE)

  expect_s3_class(p_with, "ggplot")
  expect_s3_class(p_without, "ggplot")
})

test_that("plot_v_slope works with thresholds", {
  skip_if_not_installed("ggplot2")

  data <- create_plot_test_data()
  thresholds <- Thresholds(
    vt1_vo2 = 1500,
    vt2_vo2 = 2500,
    confidence = "moderate"
  )

  p <- plot_v_slope(data, thresholds = thresholds)

  expect_s3_class(p, "ggplot")
})

test_that("plot_v_slope uses stage summary points for CpetAnalysis", {
  skip_if_not_installed("ggplot2")

  data <- create_plot_test_data()
  data_with_stages <- extract_stages(data, protocol = "step", stage_duration = 120)
  stage_summary <- summarize_stages(data_with_stages, window_s = 30)

  analysis <- CpetAnalysis(
    data = data,
    stage_summary = stage_summary
  )

  p <- plot_v_slope(analysis)
  point_layer_rows <- nrow(ggplot2::ggplot_build(p)$data[[1]])

  expect_equal(point_layer_rows, nrow(stage_summary))
})


# plot_ventilatory_equivalents() tests ------------------------------------

test_that("plot_ventilatory_equivalents returns a ggplot object", {
  skip_if_not_installed("ggplot2")

  data <- create_plot_test_data()
  p <- plot_ventilatory_equivalents(data)

  expect_s3_class(p, "ggplot")
})

test_that("plot_ventilatory_equivalents supports different x-axes", {
  skip_if_not_installed("ggplot2")

  data <- create_plot_test_data()

  p_time <- plot_ventilatory_equivalents(data, x_axis = "time")
  p_vo2 <- plot_ventilatory_equivalents(data, x_axis = "vo2")

  expect_s3_class(p_time, "ggplot")
  expect_s3_class(p_vo2, "ggplot")
})


# plot_gas_exchange() tests -----------------------------------------------

test_that("plot_gas_exchange returns a ggplot object", {
  skip_if_not_installed("ggplot2")

  data <- create_plot_test_data()
  p <- plot_gas_exchange(data)

  expect_s3_class(p, "ggplot")
})

test_that("plot_gas_exchange supports different variable combinations", {
  skip_if_not_installed("ggplot2")

  data <- create_plot_test_data()

  p1 <- plot_gas_exchange(data, variables = c("vo2"))
  p2 <- plot_gas_exchange(data, variables = c("vo2", "vco2"))
  p3 <- plot_gas_exchange(data, variables = c("vo2", "vco2", "rer"))

  expect_s3_class(p1, "ggplot")
  expect_s3_class(p2, "ggplot")
  expect_s3_class(p3, "ggplot")
})

test_that("plot_gas_exchange supports smoothing", {
  skip_if_not_installed("ggplot2")

  data <- create_plot_test_data()

  p_raw <- plot_gas_exchange(data, smooth = FALSE)
  p_smooth <- plot_gas_exchange(data, smooth = TRUE)

  expect_s3_class(p_raw, "ggplot")
  expect_s3_class(p_smooth, "ggplot")
})

test_that("plot_gas_exchange supports normalization", {
  skip_if_not_installed("ggplot2")

  data <- create_plot_test_data()

  p_abs <- plot_gas_exchange(data, normalize = FALSE)
  p_norm <- plot_gas_exchange(data, normalize = TRUE)

  expect_s3_class(p_abs, "ggplot")
  expect_s3_class(p_norm, "ggplot")
})


# plot_heart_rate() tests -------------------------------------------------

test_that("plot_heart_rate returns a ggplot object", {
  skip_if_not_installed("ggplot2")

  data <- create_plot_test_data()
  p <- plot_heart_rate(data)

  expect_s3_class(p, "ggplot")
})

test_that("plot_heart_rate supports different x-axes", {
  skip_if_not_installed("ggplot2")

  data <- create_plot_test_data()

  p_time <- plot_heart_rate(data, x_axis = "time")
  p_vo2 <- plot_heart_rate(data, x_axis = "vo2")

  expect_s3_class(p_time, "ggplot")
  expect_s3_class(p_vo2, "ggplot")
})

test_that("plot_heart_rate uses stage summary points for CpetAnalysis", {
  skip_if_not_installed("ggplot2")

  data <- create_plot_test_data()
  data_with_stages <- extract_stages(data, protocol = "step", stage_duration = 120)
  stage_summary <- summarize_stages(data_with_stages, window_s = 30)

  analysis <- CpetAnalysis(
    data = data,
    stage_summary = stage_summary
  )

  p <- plot_heart_rate(analysis)
  point_layer_rows <- nrow(ggplot2::ggplot_build(p)$data[[1]])

  expect_equal(point_layer_rows, nrow(stage_summary))
})

test_that("plot_heart_rate shows training zones", {
  skip_if_not_installed("ggplot2")

  data <- create_plot_test_data()
  p <- plot_heart_rate(data, show_zones = TRUE)

  expect_s3_class(p, "ggplot")
})

test_that("plot_heart_rate errors when HR not available", {
  skip_if_not_installed("ggplot2")

  # Create data without HR
  participant <- Participant(
    id = "X", name = "X", age = 30, sex = "M",
    height_cm = 175, weight_kg = 70
  )
  metadata <- CpetMetadata(
    test_date = Sys.Date(), device = "X", protocol = "X"
  )
  breaths <- tibble::tibble(
    time_s = 1:10,
    vo2_ml = rep(300, 10),
    vco2_ml = rep(250, 10),
    ve_l = rep(10, 10),
    rer = rep(0.83, 10)
  )
  data <- CpetData(
    participant = participant,
    metadata = metadata,
    breaths = breaths,
    is_averaged = FALSE
  )

  expect_error(plot_heart_rate(data), "Heart rate data not available")
})


# plot_power() tests ------------------------------------------------------

test_that("plot_power returns a ggplot object", {
  skip_if_not_installed("ggplot2")

  data <- create_plot_test_data()
  p <- plot_power(data)

  expect_s3_class(p, "ggplot")
})

test_that("plot_power works with and without VO2 overlay", {
  skip_if_not_installed("ggplot2")

  data <- create_plot_test_data()

  p_with <- plot_power(data, show_vo2 = TRUE)
  p_without <- plot_power(data, show_vo2 = FALSE)

  expect_s3_class(p_with, "ggplot")
  expect_s3_class(p_without, "ggplot")
})

test_that("plot_power errors when power not available", {
  skip_if_not_installed("ggplot2")

  # Create data without power
  participant <- Participant(
    id = "X", name = "X", age = 30, sex = "M",
    height_cm = 175, weight_kg = 70
  )
  metadata <- CpetMetadata(
    test_date = Sys.Date(), device = "X", protocol = "X"
  )
  breaths <- tibble::tibble(
    time_s = 1:10,
    vo2_ml = rep(300, 10),
    vco2_ml = rep(250, 10),
    ve_l = rep(10, 10),
    rer = rep(0.83, 10)
  )
  data <- CpetData(
    participant = participant,
    metadata = metadata,
    breaths = breaths,
    is_averaged = FALSE
  )

  expect_error(plot_power(data), "Power data not available")
})


# Integration tests -------------------------------------------------------

test_that("All plots work with real COSMED data", {
  skip_if_not_installed("ggplot2")
  skip_if_not_installed("patchwork")
  skip_if_not_installed("readxl")

  example_file <- system.file("extdata", "example_cosmed.xlsx", package = "cardiometR")
  skip_if(example_file == "", "Example file not found")

  data <- read_cosmed(example_file)

  # All plots should work without error
  expect_no_error(plot_cpet_panel(data))
  expect_no_error(plot_v_slope(data))
  expect_no_error(plot_ventilatory_equivalents(data))
  expect_no_error(plot_gas_exchange(data))
  expect_no_error(plot_heart_rate(data))
  expect_no_error(plot_power(data))
})


# filter_exercise_data() tests -----------------------------------------------

test_that("filter_exercise_data removes stage 0", {
  df <- tibble::tibble(
    time_s = 1:10,
    vo2_ml = rep(300, 10),
    stage = c(0, 0, 0, 1, 1, 1, 2, 2, 2, 2)
  )
  result <- filter_exercise_data(df)
  expect_equal(nrow(result), 7)
  expect_true(all(result$stage > 0))
})

test_that("filter_exercise_data uses power_w when no stage column", {
  df <- tibble::tibble(
    time_s = 1:10,
    vo2_ml = rep(300, 10),
    power_w = c(0, 0, 0, 50, 100, 150, 200, 250, 300, 300)
  )
  result <- filter_exercise_data(df)
  expect_equal(nrow(result), 7)
  expect_true(all(result$power_w > 0))
})

test_that("filter_exercise_data filters by phase column", {
  df <- tibble::tibble(
    time_s = 1:10,
    vo2_ml = rep(300, 10),
    phase = c("REST", "REST", "WARMUP", "EXERCISE", "EXERCISE",
              "EXERCISE", "EXERCISE", "EXERCISE", "EXERCISE", "EXERCISE")
  )
  result <- filter_exercise_data(df)
  expect_equal(nrow(result), 7)  # exercise only (warmup excluded)
})

test_that("filter_exercise_data returns unchanged data without indicators", {
  df <- tibble::tibble(
    time_s = 1:5,
    vo2_ml = rep(300, 5)
  )
  result <- filter_exercise_data(df)
  expect_equal(nrow(result), 5)
})


# Expected VO2 calculation tests -----------------------------------------------

test_that("calculate_expected_vo2_treadmill returns correct values", {
  # At 10 km/h, 70 kg: VO2 = (2.209 + 3.163 * 10) * 70 = 33.839 * 70 = 2368.73
  result <- calculate_expected_vo2_treadmill(speed_kmh = 10, weight_kg = 70)
  expect_equal(result, (2.209 + 3.163 * 10) * 70, tolerance = 0.01)
})

test_that("calculate_expected_vo2_treadmill is vectorized", {
  result <- calculate_expected_vo2_treadmill(speed_kmh = c(8, 10, 12), weight_kg = 70)
  expect_length(result, 3)
  expect_true(all(diff(result) > 0))
})

test_that("calculate_expected_vo2_cycling returns correct values", {
  # ACSM leg-cycling: VO2 = slope * power + 7 * body_mass
  expected <- 10.8 * 200 + 7 * 70
  result <- calculate_expected_vo2_cycling(power_w = 200, weight_kg = 70)
  expect_equal(result, expected, tolerance = 0.01)
})

test_that("calculate_expected_vo2_cycling is vectorized", {
  result <- calculate_expected_vo2_cycling(power_w = c(100, 200, 300), weight_kg = 70)
  expect_length(result, 3)
  expect_true(all(diff(result) > 0))
})

test_that("calculate_expected_vo2_cycling responds to slope", {
  low_slope <- calculate_expected_vo2_cycling(200, 70, slope_ml_per_w = 9)
  high_slope <- calculate_expected_vo2_cycling(200, 70, slope_ml_per_w = 12)
  expect_true(high_slope > low_slope)
})

# Tests for Economy Calculation Functions
# Tests calculate_running_economy(), calculate_gross_efficiency(), calculate_economy_metrics()

test_that("calculate_running_economy returns correct values", {
  # At 12 km/h with VO2 of 40 mL/kg/min
  # Economy = 40 / 12 * 60 = 200 mL/kg/km
  result <- calculate_running_economy(40, 12)
  expect_equal(result, 200)

  # Better economy at same VO2 but faster speed
  result2 <- calculate_running_economy(40, 14)
  expect_equal(result2, round(40 / 14 * 60, 1))  # ~171.4

  # Elite runner example: 35 mL/kg/min at 18 km/h
  result3 <- calculate_running_economy(35, 18)
  expect_equal(result3, round(35 / 18 * 60, 1))  # ~116.7
})

test_that("calculate_running_economy validates inputs", {
  # Missing parameters
  expect_error(calculate_running_economy(), "Both vo2_kg and speed_kmh are required")
  expect_error(calculate_running_economy(40), "Both vo2_kg and speed_kmh are required")

  # Invalid speed
  expect_error(calculate_running_economy(40, 0), "speed_kmh must be positive")
  expect_error(calculate_running_economy(40, -5), "speed_kmh must be positive")

  # Invalid VO2
  expect_error(calculate_running_economy(-10, 12), "vo2_kg cannot be negative")
})

test_that("calculate_gross_efficiency returns correct values", {
  # At 3000 mL/min VO2, 250W, RER 1.0
  # Energy eq = 5.05 kcal/L O2
  # Energy (kcal/min) = 3.0 L/min * 5.05 = 15.15 kcal/min
  # Energy (W) = 15.15 * 69.78 = 1057.2 W
  # GE = 250 / 1057.2 * 100 = 23.6%
  result <- calculate_gross_efficiency(3000, 250, 1.0)
  expect_true(result > 20 && result < 25)

  # Higher power = higher efficiency at same VO2
  result_high <- calculate_gross_efficiency(3000, 300, 1.0)
  expect_true(result_high > result)

  # Lower RER = lower energy equivalent = higher efficiency
  result_low_rer <- calculate_gross_efficiency(3000, 250, 0.85)
  expect_true(result_low_rer > result)
})

test_that("calculate_gross_efficiency validates inputs", {
  # Missing parameters - R throws missing argument error before function body runs
  expect_error(calculate_gross_efficiency())
  expect_error(calculate_gross_efficiency(3000))

  # Invalid power
  expect_error(calculate_gross_efficiency(3000, 0), "power_w must be positive")
  expect_error(calculate_gross_efficiency(3000, -100), "power_w must be positive")

  # Invalid VO2
  expect_error(calculate_gross_efficiency(0, 250), "vo2_ml must be positive")
  expect_error(calculate_gross_efficiency(-500, 250), "vo2_ml must be positive")
})

test_that("calculate_gross_efficiency constrains RER to valid range", {
  # RER below 0.70 should be clamped
  result_low <- calculate_gross_efficiency(3000, 250, 0.50)
  result_min <- calculate_gross_efficiency(3000, 250, 0.70)
  expect_equal(result_low, result_min)

  # RER above 1.10 should be clamped
  result_high <- calculate_gross_efficiency(3000, 250, 1.50)
  result_max <- calculate_gross_efficiency(3000, 250, 1.10)
  expect_equal(result_high, result_max)
})

test_that("calculate_gross_efficiency returns plausible values", {
  # Typical trained cyclist: 20-22%
  result <- calculate_gross_efficiency(2500, 200, 0.90)
  expect_true(result >= 15 && result <= 30)

  # Elite cyclist values: 22-28%
  result_elite <- calculate_gross_efficiency(2800, 280, 0.95)
  expect_true(result_elite >= 18 && result_elite <= 30)
})

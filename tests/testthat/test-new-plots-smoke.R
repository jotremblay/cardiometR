# Smoke tests for Phase 4 plot upgrades

test_that("new Phase 4 plot functions return ggplot objects", {
  skip_if_not_installed("ggplot2")

  analysis <- create_mock_cpet_analysis()

  # plot_vo2_power_slope
  p1 <- plot_vo2_power_slope(analysis, language = "en")
  expect_s3_class(p1, "ggplot")

  # plot_zscore_strip (z_scores may be NULL — must still render)
  p2 <- plot_zscore_strip(analysis, language = "fr")
  expect_s3_class(p2, "ggplot")

  # plot_longitudinal_delta: NULL prior returns NULL
  expect_null(plot_longitudinal_delta(analysis, NULL))

  # With same analysis as prior, should render (zero delta)
  p3 <- plot_longitudinal_delta(analysis, analysis, language = "en")
  expect_s3_class(p3, "ggplot")
})

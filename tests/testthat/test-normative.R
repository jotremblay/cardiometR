# tests/testthat/test-normative.R
# Tests for normative data functions

# =============================================================================
# get_normative_data Tests
# =============================================================================

test_that("get_normative_data returns expected structure", {
  result <- get_normative_data("cycling", "elite", "M", 30)

  expect_type(result, "list")
  expect_true("vo2max_low" %in% names(result))
  expect_true("vo2max_high" %in% names(result))
  expect_true("vo2max_typical" %in% names(result))
  expect_true("description" %in% names(result))
  expect_true("citation" %in% names(result))
  expect_true("citation_short" %in% names(result))
})

test_that("get_normative_data returns valid VO2max ranges", {
  result <- get_normative_data("cycling", "elite", "M", 30)

  expect_true(result$vo2max_low < result$vo2max_typical)
  expect_true(result$vo2max_typical < result$vo2max_high)
  expect_true(result$vo2max_low > 0)
  expect_true(result$vo2max_high < 100)  # Physiological upper bound
})

test_that("get_normative_data works for all sports", {
  sports <- c("cycling", "running", "triathlon", "general")

  for (sport in sports) {
    result <- get_normative_data(sport, "recreational", "M", 30)
    expect_type(result, "list")
    expect_true(result$vo2max_typical > 0,
                label = sprintf("VO2max for %s", sport))
  }
})

test_that("get_normative_data works for all levels", {
  levels <- c("elite", "competitive", "recreational", "sedentary")

  for (level in levels) {
    result <- get_normative_data("cycling", level, "M", 30)
    expect_type(result, "list")
    expect_true(result$vo2max_typical > 0,
                label = sprintf("VO2max for %s level", level))
  }
})

test_that("get_normative_data works for both sexes", {
  result_m <- get_normative_data("cycling", "elite", "M", 30)
  result_f <- get_normative_data("cycling", "elite", "F", 30)

  expect_type(result_m, "list")
  expect_type(result_f, "list")

  # Male elite values should be higher than female elite
  expect_true(result_m$vo2max_typical > result_f$vo2max_typical)
})

test_that("get_normative_data handles case-insensitive inputs", {
  result1 <- get_normative_data("Cycling", "Elite", "m", 30)
  result2 <- get_normative_data("CYCLING", "ELITE", "M", 30)

  expect_equal(result1$vo2max_typical, result2$vo2max_typical)
})

test_that("get_normative_data validates sport argument", {
  expect_error(get_normative_data("swimming", "elite", "M", 30))
  expect_error(get_normative_data("", "elite", "M", 30))
})

test_that("get_normative_data validates level argument", {
  expect_error(get_normative_data("cycling", "professional", "M", 30))
  expect_error(get_normative_data("cycling", "", "M", 30))
})

test_that("get_normative_data validates sex argument", {
  expect_error(get_normative_data("cycling", "elite", "X", 30))
  expect_error(get_normative_data("cycling", "elite", "", 30))
})

test_that("elite cyclists have higher VO2max than recreational", {
  elite <- get_normative_data("cycling", "elite", "M", 30)
  recreational <- get_normative_data("cycling", "recreational", "M", 30)

  expect_true(elite$vo2max_typical > recreational$vo2max_typical)
  expect_true(elite$vo2max_low > recreational$vo2max_low)
})

test_that("cycling data includes efficiency metrics", {
  result <- get_normative_data("cycling", "elite", "M", 30)

  expect_true("efficiency_typical" %in% names(result))
  expect_true("efficiency_unit" %in% names(result))
  expect_equal(result$efficiency_unit, "%")
  expect_true(result$efficiency_typical > 15)  # Gross efficiency above 15%
  expect_true(result$efficiency_typical < 30)  # Below 30%
})

test_that("running data includes economy metrics", {
  result <- get_normative_data("running", "elite", "M", 30)

  expect_true("economy_typical" %in% names(result))
  expect_true("economy_unit" %in% names(result))
  expect_equal(result$economy_unit, "mL/kg/km")
  expect_true(result$economy_typical > 150)
  expect_true(result$economy_typical < 300)
})


# =============================================================================
# Age Group Tests
# =============================================================================

test_that("age groups are assigned correctly", {
  # Test internal function via get_normative_data behavior
  young <- get_normative_data("general", "sedentary", "M", 25)
  middle <- get_normative_data("general", "sedentary", "M", 45)
  senior <- get_normative_data("general", "sedentary", "M", 65)

  # VO2max should decline with age in general population

  expect_true(young$vo2max_typical > middle$vo2max_typical)
  expect_true(middle$vo2max_typical > senior$vo2max_typical)
})

test_that("age boundary conditions work correctly", {
  # Test ages at category boundaries
  age_19 <- get_normative_data("general", "sedentary", "M", 19)
  age_20 <- get_normative_data("general", "sedentary", "M", 20)
  age_29 <- get_normative_data("general", "sedentary", "M", 29)
  age_30 <- get_normative_data("general", "sedentary", "M", 30)

  expect_type(age_19, "list")
  expect_type(age_20, "list")
  expect_type(age_29, "list")
  expect_type(age_30, "list")

  # 19 should be junior, 20-29 same group
  expect_equal(age_20$vo2max_typical, age_29$vo2max_typical)
})

test_that("extreme ages work correctly", {
  young <- get_normative_data("general", "sedentary", "M", 15)
  old <- get_normative_data("general", "sedentary", "M", 85)

  expect_type(young, "list")
  expect_type(old, "list")

  # Both should have valid VO2max values
  expect_true(young$vo2max_typical > 0)
  expect_true(old$vo2max_typical > 0)
})


# =============================================================================
# FRIEND Registry Tests
# =============================================================================

test_that("FRIEND percentiles have correct structure", {
  result <- get_normative_data("general", "sedentary", "M", 35)

  # FRIEND citation should be mentioned
  expect_true(grepl("FRIEND", result$citation) || grepl("Kaminsky", result$citation))
})

test_that("FRIEND values follow expected age decline", {
  ages <- c(25, 35, 45, 55, 65, 75)
  values <- sapply(ages, function(a) {
    get_normative_data("general", "sedentary", "M", a)$vo2max_typical
  })

  # Each subsequent value should be lower or equal
  for (i in 2:length(values)) {
    expect_true(values[i] <= values[i-1],
                info = sprintf("Age %d should be <= age %d", ages[i], ages[i-1]))
  }
})

test_that("FRIEND values differ by sex", {
  male <- get_normative_data("general", "sedentary", "M", 35)
  female <- get_normative_data("general", "sedentary", "F", 35)

  # Male VO2max typically higher than female
  expect_true(male$vo2max_typical > female$vo2max_typical)
})


# =============================================================================
# list_normative_categories Tests
# =============================================================================

test_that("list_normative_categories returns expected structure", {
  result <- list_normative_categories()

  expect_s3_class(result, "tbl_df")
  expect_true("sport" %in% names(result))
  expect_true("level" %in% names(result))
  expect_true("sex" %in% names(result))
  expect_true("vo2max_typical" %in% names(result))
  expect_true("description" %in% names(result))
})

test_that("list_normative_categories includes all sports", {
  result <- list_normative_categories()

  expect_true("cycling" %in% result$sport)
  expect_true("running" %in% result$sport)
  expect_true("triathlon" %in% result$sport)
  expect_true("general" %in% result$sport)
})

test_that("list_normative_categories has reasonable row count", {
  result <- list_normative_categories()

  # Should have multiple entries (sport x level x sex combinations)
  expect_true(nrow(result) >= 18)  # At least 18 combinations
  expect_true(nrow(result) <= 50)  # Reasonable upper bound
})

test_that("list_normative_categories values are consistent with get_normative_data", {
  categories <- list_normative_categories()

  # Check first cycling elite male entry
  cat_row <- categories |>
    dplyr::filter(sport == "cycling", level == "elite", sex == "M")

  if (nrow(cat_row) > 0) {
    norms <- get_normative_data("cycling", "elite", "M", 30)
    expect_equal(cat_row$vo2max_typical[1], norms$vo2max_typical)
  }
})


# =============================================================================
# Citation Tests
# =============================================================================

test_that("citations are provided for all categories", {
  sports <- c("cycling", "running", "triathlon")
  levels <- c("elite", "competitive", "recreational")

  for (sport in sports) {
    for (level in levels) {
      result <- get_normative_data(sport, level, "M", 30)
      expect_true(nchar(result$citation) > 20,
                  info = sprintf("%s/%s should have citation", sport, level))
      expect_true(nchar(result$citation_short) > 5,
                  info = sprintf("%s/%s should have short citation", sport, level))
    }
  }
})

test_that("citations include DOI or journal reference", {
  result <- get_normative_data("cycling", "elite", "M", 30)

  # Should contain either DOI or standard journal reference
  has_reference <- grepl("doi:", result$citation, ignore.case = TRUE) ||
                   grepl("[0-9]{4};", result$citation) ||  # Year;volume pattern
                   grepl("Med", result$citation)  # Journal name

  expect_true(has_reference)
})


# =============================================================================
# Edge Cases
# =============================================================================

test_that("sedentary level uses FRIEND data", {
  result <- get_normative_data("cycling", "sedentary", "M", 35)

  # Sedentary should fall back to general population (FRIEND)
  expect_true(grepl("FRIEND|Kaminsky|general", result$description, ignore.case = TRUE))
})

test_that("general sport uses FRIEND data regardless of level", {
  result <- get_normative_data("general", "elite", "M", 35)

  # General sport should use FRIEND even if "elite" is specified
  expect_true(grepl("FRIEND|Kaminsky|General", result$description, ignore.case = TRUE))
})

test_that("HR max is calculated correctly for general population", {
  result <- get_normative_data("general", "sedentary", "M", 35)

  if ("hr_max_typical" %in% names(result)) {
    # Tanaka formula: 208 - 0.7 * age
    expected_hr <- round(208 - 0.7 * 35)
    expect_true(abs(result$hr_max_typical - expected_hr) < 10)  # Within 10 bpm
  }
})

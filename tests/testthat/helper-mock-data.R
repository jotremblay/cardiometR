# Helper function to check S7 class (shared across all tests)
is_s7_class <- function(x, class_name) {
  if (inherits(x, class_name)) return(TRUE)
  if (!inherits(x, "S7_object")) return(FALSE)
  # Check if class name matches (with or without package prefix)
  actual_class <- class(x)[1]
  grepl(paste0(class_name, "$"), actual_class)
}

# Helper to check if S7 property is "empty" (NULL, NA, or zero-length)
is_empty_prop <- function(x) {
  is.null(x) || length(x) == 0 || (length(x) == 1 && is.na(x))
}

#' Create mock CPET breath-by-breath data for testing
#'
#' Generates realistic simulated CPET data for unit tests.
#'
#' @param n_breaths Number of breaths to generate (default 300)
#' @param include_hr Include heart rate data (default TRUE)
#' @param include_power Include power/workload data (default TRUE)
#' @param participant_id Participant ID (default "TEST001")
#'
#' @return A list with participant, metadata, and breaths tibble
#' @keywords internal
create_mock_breath_data <- function(n_breaths = 300,
                                     include_hr = TRUE,
                                     include_power = TRUE,
                                     participant_id = "TEST001") {
  # Generate time series
 time_s <- seq(0, by = 2, length.out = n_breaths)

  # Simulate incremental exercise (0 to 100% intensity)
  intensity <- pmin(time_s / max(time_s), 1)

  # Generate physiologically realistic breath-by-breath data
  set.seed(42)  # For reproducibility

  breaths <- tibble::tibble(
    time_s = time_s,
    # VO2: resting ~300, peak ~3000 mL/min
    vo2_ml = 300 + intensity * 2700 + rnorm(n_breaths, 0, 50),
    # VCO2: slightly lower at rest, higher at peak (RER increases)
    vco2_ml = 250 + intensity * 3000 + rnorm(n_breaths, 0, 50),
    # VE: resting ~10, peak ~130 L/min
    ve_l = 10 + intensity * 120 + rnorm(n_breaths, 0, 3),
    # Breathing frequency: resting ~12, peak ~50 breaths/min
    bf = 12 + intensity * 38 + rnorm(n_breaths, 0, 2)
  )

  # Derived variables
  breaths <- breaths |>
    dplyr::mutate(
      rer = vco2_ml / vo2_ml,
      vt_l = ve_l / bf,
      ve_vo2 = ve_l * 1000 / vo2_ml,
      ve_vco2 = ve_l * 1000 / vco2_ml,
      # End-tidal gases
      peto2_mmhg = 100 + intensity * 15 + rnorm(n_breaths, 0, 2),
      petco2_mmhg = 40 - intensity * 5 + rnorm(n_breaths, 0, 1)
    )

  # Optional HR
 if (include_hr) {
    breaths$hr_bpm <- 70 + intensity * 120 + rnorm(n_breaths, 0, 3)
  }

  # Optional power
  if (include_power) {
    breaths$power_w <- round(intensity * 300)
  }

  # Create participant info
  participant_info <- list(
    id = participant_id,
    name = "Test Subject",
    age = 30,
    sex = "M",
    height_cm = 175,
    weight_kg = 70,
    sport = NULL
  )

  # Create metadata
  metadata_info <- list(
    test_date = Sys.Date(),
    device = "Mock COSMED Quark",
    calibration_date = Sys.Date() - 1,
    temperature_c = 22,
    pressure_mmhg = 760,
    humidity_pct = 50,
    protocol = "Incremental",
    technician = "Test Technician"
  )

  list(
    participant = participant_info,
    metadata = metadata_info,
    breaths = breaths
  )
}

#' Create minimal valid breath data
#'
#' Creates the minimum required columns for CpetData validation.
#'
#' @param n Number of rows
#' @return A tibble with required columns
#' @keywords internal
create_minimal_breaths <- function(n = 10) {
  tibble::tibble(
    time_s = seq(0, by = 2, length.out = n),
    vo2_ml = rep(300, n),
    vco2_ml = rep(250, n),
    ve_l = rep(10, n),
    rer = rep(0.83, n)
  )
}

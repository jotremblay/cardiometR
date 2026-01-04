# Economy and Efficiency Calculation Methods for cardiometR
# Running economy, cycling gross efficiency, and related metrics

#' Calculate Running Economy
#'
#' @description
#' Calculates running economy as oxygen cost per kilometer.
#' Running economy represents the metabolic cost of running at a given speed,
#' with lower values indicating better economy (less oxygen required to cover
#' the same distance).
#'
#' @param vo2_kg VO2 in mL/kg/min at submaximal steady state
#' @param speed_kmh Running speed in km/h
#'
#' @return Running economy in mL O2/kg/km
#'
#' @details
#' Formula: Running Economy = VO2 (mL/kg/min) / Speed (km/h) * 60
#'
#' Typical values:
#' - Elite runners: 150-180 mL/kg/km
#' - Trained runners: 180-220 mL/kg/km
#' - Recreational runners: 220-280 mL/kg/km
#'
#' @references
#' Saunders PU, Pyne DB, Telford RD, Hawley JA. Factors affecting running economy
#' in trained distance runners. Sports Med. 2004;34(7):465-485.
#'
#' @examples
#' # At 12 km/h with VO2 of 40 mL/kg/min
#' calculate_running_economy(40, 12)  # Returns 200 mL/kg/km
#'
#' # Better economy at same VO2 but faster speed
#' calculate_running_economy(40, 14)  # Returns ~171 mL/kg/km
#'
#' @export
calculate_running_economy <- function(vo2_kg, speed_kmh) {
  if (missing(vo2_kg) || missing(speed_kmh)) {
    cli::cli_abort("Both vo2_kg and speed_kmh are required")
  }

  if (speed_kmh <= 0) {
    cli::cli_abort("speed_kmh must be positive")
  }

  if (vo2_kg < 0) {
    cli::cli_abort("vo2_kg cannot be negative")
  }

 # Convert VO2/min to VO2/km
  # VO2 (mL/kg/min) / speed (km/h) * 60 min/h = mL/kg/km
  economy <- vo2_kg / speed_kmh * 60

  round(economy, 1)
}


#' Calculate Gross Efficiency
#'
#' @description
#' Calculates cycling gross efficiency (GE%) from VO2 and power output.
#' Gross efficiency represents the ratio of mechanical work output to
#' metabolic energy expenditure.
#'
#' @param vo2_ml VO2 in mL/min
#' @param power_w Power output in watts
#' @param rer Respiratory exchange ratio (default 1.0 for high intensity)
#'
#' @return Gross efficiency as percentage
#'
#' @details
#' Formula: GE = (Power output / Energy expenditure) * 100
#'
#' The energy equivalent of oxygen varies with RER:
#' - RER 0.70: 4.69 kcal/L O2 (pure fat oxidation)
#' - RER 1.00: 5.05 kcal/L O2 (pure carbohydrate oxidation)
#'
#' Typical values:
#' - Elite cyclists: 22-25%
#' - Trained cyclists: 19-22%
#' - Recreational: 17-20%
#'
#' @references
#' Coyle EF, Sidossis LS, Horowitz JF, Beltz JD. Cycling efficiency is related
#' to the percentage of type I muscle fibers. Med Sci Sports Exerc. 1992;24(7):782-788.
#'
#' @examples
#' # At 3000 mL/min VO2, 250W, RER 1.0
#' calculate_gross_efficiency(3000, 250, 1.0)  # ~20.9%
#'
#' @export
calculate_gross_efficiency <- function(vo2_ml, power_w, rer = 1.0) {
  if (missing(vo2_ml) || missing(power_w)) {
    cli::cli_abort("Both vo2_ml and power_w are required")
  }

  if (power_w <= 0) {
    cli::cli_abort("power_w must be positive")
  }

  if (vo2_ml <= 0) {
    cli::cli_abort("vo2_ml must be positive")
  }

  # Constrain RER to valid range
  rer <- min(max(rer, 0.70), 1.10)

  # Energy equivalent of O2 depends on RER (kcal/L O2)
  # Linear interpolation between RER 0.70 and 1.00
  energy_eq <- 4.69 + (rer - 0.70) * (5.05 - 4.69) / 0.30

  # Convert VO2 from mL/min to L/min
  vo2_l <- vo2_ml / 1000

  # Energy expenditure in kcal/min
  energy_kcal <- vo2_l * energy_eq

  # Convert to watts: 1 kcal/min = 69.78 watts
  energy_watts <- energy_kcal * 69.78

  # Gross efficiency
  ge <- (power_w / energy_watts) * 100

  round(ge, 1)
}


#' Calculate Economy Metrics for Analysis
#'
#' @description
#' Wrapper function to calculate all economy/efficiency metrics for a CpetAnalysis
#' object at a user-specified reference stage.
#'
#' @param analysis CpetAnalysis object with stage_summary populated
#' @param reference_stage Stage number to use for economy calculations
#' @param speed_kmh For treadmill tests, the speed at the reference stage in km/h.
#'   If NULL and stage_summary has a speed column, that will be used.
#'
#' @return An EconomyMetrics S7 object
#'
#' @details
#' For cycling tests: Calculates gross efficiency using power and VO2 at the
#' reference stage.
#'
#' For treadmill tests: Calculates running economy using VO2/kg and speed at
#' the reference stage.
#'
#' The modality is automatically detected from the analysis protocol metadata.
#'
#' @examples
#' \dontrun{
#' # Calculate efficiency at stage 4
#' economy <- calculate_economy_metrics(analysis, reference_stage = 4)
#'
#' # For treadmill with known speed
#' economy <- calculate_economy_metrics(analysis, reference_stage = 3, speed_kmh = 12)
#' }
#'
#' @export
calculate_economy_metrics <- function(analysis, reference_stage, speed_kmh = NULL) {
  if (!inherits(analysis, "CpetAnalysis")) {
    cli::cli_abort("analysis must be a CpetAnalysis object")
  }

  if (is.null(analysis@stage_summary) || nrow(analysis@stage_summary) == 0) {
    cli::cli_abort("analysis must have stage_summary populated. Run summarize_stages() first.")
  }

  stage_summary <- analysis@stage_summary
  n_stages <- nrow(stage_summary)

  if (reference_stage < 1 || reference_stage > n_stages) {
    cli::cli_abort("reference_stage must be between 1 and {n_stages}")
  }

  # Get reference stage data
  ref_data <- stage_summary[reference_stage, ]

  # Detect modality from metadata protocol
  protocol_lower <- tolower(analysis@data@metadata@protocol)
  modality <- dplyr::case_when(
    grepl("treadmill|running|walk|bruce", protocol_lower) ~ "treadmill",
    grepl("cycle|bike|ergo|watt", protocol_lower) ~ "cycling",
    TRUE ~ "cycling"  # default
  )

  # Override with protocol_config if available
  if (!is.null(analysis@protocol_config)) {
    modality <- analysis@protocol_config@modality
  }

  # Initialize result
  result <- list(
    modality = modality,
    reference_stage = reference_stage,
    gross_efficiency = NULL,
    running_economy = NULL,
    reference_power = NULL,
    reference_speed = NULL
  )

  if (modality == "cycling") {
    # Calculate gross efficiency
    if (!is.null(ref_data$power_w) && !is.na(ref_data$power_w) && ref_data$power_w > 0) {
      vo2_ml <- ref_data$vo2_ml
      power_w <- ref_data$power_w
      rer <- ref_data$rer %||% 0.90

      result$gross_efficiency <- calculate_gross_efficiency(vo2_ml, power_w, rer)
      result$reference_power <- power_w
    } else {
      cli::cli_warn("No power data available at stage {reference_stage}. Cannot calculate gross efficiency.")
    }
  } else if (modality == "treadmill") {
    # Calculate running economy
    # Need speed - check if provided or in stage_summary
    if (is.null(speed_kmh)) {
      if ("speed_kmh" %in% names(ref_data) && !is.na(ref_data$speed_kmh)) {
        speed_kmh <- ref_data$speed_kmh
      } else if (!is.null(analysis@protocol_config)) {
        # Calculate speed from protocol config
        pc <- analysis@protocol_config
        if (!is.null(pc@starting_intensity) && !is.null(pc@increment_size)) {
          speed_kmh <- pc@starting_intensity + (reference_stage - 1) * pc@increment_size
        }
      }
    }

    if (!is.null(speed_kmh) && speed_kmh > 0) {
      # Get VO2/kg
      weight_kg <- analysis@data@participant@weight_kg
      vo2_kg <- ref_data$vo2_ml / weight_kg

      result$running_economy <- calculate_running_economy(vo2_kg, speed_kmh)
      result$reference_speed <- speed_kmh
    } else {
      cli::cli_warn("No speed data available. Cannot calculate running economy.")
    }
  }

  # Create and return EconomyMetrics object
  EconomyMetrics(
    modality = result$modality,
    gross_efficiency = result$gross_efficiency,
    running_economy = result$running_economy,
    reference_stage = result$reference_stage,
    reference_speed = result$reference_speed,
    reference_power = result$reference_power
  )
}

#' Run the standard CPET analysis pipeline
#'
#' @description
#' Orchestrates averaging, peak detection, ventilatory thresholds, stage
#' extraction, and Phase-1 metrics into a `CpetAnalysis` object. This is the
#' programmatic counterpart of the Shiny Results tab pipeline.
#'
#' @param data A `CpetData` object from `read_cpet()` / `read_cosmed()`.
#' @param settings Optional named list of analysis settings. Recognised keys:
#'   `averaging_method`, `averaging_window`, `threshold_methods`, `protocol`,
#'   `stage_duration`, `increment_size`, `modality`, `starting_intensity`,
#'   `athlete_sport`, `athlete_level`. Missing keys use package defaults.
#' @param participant Optional `Participant` object that overrides the one
#'   stored on `data`.
#' @param threshold_override Optional list with `vt1_vo2` and/or `vt2_vo2`
#'   for a manual threshold correction.
#'
#' @return A `CpetAnalysis` object.
#'
#' @examples
#' \dontrun{
#' data <- read_cpet("path/to/export.xlsx")
#' analysis <- analyze_cpet(data)
#' plot_cpet_panel(analysis)
#' generate_report(analysis, "report.pdf", ReportConfig())
#' }
#'
#' @export
analyze_cpet <- function(
  data,
  settings = list(),
  participant = NULL,
  threshold_override = NULL
) {
  if (!S7::S7_inherits(data, CpetData)) {
    cli::cli_abort("{.arg data} must be a {.cls CpetData} object.")
  }

  s <- list(
    averaging_method = settings$averaging_method %||% "rolling",
    averaging_window = settings$averaging_window %||% 30,
    threshold_methods = settings$threshold_methods %||%
      c("v_slope", "ve_vo2", "ve_vco2"),
    protocol = settings$protocol %||% "ramp",
    stage_duration = settings$stage_duration %||% 60,
    increment_size = settings$increment_size %||% 25,
    modality = settings$modality %||% "cycling",
    starting_intensity = settings$starting_intensity %||% 0,
    athlete_sport = settings$athlete_sport %||% "cycling",
    athlete_level = settings$athlete_level %||% "recreational"
  )

  if (!is.null(participant)) {
    data@participant <- participant
  }
  p <- data@participant

  data_avg <- if (isTRUE(data@is_averaged)) {
    data
  } else {
    average(data, method = s$averaging_method, window = s$averaging_window)
  }
  if (!is.null(participant)) {
    data_avg@participant <- participant
  }

  peaks <- find_peaks(data_avg, averaging = s$averaging_window)

  thresholds <- NULL
  if (length(s$threshold_methods) > 0) {
    thresholds <- tryCatch(
      detect_thresholds(
        data_avg,
        methods = s$threshold_methods,
        window_s = s$averaging_window
      ),
      error = function(e) {
        cli::cli_warn("Threshold detection failed: {e$message}")
        NULL
      }
    )
  }

  if (!is.null(threshold_override)) {
    thresholds <- apply_threshold_override(
      thresholds,
      threshold_override,
      data_avg
    )
  }

  stage_summary <- NULL
  stages_tbl <- NULL
  tryCatch(
    {
      data_with_stages <- extract_stages(
        data_avg,
        protocol = s$protocol,
        stage_duration = s$stage_duration,
        increment = s$increment_size
      )
      stage_summary <- summarize_stages(
        data_with_stages,
        window_s = s$averaging_window
      )
      stages_tbl <- tryCatch(data_with_stages@stages, error = function(e) NULL)
      data_avg@stages <- stages_tbl
      data_avg@breaths <- data_with_stages@breaths
    },
    error = function(e) {
      cli::cli_warn("Stage extraction failed: {e$message}")
    }
  )

  protocol_config <- tryCatch(
    ProtocolConfig(
      modality = s$modality,
      starting_intensity = s$starting_intensity,
      increment_size = s$increment_size,
      stage_duration_s = s$stage_duration
    ),
    error = function(e) NULL
  )

  analysis_obj <- CpetAnalysis(
    data = data_avg,
    peaks = peaks,
    thresholds = thresholds,
    stage_summary = stage_summary,
    protocol_config = protocol_config
  )

  populate_phase1_metrics(
    analysis_obj,
    stage_summary = stage_summary,
    breath_df = data_avg@breaths,
    participant = p,
    settings = s,
    stages = stages_tbl
  )
}

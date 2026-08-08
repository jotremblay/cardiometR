#' Session save/load helpers for the Shiny app
#'
#' Persist a local analysis session (CpetData + settings + overrides) so an
#' operator can resume without re-importing the metabolic-cart file.
#'
#' @keywords internal
#' @name session-cache
NULL

session_cache_dir <- function() {
  dir <- tools::R_user_dir("cardiometR", which = "cache")
  sessions <- file.path(dir, "sessions")
  if (!dir.exists(sessions)) {
    dir.create(sessions, recursive = TRUE, showWarnings = FALSE)
  }
  sessions
}

#' Default path for the latest session snapshot
#' @keywords internal
session_cache_default_path <- function() {
  file.path(session_cache_dir(), "latest_session.rds")
}

#' Save a Shiny analysis session to RDS
#'
#' @param path File path ending in `.rds`.
#' @param cpet_data A `CpetData` object (required).
#' @param settings Named list of analysis settings.
#' @param participant Optional `Participant` object.
#' @param threshold_override Optional threshold override list.
#' @param analysis Optional `CpetAnalysis` object.
#' @return Invisibly, `path`.
#' @keywords internal
save_analysis_session <- function(
  path,
  cpet_data,
  settings = list(),
  participant = NULL,
  threshold_override = NULL,
  analysis = NULL
) {
  if (is.null(cpet_data) || !S7::S7_inherits(cpet_data, CpetData)) {
    cli::cli_abort("Cannot save session without a {.cls CpetData} object.")
  }
  payload <- list(
    version = 1L,
    saved_at = Sys.time(),
    cpet_data = cpet_data,
    settings = settings %||% list(),
    participant = participant,
    threshold_override = threshold_override,
    analysis = analysis
  )
  dir <- dirname(path)
  if (!dir.exists(dir)) {
    dir.create(dir, recursive = TRUE, showWarnings = FALSE)
  }
  saveRDS(payload, path)
  invisible(path)
}

#' Load a Shiny analysis session from RDS
#'
#' @param path File path to a session RDS written by `save_analysis_session()`.
#' @return A named list with session fields.
#' @keywords internal
load_analysis_session <- function(path) {
  if (!file.exists(path)) {
    cli::cli_abort("Session file not found: {.file {path}}")
  }
  payload <- readRDS(path)
  if (!is.list(payload) || is.null(payload$cpet_data)) {
    cli::cli_abort("Invalid session file: missing {.field cpet_data}.")
  }
  if (!S7::S7_inherits(payload$cpet_data, CpetData)) {
    cli::cli_abort(
      "Invalid session file: {.field cpet_data} is not {.cls CpetData}."
    )
  }
  list(
    cpet_data = payload$cpet_data,
    settings = payload$settings %||% list(),
    participant = payload$participant,
    threshold_override = payload$threshold_override,
    analysis = payload$analysis,
    saved_at = payload$saved_at
  )
}

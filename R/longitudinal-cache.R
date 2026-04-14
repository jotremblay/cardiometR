#' Longitudinal cache helpers
#'
#' A small, opt-in file-backed cache for tracking longitudinal CPET summaries
#' per participant. Keys are hashed (sha256 via `rlang::hash()`) so raw
#' participant identifiers do not cross process boundaries. Only a minimal set
#' of non-PII fields is persisted.
#'
#' @keywords internal
#' @name longitudinal-cache
NULL

longitudinal_cache_path <- function() {
  dir <- tools::R_user_dir("cardiometR", which = "cache")
  if (!dir.exists(dir)) {
    dir.create(dir, recursive = TRUE, showWarnings = FALSE)
  }
  file.path(dir, "longitudinal.rds")
}

longitudinal_hash <- function(participant_id) {
  if (is.null(participant_id) || !nzchar(as.character(participant_id))) {
    return(NA_character_)
  }
  rlang::hash(as.character(participant_id))
}

#' Read cached longitudinal summaries for a participant.
#'
#' @param participant_id A character participant identifier (plain text; hashed
#'   internally before lookup).
#' @return A tibble of prior summary rows, or `NULL` if no cache or no match.
#' @keywords internal
longitudinal_cache_read <- function(participant_id) {
  key <- longitudinal_hash(participant_id)
  if (is.na(key)) return(NULL)
  path <- longitudinal_cache_path()
  if (!file.exists(path)) return(NULL)
  store <- tryCatch(readRDS(path), error = function(e) NULL)
  if (is.null(store) || !is.list(store)) return(NULL)
  store[[key]]
}

#' Append a summary row to the cache, deduplicating by timestamp.
#'
#' @param participant_id A character participant identifier.
#' @param summary_row A named list or 1-row tibble with fields `vo2_peak`,
#'   `map_per_kg`, `ppo`, `weight_kg`, `age`, `sex`, `timestamp`.
#' @return Invisibly, the updated tibble for the participant.
#' @keywords internal
longitudinal_cache_write <- function(participant_id, summary_row) {
  key <- longitudinal_hash(participant_id)
  if (is.na(key)) return(invisible(NULL))
  row <- tibble::as_tibble(as.list(summary_row))
  path <- longitudinal_cache_path()
  store <- if (file.exists(path)) {
    tryCatch(readRDS(path), error = function(e) list())
  } else list()
  if (!is.list(store)) store <- list()

  prior <- store[[key]]
  combined <- if (is.null(prior)) row else dplyr::bind_rows(prior, row)
  combined <- combined |>
    dplyr::distinct(.data$timestamp, .keep_all = TRUE) |>
    dplyr::arrange(.data$timestamp)

  store[[key]] <- combined
  tryCatch(saveRDS(store, path), error = function(e) {
    cli::cli_warn("Longitudinal cache write failed: {e$message}")
  })
  invisible(combined)
}

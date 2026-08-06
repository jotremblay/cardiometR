#' Compute Resting Values from the Leading Rest Block
#'
#' @description
#' Summarises the last `window_s` seconds of the leading seated-rest period
#' (breaths tagged `stage == 0` by [extract_stages()]) into descriptive means
#' for VO2, VCO2, VE, HR and RER. These are **not** a formal basal metabolic
#' rate: the subject is on the ergometer and not in a fasted supine state.
#' Returns `NULL` when no leading rest block is present or when it is shorter
#' than 20 s.
#'
#' @param data_avg A `CpetData` object (typically after `average()`), carrying
#'   `@breaths`, `@stages`, `@participant`, and `@averaging_window`.
#' @param stages Optional stages tibble with `time_s` and `stage` columns. When
#'   `NULL` (default), `data_avg@stages` is used.
#' @param window_s Target averaging window at the end of the rest block, in
#'   seconds. Capped to the actual rest-block duration. Default: 60.
#'
#' @return A named list with `duration_s`, `window_s`, `n_breaths`,
#'   `vo2_rest`, `vo2_kg_rest`, `vco2_rest`, `ve_rest`, `hr_rest`,
#'   `rer_rest`, and `averaging_s`. Returns `NULL` when no usable rest block
#'   is detected.
#'
#' @examples
#' \dontrun{
#'   analysis@resting <- compute_resting_values(analysis@data)
#' }
#' @export
compute_resting_values <- function(data_avg, stages = NULL, window_s = 60) {
  if (is.null(data_avg)) return(NULL)
  breaths <- tryCatch(data_avg@breaths, error = function(e) NULL)
  if (is.null(breaths) || !nrow(breaths) || !("time_s" %in% names(breaths))) {
    return(NULL)
  }

  if (!"stage" %in% names(breaths)) {
    if (is.null(stages)) stages <- tryCatch(data_avg@stages, error = function(e) NULL)
    if (is.null(stages) || !("stage" %in% names(stages)) ||
        !("time_s" %in% names(stages)) || !nrow(stages)) {
      return(NULL)
    }
    so <- order(stages$time_s)
    st <- stages$time_s[so]
    sv <- stages$stage[so]
    idx <- findInterval(breaths$time_s, st)
    idx[idx == 0] <- 1
    idx[idx > length(st)] <- length(st)
    breaths$stage <- sv[idx]
  }

  # Use the recorded phase when there is one. Stage 0 is not the rest period:
  # extract_stages() gives stage 0 to rest, warmup and recovery alike, so the
  # last minute of the leading stage-0 block is the end of the WARMUP. Reading
  # it as rest reports a resting VO2 several times too high.
  rest_rows <- if (has_rest_phase(breaths)) {
    breaths |> dplyr::filter(!is.na(.data$phase), .data$phase == "rest")
  } else {
    breaths |> dplyr::filter(!is.na(.data$stage), .data$stage == 0)
  }
  ex_rows <- breaths |> dplyr::filter(!is.na(.data$stage), .data$stage > 0)
  if (nrow(rest_rows) == 0 || nrow(ex_rows) == 0) return(NULL)

  ex_start <- min(ex_rows$time_s, na.rm = TRUE)
  leading  <- rest_rows |> dplyr::filter(.data$time_s < ex_start)
  if (nrow(leading) == 0) return(NULL)

  duration_s <- max(leading$time_s) - min(leading$time_s)
  if (!is.finite(duration_s) || duration_s < 20) return(NULL)

  actual_window <- min(window_s, duration_s)
  cutoff <- max(leading$time_s) - actual_window
  win <- leading |> dplyr::filter(.data$time_s >= cutoff)
  if (nrow(win) < 3) return(NULL)

  wt <- tryCatch(data_avg@participant@weight_kg, error = function(e) NA_real_)
  aw <- tryCatch(data_avg@averaging_window, error = function(e) NA_real_)

  mean_na <- function(x) if (all(is.na(x))) NA_real_ else mean(x, na.rm = TRUE)
  vo2  <- if ("vo2_ml"  %in% names(win)) mean_na(win$vo2_ml)  else NA_real_
  vco2 <- if ("vco2_ml" %in% names(win)) mean_na(win$vco2_ml) else NA_real_
  ve   <- if ("ve_l"    %in% names(win)) mean_na(win$ve_l)    else NA_real_
  rer  <- if ("rer"     %in% names(win)) mean_na(win$rer)     else NA_real_
  hr   <- if ("hr_bpm"  %in% names(win)) mean_na(win$hr_bpm)  else NA_real_

  list(
    duration_s  = duration_s,
    window_s    = actual_window,
    n_breaths   = nrow(win),
    vo2_rest    = vo2,
    vo2_kg_rest = if (is.numeric(wt) && length(wt) == 1 && is.finite(wt) && wt > 0) vo2 / wt else NA_real_,
    vco2_rest   = vco2,
    ve_rest     = ve,
    hr_rest     = hr,
    rer_rest    = rer,
    averaging_s = if (is.numeric(aw) && length(aw) == 1) aw else NA_real_
  )
}

#' Does an Analysis Carry Resting Values?
#'
#' @param analysis A `CpetAnalysis`.
#' @return `TRUE` when `analysis@resting` is a non-empty list.
#' @export
has_resting <- function(analysis) {
  v <- tryCatch(analysis@resting, error = function(e) NULL)
  !is.null(v) && is.list(v) && length(v) > 0
}

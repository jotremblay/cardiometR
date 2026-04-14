# Steady-state gating for stage-level substrate oxidation (Achten & Jeukendrup 2004)

#' Per-Stage Steady-State Check
#'
#' @description
#' For each stage in a CPET, evaluate whether the stage satisfies the
#' steady-state assumption required for indirect-calorimetry substrate
#' oxidation: stage duration >= 3 min, HR drift < 3 bpm, RER drift < 0.03
#' across the last 90 s, and sub-unity RER (max RER < 1.0 in the last 90 s).
#'
#' HR drift is computed as `mean(HR[last 30s]) - mean(HR[prior 60s])`
#' within each stage; RER drift is the same for RER.
#'
#' @param breath_df Breath-by-breath (or averaged) tibble with `time_s`,
#'   `stage`, `hr_bpm` (optional), and `rer` columns.
#' @param stage_summary Stage summary tibble keyed on `stage` (used to
#'   align with the caller's stage identifiers).
#' @return A tibble keyed on `stage` with columns `duration_ok`,
#'   `hr_drift_bpm`, `rer_drift`, `rer_sub_unity`, `steady_state_ok`.
#' @references Achten J, Jeukendrup AE. 2004. Optimizing fat oxidation
#'   through exercise and diet. Nutrition 20:716-727.
#' @export
check_steady_state <- function(breath_df, stage_summary) {
  stopifnot(inherits(breath_df, "data.frame"))
  required <- c("time_s", "stage", "rer")
  if (!all(required %in% names(breath_df))) {
    return(tibble::tibble(
      stage = integer(), duration_ok = logical(),
      hr_drift_bpm = double(), rer_drift = double(),
      rer_sub_unity = logical(), steady_state_ok = logical()
    ))
  }

  has_hr <- "hr_bpm" %in% names(breath_df)

  breath_df |>
    dplyr::filter(!is.na(.data$stage), .data$stage > 0) |>
    dplyr::group_by(.data$stage) |>
    dplyr::group_modify(~ {
      df <- .x
      t_max <- max(df$time_s, na.rm = TRUE)
      t_min <- min(df$time_s, na.rm = TRUE)
      duration_s <- t_max - t_min

      last30 <- dplyr::filter(df, .data$time_s >= t_max - 30)
      prior60 <- dplyr::filter(df,
        .data$time_s >= t_max - 90,
        .data$time_s <  t_max - 30
      )
      last90 <- dplyr::filter(df, .data$time_s >= t_max - 90)

      hr_drift <- if (has_hr && nrow(last30) > 0 && nrow(prior60) > 0) {
        mean(last30$hr_bpm, na.rm = TRUE) - mean(prior60$hr_bpm, na.rm = TRUE)
      } else {
        NA_real_
      }
      rer_drift <- if (nrow(last30) > 0 && nrow(prior60) > 0) {
        mean(last30$rer, na.rm = TRUE) - mean(prior60$rer, na.rm = TRUE)
      } else {
        NA_real_
      }
      rer_max_last90 <- if (nrow(last90) > 0) max(last90$rer, na.rm = TRUE) else NA_real_

      duration_ok  <- isTRUE(duration_s >= 180)
      hr_ok        <- isTRUE(abs(hr_drift) < 3)
      rer_drift_ok <- isTRUE(abs(rer_drift) < 0.03)
      rer_sub     <- isTRUE(rer_max_last90 < 1.0)

      tibble::tibble(
        duration_ok = duration_ok,
        hr_drift_bpm = hr_drift,
        rer_drift = rer_drift,
        rer_sub_unity = rer_sub,
        steady_state_ok = duration_ok && hr_ok && rer_drift_ok && rer_sub
      )
    }) |>
    dplyr::ungroup()
}

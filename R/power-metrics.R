# Power metrics: Kuipers MAP/PPO and VO2-Power slope

#' Compute Kuipers (1985) MAP / PPO from a Stage Summary
#'
#' @description
#' Computes Maximal Aerobic Power (MAP) and Peak Power Output (PPO) from a
#' ramp or incremental CPET using the Kuipers (1985) last-stage adjustment:
#' `PPO = P_last_complete + (t_final / t_stage) * P_increment`. When the
#' last stage is completed fully, `map == ppo == power of last complete stage`.
#'
#' @param stage_summary A tibble as produced by [summarize_stages()] with at
#'   least `stage`, `duration_s`, and `power_w` columns.
#' @return A one-row tibble with `map_watts`, `ppo_watts`, `kuipers_fraction`,
#'   and `last_stage_fraction`.
#' @references Kuipers H et al. 1985. Variability of aerobic performance.
#'   Int J Sports Med 6:197-201.
#' @export
compute_map_kuipers <- function(stage_summary) {
  stopifnot(inherits(stage_summary, "data.frame"))
  if (!all(c("duration_s", "power_w") %in% names(stage_summary)) ||
      nrow(stage_summary) == 0) {
    return(tibble::tibble(
      map_watts = NA_real_, ppo_watts = NA_real_,
      kuipers_fraction = NA_real_, last_stage_fraction = NA_real_
    ))
  }

  ss <- stage_summary |>
    dplyr::filter(!is.na(.data$power_w), !is.na(.data$duration_s)) |>
    dplyr::arrange(dplyr::across(dplyr::any_of("stage")))

  if (nrow(ss) == 0) {
    return(tibble::tibble(
      map_watts = NA_real_, ppo_watts = NA_real_,
      kuipers_fraction = NA_real_, last_stage_fraction = NA_real_
    ))
  }

  # Typical stage duration = median of non-last stages (fallback: overall median)
  typical_stage <- if (nrow(ss) >= 2) {
    stats::median(ss$duration_s[-nrow(ss)], na.rm = TRUE)
  } else {
    stats::median(ss$duration_s, na.rm = TRUE)
  }
  if (!is.finite(typical_stage) || typical_stage <= 0) {
    typical_stage <- stats::median(ss$duration_s, na.rm = TRUE)
  }

  last <- ss[nrow(ss), ]
  fraction <- if (is.finite(typical_stage) && typical_stage > 0) {
    min(1, last$duration_s / typical_stage)
  } else {
    1
  }

  if (fraction >= 0.98 || nrow(ss) < 2) {
    # Last stage essentially complete — MAP == PPO == last complete stage
    map_w <- last$power_w
    ppo_w <- last$power_w
  } else {
    prev <- ss[nrow(ss) - 1, ]
    increment <- last$power_w - prev$power_w
    map_w <- prev$power_w
    ppo_w <- prev$power_w + fraction * increment
  }

  tibble::tibble(
    map_watts = map_w,
    ppo_watts = ppo_w,
    kuipers_fraction = fraction,
    last_stage_fraction = fraction
  )
}


#' Fit VO2-Power Linear Slope (Submax Portion)
#'
#' @description
#' Linear regression of VO2 (mL/min) on Power (W) using the submax portion
#' of the test only — up to VT2 power if available, else 85% of PPO. Returns
#' slope, intercept, 95% CI, `n`, and `r_squared`.
#'
#' @param breath_df Breath-by-breath or averaged tibble with `vo2_ml` and
#'   `power_w` columns.
#' @param stage_summary Stage summary tibble (for PPO fallback via
#'   [compute_map_kuipers()]).
#' @param vt2_power Optional numeric: VT2 power in watts. When `NULL`, 85%
#'   of PPO is used as the submax cutoff.
#' @return A list with `slope`, `intercept`, `slope_ci_low`, `slope_ci_high`,
#'   `n`, `r_squared`.
#' @export
fit_vo2_power_slope <- function(breath_df, stage_summary, vt2_power = NULL) {
  stopifnot(inherits(breath_df, "data.frame"))
  if (!all(c("vo2_ml", "power_w") %in% names(breath_df))) {
    return(list(slope = NA_real_, intercept = NA_real_,
                slope_ci_low = NA_real_, slope_ci_high = NA_real_,
                n = 0L, r_squared = NA_real_))
  }

  cutoff <- vt2_power
  if (is.null(cutoff) || !is.finite(cutoff)) {
    mp <- compute_map_kuipers(stage_summary)
    cutoff <- if (is.finite(mp$ppo_watts)) 0.85 * mp$ppo_watts else NA_real_
  }

  df <- breath_df |>
    dplyr::select("vo2_ml", "power_w") |>
    dplyr::filter(!is.na(.data$vo2_ml), !is.na(.data$power_w),
                  .data$power_w > 0)
  if (is.finite(cutoff)) {
    df <- dplyr::filter(df, .data$power_w <= cutoff)
  }

  if (nrow(df) < 5) {
    return(list(slope = NA_real_, intercept = NA_real_,
                slope_ci_low = NA_real_, slope_ci_high = NA_real_,
                n = nrow(df), r_squared = NA_real_))
  }

  fit <- stats::lm(vo2_ml ~ power_w, data = df)
  ci <- tryCatch(stats::confint(fit, "power_w", level = 0.95),
                 error = function(e) matrix(c(NA_real_, NA_real_), nrow = 1))
  coefs <- stats::coef(fit)

  list(
    slope = unname(coefs["power_w"]),
    intercept = unname(coefs["(Intercept)"]),
    slope_ci_low = unname(ci[1, 1]),
    slope_ci_high = unname(ci[1, 2]),
    n = nrow(df),
    r_squared = summary(fit)$r.squared
  )
}


#' Populate Phase-1 Additive Metrics on a CpetAnalysis
#'
#' @description
#' Internal helper: computes MAP/PPO (Kuipers), MAP/kg, VO2-Power slope,
#' steady-state stage flags, and vo2_peak / map_per_kg / ppo z-scores
#' against the participant's stratum, and assigns them to the analysis
#' object's additive slots. Each computation is wrapped in `tryCatch` so
#' any single failure downgrades to `NULL`.
#'
#' @param analysis A CpetAnalysis S7 object.
#' @param stage_summary Stage summary tibble (may be NULL).
#' @param breath_df Averaged or breath-by-breath tibble (may be NULL).
#' @param participant A Participant S7 object or NULL.
#' @param settings Shiny settings list (for sport / level selection).
#' @return The updated CpetAnalysis.
#' @keywords internal
populate_phase1_metrics <- function(analysis, stage_summary, breath_df,
                                    participant = NULL, settings = list()) {
  map_res <- tryCatch(
    if (!is.null(stage_summary)) compute_map_kuipers(stage_summary) else NULL,
    error = function(e) { cli::cli_warn("MAP/PPO computation failed: {e$message}"); NULL }
  )
  if (!is.null(map_res) && is.finite(map_res$map_watts)) {
    analysis@map_watts <- map_res$map_watts
    analysis@ppo_watts <- map_res$ppo_watts
    analysis@kuipers_fraction <- map_res$kuipers_fraction
    if (!is.null(participant)) {
      wt <- tryCatch(participant@weight_kg, error = function(e) NULL)
      if (is.numeric(wt) && is.finite(wt) && wt > 0) {
        analysis@map_per_kg <- map_res$map_watts / wt
      }
    }
  }

  analysis@vo2_power_slope <- tryCatch(
    if (!is.null(breath_df)) fit_vo2_power_slope(breath_df, stage_summary) else NULL,
    error = function(e) { cli::cli_warn("VO2-Power slope fit failed: {e$message}"); NULL }
  )

  analysis <- populate_threshold_ranges(analysis, breath_df)

  analysis@steady_state_stages <- tryCatch(
    if (!is.null(breath_df) && !is.null(stage_summary)) {
      check_steady_state(breath_df, stage_summary)
    } else NULL,
    error = function(e) { cli::cli_warn("Steady-state check failed: {e$message}"); NULL }
  )

  analysis@z_scores <- tryCatch({
    sport_in <- settings$athlete_sport %||% settings$sport
    if (is.null(sport_in) || !nzchar(sport_in) || sport_in == "general") {
      sport <- "general"
      level <- "recreational"
    } else {
      sport <- sport_in
      level <- settings$athlete_level %||% settings$level %||% "recreational"
    }
    sex <- tryCatch(as.character(participant@sex), error = function(e) "M")
    age <- tryCatch(as.numeric(participant@age), error = function(e) 30)
    if (!length(sex) || is.na(sex)) sex <- "M"
    if (!length(age) || is.na(age)) age <- 30
    stratum <- get_normative_data(sport = sport, level = level, sex = sex, age = age)

    vo2_peak_val <- tryCatch(analysis@peaks@vo2_kg_peak, error = function(e) NA_real_)
    list(
      vo2_peak_z   = z_score(vo2_peak_val,         stratum, metric = "vo2max"),
      map_per_kg_z = z_score(analysis@map_per_kg %||% NA_real_, stratum, metric = "map_per_kg"),
      ppo_z        = z_score(analysis@ppo_watts   %||% NA_real_, stratum, metric = "map_per_kg")
    )
  }, error = function(e) { cli::cli_warn("Z-score computation failed: {e$message}"); NULL })

  analysis
}

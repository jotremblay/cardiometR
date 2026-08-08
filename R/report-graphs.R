# Report graph generation and cache for cardiometR

#' Generate Report Graphs
#'
#' @description
#' Creates temporary graph files for inclusion in the report.
#'
#' @param analysis CpetAnalysis object
#' @param language Language code
#' @param athlete_sport Sport for normative comparison (optional)
#' @param athlete_level Competitive level
#' @param prediction_source Prediction equation source: "jones" or "prefaut"
#' @return List with graph file paths
#' @keywords internal
generate_report_graphs <- function(analysis, language = "en",
                                   athlete_sport = NULL, athlete_level = "recreational",
                                   prediction_source = "jones") {
  if (!requireNamespace("patchwork", quietly = TRUE)) {
    cli::cli_abort(c(
      "The {.pkg patchwork} package is required to generate the 9-panel report plot",
      "i" = "Install it with {.code install.packages('patchwork')}"
    ))
  }

  cache_key <- build_report_graph_cache_key(
    analysis,
    language,
    athlete_sport,
    athlete_level
  )

  cached <- !is.null(report_graph_cache$key) &&
    identical(report_graph_cache$key, cache_key) &&
    is.list(report_graph_cache$plots)

  if (!cached) {
    p_panel <- plot_cpet_panel(analysis, language = language)
    p_vslope <- plot_v_slope(analysis, language = language)
    p_predicted <- plot_predicted_comparison(
      analysis,
      sport = athlete_sport,
      level = athlete_level,
      language = language,
      show_citation = TRUE,
      prediction_source = prediction_source
    )

    report_graph_cache$key <- cache_key
    report_graph_cache$plots <- list(
      panel = p_panel,
      vslope = p_vslope,
      predicted = p_predicted
    )
  }

  plots <- report_graph_cache$plots

  # Generate 9-panel plot
  panel_file <- tempfile("cpet_panel_", fileext = ".png")
  ggplot2::ggsave(panel_file, plots$panel, width = 10, height = 10, dpi = 150)

  # Generate V-slope plot
  vslope_file <- tempfile("vslope_", fileext = ".png")
  ggplot2::ggsave(vslope_file, plots$vslope, width = 6, height = 5, dpi = 150)

  # Generate predicted comparison plot (with optional athlete norms)
  predicted_file <- tempfile("predicted_comparison_", fileext = ".png")
  ggplot2::ggsave(predicted_file, plots$predicted, width = 10, height = 5, dpi = 150)

  out <- list(
    graph_panel = panel_file,
    graph_vslope = vslope_file,
    graph_predicted = predicted_file
  )

  # Phase 7: VO2-power slope, z-score strip, longitudinal delta
  try_save <- function(expr, name, w = 7, h = 3.5) {
    p <- tryCatch(expr, error = function(e) NULL)
    if (is.null(p)) return(NULL)
    f <- tempfile(paste0(name, "_"), fileext = ".png")
    tryCatch({
      ggplot2::ggsave(f, p, width = w, height = h, dpi = 200)
      f
    }, error = function(e) NULL)
  }

  slope_file <- try_save(plot_vo2_power_slope(analysis, language = language),
                         "vo2_power_slope")
  if (!is.null(slope_file)) out$graph_slope <- slope_file

  zstrip_file <- try_save(plot_zscore_strip(analysis, language = language),
                          "zscore_strip", w = 7, h = 3)
  if (!is.null(zstrip_file)) out$graph_zstrip <- zstrip_file

  pid <- tryCatch(analysis@data@participant@id, error = function(e) "")
  prior <- tryCatch(longitudinal_cache_read(pid), error = function(e) NULL)
  if (is.data.frame(prior) && nrow(prior) > 0) {
    long_file <- try_save(
      plot_longitudinal_delta(analysis, prior, language = language),
      "longitudinal_delta", w = 7, h = 3.5)
    if (!is.null(long_file)) out$graph_longitudinal <- long_file
  }

  out
}

# ---- Phase 7 helpers --------------------------------------------------------

# Build template data for athlete-profile, longitudinal and estimates-&-caveats
# sections. Returns a list merged into the main template data.
build_phase7_template_data <- function(analysis, language, report_sections,
                                       athlete_sport = NULL,
                                       athlete_level = "recreational") {
  out <- list()
  want <- function(key) is.null(report_sections) || key %in% report_sections

  has_ap <- want("athlete_profile")
  out$has_athlete_profile <- has_ap

  peaks <- analysis@peaks
  participant <- analysis@data@participant
  wt <- tryCatch(participant@weight_kg, error = function(e) NA_real_)

  vo2_kg <- tryCatch({
    if (!is.null(peaks@vo2_kg_peak) && is.finite(peaks@vo2_kg_peak)) peaks@vo2_kg_peak
    else if (!is.null(peaks@vo2_peak) && is.finite(wt) && wt > 0) peaks@vo2_peak / wt
    else NA_real_
  }, error = function(e) NA_real_)

  zs <- analysis@z_scores %||% list()
  fmt_num <- function(x, d = 1) {
    if (is.numeric(x) && length(x) == 1 && !is.na(x) && is.finite(x))
      formatC(x, digits = d, format = "f") else "--"
  }
  fmt_int <- function(x) {
    if (is.numeric(x) && length(x) == 1 && !is.na(x) && is.finite(x))
      as.character(round(x)) else "--"
  }
  fmt_z   <- function(entry) {
    z <- if (is.list(entry)) entry$z else entry
    if (!is.numeric(z) || !is.finite(z)) return("--")
    formatC(as.numeric(z), digits = 2, format = "f")
  }
  fmt_pct <- function(entry) {
    p <- if (is.list(entry)) entry$percentile else NA_real_
    if (!is.numeric(p) || !is.finite(p)) return("--")
    paste0(round(as.numeric(p)), "%")
  }

  out$ap_vo2_kg        <- fmt_num(vo2_kg, 1)
  out$ap_map_kg        <- fmt_num(analysis@map_per_kg %||% NA_real_, 2)
  out$ap_ppo           <- fmt_int(analysis@ppo_watts %||% NA_real_)
  out$ap_kuipers       <- fmt_num(analysis@kuipers_fraction %||% NA_real_, 2)
  out$ap_vo2_z         <- fmt_z(zs$vo2_peak_z)
  out$ap_vo2_pct       <- fmt_pct(zs$vo2_peak_z)
  out$ap_map_z         <- fmt_z(zs$map_per_kg_z)
  out$ap_map_pct       <- fmt_pct(zs$map_per_kg_z)
  out$ap_ppo_z         <- fmt_z(zs$ppo_z)
  out$ap_ppo_pct       <- fmt_pct(zs$ppo_z)

  # Modality-aware card set: card1 always VO2/kg. Cards 2 and 3 depend on
  # modality so the same Typst block renders meaningful values on both
  # cycling (MAP/kg, PPO) and treadmill (peak speed, peak HR).
  modality <- tryCatch(analysis@protocol_config@modality, error = function(e) NULL)
  is_treadmill <- identical(modality, "treadmill")

  out$ap_card1_label <- escape_typst(tr("aerobic_capacity", language))
  out$ap_card1_value <- out$ap_vo2_kg
  out$ap_card1_unit  <- "mL/kg/min"
  out$ap_card1_zline <- sprintf("%s %s \u00b7 %s",
                                tr("z_score", language),
                                out$ap_vo2_z, out$ap_vo2_pct)

  if (is_treadmill) {
    speed_peak <- tryCatch(peaks@speed_peak, error = function(e) NA_real_)
    hr_peak    <- tryCatch(peaks@hr_peak, error = function(e) NA_real_)
    out$ap_card2_label <- escape_typst(tr("peak_speed", language))
    out$ap_card2_value <- fmt_num(speed_peak, 1)
    out$ap_card2_unit  <- tr("unit_kmh", language)
    out$ap_card2_zline <- ""
    out$ap_card3_label <- escape_typst(tr("peak_hr", language))
    out$ap_card3_value <- fmt_int(hr_peak)
    out$ap_card3_unit  <- tr("unit_bpm", language)
    out$ap_card3_zline <- ""
  } else {
    out$ap_card2_label <- escape_typst(tr("aerobic_power", language))
    out$ap_card2_value <- out$ap_map_kg
    out$ap_card2_unit  <- "W/kg"
    out$ap_card2_zline <- sprintf("%s %s \u00b7 %s",
                                  tr("z_score", language),
                                  out$ap_map_z, out$ap_map_pct)
    out$ap_card3_label <- escape_typst(tr("peak_power", language))
    out$ap_card3_value <- out$ap_ppo
    out$ap_card3_unit  <- sprintf("W \u00b7 k=%s", out$ap_kuipers)
    out$ap_card3_zline <- sprintf("%s %s \u00b7 %s",
                                  tr("z_score", language),
                                  out$ap_ppo_z, out$ap_ppo_pct)
  }

  # Resting values (gated by athlete_profile section)
  r <- tryCatch(analysis@resting, error = function(e) NULL)
  if (has_ap && is.list(r) && length(r) > 0) {
    dur_s <- as.numeric(r$duration_s %||% NA_real_)
    out$has_resting     <- TRUE
    out$resting_title   <- escape_typst(tr("resting_values_title", language))
    out$resting_vo2     <- fmt_int(r$vo2_rest)
    out$resting_vo2_kg  <- fmt_num(r$vo2_kg_rest, 1)
    out$resting_hr      <- fmt_int(r$hr_rest)
    out$resting_ve      <- fmt_num(r$ve_rest, 1)
    out$resting_rer     <- fmt_num(r$rer_rest, 2)
    out$resting_duration <- if (is.finite(dur_s)) {
      sprintf("%d:%02d", as.integer(dur_s) %/% 60L, as.integer(dur_s) %% 60L)
    } else "--"
    out$resting_duration_label <- escape_typst(tr("resting_rest_duration", language))
    caption_key <- if (is_treadmill) "resting_values_caption_treadmill"
                   else "resting_values_caption_cycling"
    out$resting_caption <- escape_typst(tryCatch(
      sprintf(tr(caption_key, language),
              as.integer(round(r$window_s %||% NA_real_)),
              as.integer(r$n_breaths %||% 0L)),
      error = function(e) ""
    ))
  } else {
    out$has_resting <- FALSE
    out$resting_title <- ""
    out$resting_vo2 <- out$resting_vo2_kg <- out$resting_hr <- "--"
    out$resting_ve <- out$resting_rer <- out$resting_duration <- "--"
    out$resting_duration_label <- ""
    out$resting_caption <- ""
  }

  # VO2-power slope caption
  # Figure titles and captions live in the translation files; the report
  # only sees what is forwarded here.
  for (key in c("section_vo2_power_slope", "section_zscore_strip",
                "caption_vo2_power_slope", "caption_zscore_strip")) {
    out[[key]] <- escape_typst(tr(key, language))
  }

  slope <- analysis@vo2_power_slope
  if (is.list(slope) && !is.null(slope$slope) && length(slope$slope) == 1 &&
      is.numeric(slope$slope) && !is.na(slope$slope) && is.finite(slope$slope)) {
    lo <- slope$slope_ci_low %||% NA_real_
    hi <- slope$slope_ci_high %||% NA_real_
    out$slope_caption <- escape_typst(sprintf(
      "%s: %.2f [%.2f, %.2f] mL/min/W",
      tr("slope_label", language),
      as.numeric(slope$slope),
      as.numeric(lo),
      as.numeric(hi)
    ))
    out$has_slope_caption <- TRUE
  } else {
    out$slope_caption <- ""
    out$has_slope_caption <- FALSE
  }

  # Estimates & caveats
  has_ec <- want("estimates_caveats")
  out$has_estimates_caveats <- has_ec

  # VT ranges table (metric | low | high | point)
  vt1r <- analysis@vt1_range %||% NA_real_
  vt2r <- analysis@vt2_range %||% NA_real_
  th <- analysis@thresholds
  vt1_point <- tryCatch(th@vt1_vo2, error = function(e) NA_real_)
  vt2_point <- tryCatch(th@vt2_vo2, error = function(e) NA_real_)

  build_vt_row <- function(name, rg, pt) {
    lo <- if (length(rg) >= 1) rg[1] else NA_real_
    hi <- if (length(rg) >= 2) rg[2] else NA_real_
    sprintf("[%s], [%s], [%s], [%s]",
      escape_typst(name),
      fmt_int(lo), fmt_int(hi), fmt_int(pt))
  }
  out$vt_rows_content <- paste0(
    build_vt_row("VT1", vt1r, vt1_point), ",\n    ",
    build_vt_row("VT2", vt2r, vt2_point)
  )

  has_vt <- any(is.finite(c(vt1r, vt2r, vt1_point, vt2_point)))
  out$has_vt_block <- has_vt

  # VT-range table i18n labels (must be explicitly forwarded -- tr() keys
  # are not auto-merged into template_data)
  out$estimates_and_caveats <- escape_typst(tr("estimates_and_caveats", language))
  out$vt_range        <- escape_typst(tr("vt_range", language))
  out$vt_range_title  <- escape_typst(tr("vt_range", language))
  out$vt_caveat       <- escape_typst(tr("vt_caveat", language))
  out$metric          <- escape_typst(tr("metric", language))
  out$low             <- escape_typst(tr("low", language))
  out$high            <- escape_typst(tr("high", language))
  out$point           <- escape_typst(tr("point", language))

  # FTP range -- cycling-only (meaningless for treadmill tests)
  map_w <- analysis@map_watts %||% NA_real_
  is_scalar_num <- function(v) is.numeric(v) && length(v) == 1 && !is.na(v) && is.finite(v)
  if (!is_treadmill && is_scalar_num(map_w)) {
    out$ftp_low  <- fmt_int(0.72 * map_w)
    out$ftp_high <- fmt_int(0.77 * map_w)
    out$has_ftp_block <- TRUE
  } else {
    out$ftp_low <- "--"
    out$ftp_high <- "--"
    out$has_ftp_block <- FALSE
  }
  out$ftp_range  <- escape_typst(tr("ftp_range", language))
  out$ftp_caveat <- escape_typst(tr("ftp_caveat", language))

  # CP and substrate explainer text/title -- only render blocks when
  # content is non-empty (avoids blank colored bars on page 3)
  cp_title    <- tr("cp_explainer_title", language)
  cp_text     <- tr("cp_explainer", language)
  sub_title   <- tr("substrate_explainer_title", language)
  sub_text    <- tr("substrate_explainer", language)
  # Cycling-only: CP explainer is about critical power, not meaningful
  # for treadmill; suppress. Substrate explainer applies to both.
  out$cp_explainer_title       <- escape_typst(cp_title)
  out$cp_explainer             <- escape_typst(cp_text)
  out$has_cp_explainer         <- !is_treadmill && nzchar(trimws(cp_text))
  out$substrate_explainer_title <- escape_typst(sub_title)
  out$substrate_explainer      <- escape_typst(sub_text)
  out$has_substrate_explainer  <- nzchar(trimws(sub_text))
  out$stage         <- escape_typst(tr("stage", language))
  out$fat_oxidation <- escape_typst(tr("fat_oxidation", language))
  out$cho_oxidation <- escape_typst(tr("cho_oxidation", language))

  # Substrate: check steady-state stages
  sss <- analysis@steady_state_stages
  sbs <- analysis@substrate_by_stage
  qualifying_rows <- ""
  has_substrate_table <- FALSE
  if (is.data.frame(sss) && "steady_state_ok" %in% names(sss) &&
      any(isTRUE(any(sss$steady_state_ok)), na.rm = TRUE) &&
      is.data.frame(sbs)) {
    ok <- which(isTRUE(sss$steady_state_ok) | sss$steady_state_ok %in% TRUE)
    if (length(ok) > 0 && all(c("fat_pct", "cho_pct") %in% names(sbs))) {
      sub <- sbs[ok, , drop = FALSE]
      rows <- vapply(seq_len(nrow(sub)), function(i) {
        st <- if ("stage" %in% names(sub)) sub$stage[i] else i
        sprintf("[%s], [%s], [%s]",
          escape_typst(as.character(st)),
          fmt_num(sub$fat_pct[i], 0),
          fmt_num(sub$cho_pct[i], 0))
      }, character(1))
      qualifying_rows <- paste(rows, collapse = ",\n    ")
      has_substrate_table <- nzchar(qualifying_rows)
    }
  }
  out$substrate_rows_content <- qualifying_rows
  out$has_substrate_table <- has_substrate_table

  # Longitudinal gate: cache has prior entry
  has_long <- want("longitudinal")
  prior <- tryCatch(longitudinal_cache_read(participant@id %||% ""),
                    error = function(e) NULL)
  out$has_longitudinal <- isTRUE(has_long) && !is.null(prior) &&
    (is.data.frame(prior) && nrow(prior) > 0)

  # Population-norms comparison table (phase: norms)
  pn_want <- want("population_norms")
  out$has_population_norms <- FALSE
  out$pn_section_title    <- escape_typst(tr("section_population_norms", language))
  out$pn_description      <- ""
  out$pn_citation_short   <- ""
  out$pn_sd_note          <- ""
  out$pn_rows_content     <- ""
  out$pn_label_metric     <- escape_typst(tr("metric", language))
  out$pn_label_patient    <- escape_typst(tr("norms_patient", language))
  out$pn_label_mean       <- escape_typst(tr("norms_mean", language))
  out$pn_label_band       <- escape_typst(tr("norms_band", language))
  out$pn_label_zpct       <- escape_typst(tr("norms_zscore_percentile", language))

  if (isTRUE(pn_want)) {
    modality <- tryCatch(analysis@protocol_config@modality,
                         error = function(e) "cycling")
    if (is.null(modality) || !length(modality)) modality <- "cycling"
    sport_for_norms <- if (!is.null(athlete_sport) && nzchar(athlete_sport) &&
                           athlete_sport != "general") {
      athlete_sport
    } else {
      default_sport_for_modality(modality)
    }
    level_for_norms <- athlete_level %||% "recreational"
    stratum <- tryCatch(
      get_normative_data(sport = sport_for_norms, level = level_for_norms,
                         sex = participant@sex, age = participant@age),
      error = function(e) NULL
    )

    # Build a concise, localized stratum label so the FR report doesn't
    # quote the English research-paper description verbatim.
    stratum_localized_desc <- function(sport, level, sex, age, lang) {
      sport_lc <- tolower(sport %||% "general")
      level_lc <- tolower(level %||% "recreational")
      sex_lc <- toupper(sex %||% "M")
      ag <- if (!is.null(age) && is.finite(age)) {
        paste0(10L * (as.integer(age) %/% 10L), "\u2013",
               10L * (as.integer(age) %/% 10L) + 9L, " ",
               if (identical(lang, "fr")) "ans" else "yr")
      } else ""
      sport_tr  <- tr(if (sport_lc %in% c("cycling","running","triathlon","general")) sport_lc else "general", lang)
      level_tr  <- tr(level_lc, lang)
      sex_tr    <- if (sex_lc == "F") {
                      if (identical(lang, "fr")) "femmes" else "women"
                   } else {
                      if (identical(lang, "fr")) "hommes" else "men"
                   }
      paste0(tools::toTitleCase(level_tr), " ", tolower(sport_tr), ", ",
             sex_tr, ", ", ag)
    }

    fmt_val <- function(x, d) {
      if (is.null(x) || length(x) != 1) return("--")
      if (is.na(x) || !is.finite(x)) return("--")
      if (d == 0) as.character(round(as.numeric(x)))
      else formatC(as.numeric(x), digits = d, format = "f")
    }
    fmt_band <- function(low, high, d) {
      if (is.null(low) || is.null(high) || length(low) != 1 || length(high) != 1) return("--")
      if (is.na(low) || is.na(high) || !is.finite(low) || !is.finite(high)) return("--")
      sprintf("%s \u2013 %s", fmt_val(low, d), fmt_val(high, d))
    }
    fmt_zpct <- function(z_entry) {
      if (is.null(z_entry)) return("--")
      z <- if (is.list(z_entry)) z_entry$z else z_entry
      p <- if (is.list(z_entry)) z_entry$percentile else NA_real_
      if (!is.numeric(z) || !is.finite(z)) return("--")
      sprintf("%+.2f \u00b7 p%.0f", as.numeric(z), as.numeric(p))
    }

    rows <- character(0)
    sd_sources <- character(0)
    add_row <- function(label, patient, low, high, mean, z_entry, decimals = 1) {
      rows <<- c(rows, sprintf(
        "[%s], [%s], [%s], [%s], [%s]",
        escape_typst(label),
        fmt_val(patient, decimals),
        fmt_val(mean, decimals),
        fmt_band(low, high, decimals),
        fmt_zpct(z_entry)
      ))
      if (is.list(z_entry) && !is.null(z_entry$sd_source) &&
          !is.na(z_entry$sd_source)) {
        sd_sources <<- unique(c(sd_sources, as.character(z_entry$sd_source)))
      }
    }

    zs <- analysis@z_scores %||% list()
    peaks <- analysis@peaks

    if (!is.null(stratum)) {
      weight_kg <- tryCatch(participant@weight_kg, error = function(e) NA_real_)

      # VO2peak -- always shown, Unicode subscript for VO2
      add_row(
        label = "VO\u2082 (mL/kg/min)",
        patient = tryCatch(peaks@vo2_kg_peak, error = function(e) NA_real_),
        low = stratum$vo2max_low,
        high = stratum$vo2max_high,
        mean = stratum$vo2max_typical,
        z_entry = zs$vo2_peak_z, decimals = 1
      )

      if (identical(modality, "treadmill")) {
        add_row(
          label = paste0(tr("peak_speed", language), " (km/h)"),
          patient = tryCatch(peaks@speed_peak, error = function(e) NA_real_),
          low = NA, high = NA, mean = NA,
          z_entry = zs$speed_peak_z, decimals = 1
        )
        # Running economy at a submaximal reference stage near 70% VO2peak.
        if (!is.null(stratum$economy_typical)) {
          re_val <- NA_real_
          idx <- submax_stage_idx(analysis, peaks, required = "speed_kmh")
          if (!is.na(idx) && is.numeric(weight_kg) && is.finite(weight_kg)) {
            ss <- analysis@stage_summary
            vo2_kg <- ss$vo2_ml[idx] / weight_kg
            re_val <- tryCatch(
              calculate_running_economy(vo2_kg, ss$speed_kmh[idx]),
              error = function(e) NA_real_
            )
          }
          add_row(
            label = paste0(tr("running_economy", language), " (mL/kg/km)"),
            patient = re_val,
            low = stratum$economy_low,
            high = stratum$economy_high,
            mean = stratum$economy_typical,
            z_entry = NULL, decimals = 0
          )
        }
      } else {
        add_row(
          label = paste0(tr("aerobic_power", language), " (W/kg)"),
          patient = analysis@map_per_kg %||% NA_real_,
          low = stratum$map_per_kg_low %||% NA_real_,
          high = stratum$map_per_kg_high %||% NA_real_,
          mean = stratum$map_per_kg_typical %||% NA_real_,
          z_entry = zs$map_per_kg_z, decimals = 2
        )
        # PPO in absolute W -- derive stratum mean/band from W/kg * body mass
        ppo_low <- if (is.numeric(stratum$map_per_kg_low)   && is.numeric(weight_kg) && is.finite(weight_kg)) stratum$map_per_kg_low   * weight_kg else NA_real_
        ppo_high <- if (is.numeric(stratum$map_per_kg_high) && is.numeric(weight_kg) && is.finite(weight_kg)) stratum$map_per_kg_high  * weight_kg else NA_real_
        ppo_mean <- if (is.numeric(stratum$map_per_kg_typical) && is.numeric(weight_kg) && is.finite(weight_kg)) stratum$map_per_kg_typical * weight_kg else NA_real_
        add_row(
          label = paste0(tr("peak_power", language), " (W)"),
          patient = analysis@ppo_watts %||% NA_real_,
          low = ppo_low, high = ppo_high, mean = ppo_mean,
          z_entry = zs$ppo_z, decimals = 0
        )
        if (!is.null(stratum$efficiency_typical)) {
          ge_val <- NA_real_
          sbs <- tryCatch(analysis@substrate_by_stage, error = function(e) NULL)
          if (is.data.frame(sbs) && "gross_efficiency_pct" %in% names(sbs)) {
            ge_val <- suppressWarnings(max(sbs$gross_efficiency_pct, na.rm = TRUE))
            if (!is.finite(ge_val)) ge_val <- NA_real_
          }
          # Fall back: compute GE at a submaximal reference stage (~70% VO2peak)
          # directly from stage_summary when substrate_by_stage is absent.
          if (!is.finite(ge_val)) {
            idx <- submax_stage_idx(analysis, peaks, required = "power_w")
            if (!is.na(idx)) {
              ss <- analysis@stage_summary
              rer_ref <- if ("rer" %in% names(ss) && is.finite(ss$rer[idx])) ss$rer[idx] else 0.95
              ge_val <- tryCatch(
                calculate_gross_efficiency(ss$vo2_ml[idx], ss$power_w[idx], rer_ref),
                error = function(e) NA_real_
              )
            }
          }
          add_row(
            label = paste0(tr("gross_efficiency", language), " (%)"),
            patient = ge_val,
            low = stratum$efficiency_low,
            high = stratum$efficiency_high,
            mean = stratum$efficiency_typical,
            z_entry = NULL, decimals = 1
          )
        }
      }

      out$has_population_norms <- length(rows) > 0

      # Localized one-line stratum label, with the source citation after.
      desc_local <- stratum_localized_desc(sport_for_norms, level_for_norms,
                                           participant@sex,
                                           tryCatch(participant@age, error = function(e) NA_real_),
                                           language)
      out$pn_description    <- escape_typst(desc_local)
      base_cite <- stratum$citation_short %||% ""
      cite_parts <- base_cite
      map_cite <- stratum$map_per_kg_citation_short %||% ""
      if (nzchar(map_cite) && !identical(map_cite, base_cite)) {
        cite_parts <- paste0(cite_parts, "; ", map_cite, " (MAP)")
      }
      eff_cite <- stratum$efficiency_citation_short %||% ""
      if (nzchar(eff_cite) && !identical(eff_cite, base_cite)) {
        cite_parts <- paste0(cite_parts, "; ", eff_cite, " (GE)")
      }
      out$pn_citation_short <- escape_typst(cite_parts)
      out$pn_rows_content   <- paste(rows, collapse = ",\n    ")

      sd_msg <- NULL
      if ("tabulated" %in% sd_sources) {
        sd_msg <- c(sd_msg, tr("norms_sd_tabulated", language))
      }
      if ("estimated" %in% sd_sources) {
        sd_msg <- c(sd_msg, tr("norms_sd_estimated", language))
      }
      out$pn_sd_note <- escape_typst(paste(sd_msg, collapse = " "))
    } else {
      out$pn_description <- escape_typst(tr("norms_no_stratum", language))
    }
  }

  out
}

build_report_graph_cache_key <- function(analysis, language, athlete_sport, athlete_level) {
  breaths <- analysis@data@breaths
  peaks <- analysis@peaks
  thresholds <- analysis@thresholds
  stage_summary <- analysis@stage_summary

  stage_rows <- if (is.null(stage_summary)) 0L else nrow(stage_summary)
  stage_end_time <- if (stage_rows > 0 && "time_s" %in% names(stage_summary)) {
    round(max(stage_summary$time_s, na.rm = TRUE), 1)
  } else {
    NA_real_
  }
  stage_vo2_signature <- if (stage_rows > 0 && "vo2_ml" %in% names(stage_summary)) {
    round(sum(stage_summary$vo2_ml, na.rm = TRUE), 1)
  } else {
    NA_real_
  }

  paste(
    analysis@data@participant@id %||% "",
    as.character(analysis@data@metadata@test_date) %||% "",
    as.character(analysis@data@is_averaged),
    analysis@data@averaging_window %||% "",
    nrow(breaths),
    round(max(breaths$time_s, na.rm = TRUE), 1),
    stage_rows,
    stage_end_time,
    stage_vo2_signature,
    if (!is.null(peaks) && length(peaks@vo2_peak) > 0) round(peaks@vo2_peak, 1) else NA_real_,
    if (!is.null(thresholds) && length(thresholds@vt1_vo2) > 0) round(thresholds@vt1_vo2, 1) else NA_real_,
    if (!is.null(thresholds) && length(thresholds@vt2_vo2) > 0) round(thresholds@vt2_vo2, 1) else NA_real_,
    language,
    athlete_sport %||% "",
    athlete_level %||% "",
    sep = "|"
  )
}


#' Clean Up Temporary Files
#'
#' @param files List of file paths to remove
#' @keywords internal
cleanup_temp_files <- function(files) {
  for (f in files) {
    if (is.character(f) && file.exists(f)) {
      unlink(f)
    }
  }
}


#' Get Template Path
#'
#' @param custom_template Optional custom template path
#' @return Path to Typst template
#' @keywords internal
get_template_path <- function(custom_template = NULL) {
  if (!is.null(custom_template) && length(custom_template) > 0 && file.exists(custom_template)) {
    return(custom_template)
  }

  system.file("templates", "cpet_report.typ", package = "cardiometR")
}


#' Process Template Conditionals
#'
#' @description
#' Recursively processes mustache-style conditionals in template content.
#' Handles nested conditionals correctly by finding matching pairs.
#'
#' @param content Template content string
#' @param data Named list of template data
#' @return Processed template content
#' @keywords internal

#' Results Module UI
#'
#' @param id Module namespace ID.
#' @param language Language code ("en" or "fr").
#'
#' @return A Shiny UI element.
#'
#' @keywords internal
mod_results_ui <- function(id, language = "en") {
  ns <- shiny::NS(id)

  bslib::card(
    fill = TRUE,
    bslib::card_header(
      shiny::icon("chart-column"),
      shiny::span(id = ns("results_header"), tr("peak_values", language))
    ),
    bslib::card_body(
      class = "overflow-auto",

      # Athlete profile headline (3 value_boxes)
      shiny::uiOutput(ns("athlete_profile")),

      shiny::tags$hr(class = "section-divider"),

      # Compact peak stat-strip (VO2/kg, HR, Power/Speed, VE, QR)
      shiny::h6(shiny::span(id = ns("peak_title"), tr("detailed_peak_values", language))),
      shiny::uiOutput(ns("peak_display")),

      # Resting values (rendered only when a leading rest block exists)
      shiny::uiOutput(ns("resting_display")),

      shiny::tags$hr(class = "section-divider"),

      # Thresholds table (legacy point estimates)
      shiny::h6(shiny::span(id = ns("threshold_title"), tr("threshold_results", language))),
      shiny::uiOutput(ns("threshold_display"))
    )
  )
}

#' Secondary right-column output slot produced by mod_results_server.
#'
#' Exposes the normative-comparison card, estimates accordion, stage table,
#' and longitudinal panel so mod_plots_ui can render them under the 9-panel.
#'
#' @param id Namespace id of the results module.
#' @keywords internal
mod_results_secondary_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::uiOutput(ns("secondary_right"))
}

#' Results Module Server
#'
#' @param id Module namespace ID.
#' @param language Reactive language value.
#' @param cpet_data Reactive CpetData object from upload module.
#' @param participant Reactive Participant object from participant module.
#' @param settings Reactive settings list from settings module.
#' @param prediction_source Reactive prediction equation source ("jones" or "prefaut").
#'
#' @return A list with reactive values:
#'   - `analysis`: Reactive CpetAnalysis object.
#'
#' @keywords internal
mod_results_server <- function(id, language, cpet_data, participant, settings,
                               prediction_source = shiny::reactive("jones"),
                               dark_mode = shiny::reactive(FALSE)) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

    shiny::observeEvent(language(), {
      lang <- language()
      ids <- c(ns("results_header"), ns("peak_title"), ns("norm_header"),
               ns("threshold_title"), ns("stage_title"), ns("export_label"),
               ns("resting_title"))
      texts <- c(tr("peak_values", lang), tr("detailed_peak_values", lang),
                 tr("normative_comparison", lang), tr("threshold_results", lang),
                 tr("stage_results", lang), tr("export_csv", lang),
                 tr("resting_values_title", lang))
      session$sendCustomMessage("update_text", as.list(stats::setNames(texts, ids)))
    })

    averaged_data <- shiny::reactive({
      data <- cpet_data()
      shiny::req(data)

      if (isTRUE(data@is_averaged)) {
        return(data)
      }

      s <- settings()
      lang <- language()
      method <- s$averaging_method %||% "rolling"
      window <- s$averaging_window %||% 30

      tryCatch({
        average(data, method = method, window = window)
      }, error = function(e) {
        shiny::showNotification(
          tr("warning_averaging_failed", lang),
          type = "warning",
          duration = 5
        )
        data
      })
    })

    analysis <- shiny::eventReactive(
      list(cpet_data(), settings(), participant()),
      {
        data <- cpet_data()
        shiny::req(data)

        lang <- language()

        shiny::withProgress(message = tr("analyzing_data", lang), value = 0, {
          s <- settings()
          p <- participant()
          data_avg <- averaged_data()

          shiny::incProgress(0.2, detail = tr("step_averaging", lang))

          if (!is.null(p)) {
            data@participant <- p
            data_avg@participant <- p
          }

          shiny::incProgress(0.2, detail = tr("step_peaks", lang))
          peaks <- find_peaks(
            data_avg,
            averaging = s$averaging_window %||% 30
          )

          shiny::incProgress(0.2, detail = tr("step_thresholds", lang))
          thresholds <- NULL
          if (length(s$threshold_methods) > 0) {
            tryCatch({
              thresholds <- detect_thresholds(
                data_avg,
                methods = s$threshold_methods,
                window_s = s$averaging_window
              )
            }, error = function(e) {
              shiny::showNotification(
                tr("warning_thresholds_failed", lang),
                type = "warning",
                duration = 5
              )
              NULL
            })
          }

          shiny::incProgress(0.2, detail = tr("step_stages", lang))
          stage_summary <- NULL
          stages_tbl <- NULL
          tryCatch({
            data_with_stages <- extract_stages(data_avg, protocol = s$protocol,
                                               stage_duration = s$stage_duration,
                                               increment = s$increment_size)
            stage_summary <- summarize_stages(data_with_stages, window_s = s$averaging_window %||% 30)
            stages_tbl <- tryCatch(data_with_stages@stages, error = function(e) NULL)
            data_avg@stages <- stages_tbl
            data_avg@breaths <- data_with_stages@breaths
          }, error = function(e) {
            shiny::showNotification(
              tr("warning_stages_failed", lang),
              type = "warning",
              duration = 5
            )
            NULL
          })

          shiny::incProgress(0.2, detail = tr("step_complete", lang))

          protocol_config <- tryCatch({
            ProtocolConfig(
              modality = s$modality %||% "cycling",
              starting_intensity = s$starting_intensity,
              increment_size = s$increment_size,
              stage_duration_s = s$stage_duration
            )
          }, error = function(e) NULL)

          analysis_obj <- CpetAnalysis(
            data = data_avg,
            peaks = peaks,
            thresholds = thresholds,
            stage_summary = stage_summary,
            protocol_config = protocol_config
          )

          analysis_obj <- populate_phase1_metrics(
            analysis_obj,
            stage_summary = stage_summary,
            breath_df = data_avg@breaths,
            participant = p,
            settings = s,
            stages = stages_tbl
          )

          analysis_obj
        })
      }
    )

    # -- Helpers ---------------------------------------------------------

    fmt_z_line <- function(z_entry, lang) {
      if (is.null(z_entry)) return(shiny::span(class = "text-muted small", "\u2014"))
      z <- z_entry$z
      pct <- z_entry$percentile
      if (!is.finite(z) || !is.finite(pct)) {
        return(shiny::span(class = "text-muted small", "\u2014"))
      }
      shiny::tags$small(
        class = "text-muted d-block",
        sprintf("%s %.2f \u00b7 p%.0f",
                tr("z_score", lang), z, pct)
      )
    }

    popover_howto <- function(text, title_text) {
      bslib::popover(
        shiny::icon("circle-info"),
        shiny::tags$strong(title_text),
        shiny::br(),
        text,
        title = title_text,
        placement = "right"
      )
    }

    # -- Athlete profile headline ---------------------------------------

    output$athlete_profile <- shiny::renderUI({
      a <- analysis()
      lang <- language()
      if (is.null(a)) {
        return(shiny::div(class = "text-muted text-center py-4",
                          tr("upload_prompt", lang)))
      }

      peaks <- a@peaks
      participant_obj <- a@data@participant
      wt <- tryCatch(participant_obj@weight_kg, error = function(e) NA_real_)

      vo2_kg <- tryCatch({
        if (!is.null(peaks@vo2_kg_peak) && is.finite(peaks@vo2_kg_peak)) peaks@vo2_kg_peak
        else if (!is.null(peaks@vo2_peak) && is.finite(wt) && wt > 0) peaks@vo2_peak / wt
        else NA_real_
      }, error = function(e) NA_real_)

      map_kg <- a@map_per_kg %||% NA_real_
      ppo    <- a@ppo_watts %||% NA_real_
      kfrac  <- a@kuipers_fraction %||% NA_real_

      zs <- a@z_scores %||% list()

      howto_vo2 <- paste0(
        "VO2peak / body mass. Peak taken as 30-s rolling average at test end. ",
        "Bassett & Howley 2000; Poole & Jones 2017."
      )
      howto_map <- paste0(
        "MAP = last completed stage power + (t_final / t_stage) x increment (Kuipers 1985). ",
        "Divided by body mass for W/kg. Hawley & Noakes 1992."
      )
      howto_ppo <- "Peak 1-min power sustained at test end; Kuipers last-stage adjustment applied."

      bslib::layout_column_wrap(
        width = 1 / 3,
        fill = FALSE,
        gap = "1rem",
        bslib::value_box(
          title = shiny::tagList(
            tr("aerobic_capacity", lang), " ",
            popover_howto(howto_vo2, tr("how_computed", lang))
          ),
          value = if (is.finite(vo2_kg)) sprintf("%.1f", vo2_kg) else "--",
          showcase = shiny::icon("heart-pulse"),
          theme = "primary",
          min_height = "150px",
          shiny::p(tr("unit_ml_kg_min", lang)),
          fmt_z_line(zs$vo2_peak_z, lang)
        ),
        bslib::value_box(
          title = shiny::tagList(
            tr("aerobic_power", lang), " ",
            popover_howto(howto_map, tr("how_computed", lang))
          ),
          value = if (is.finite(map_kg)) sprintf("%.2f", map_kg) else "--",
          showcase = shiny::icon("bolt-lightning"),
          theme = bslib::value_box_theme(bg = "var(--bs-tertiary-bg)", fg = "var(--bs-body-color)"),
          min_height = "150px",
          shiny::p("W/kg"),
          fmt_z_line(zs$map_per_kg_z, lang)
        ),
        bslib::value_box(
          title = shiny::tagList(
            tr("peak_power", lang), " ",
            popover_howto(howto_ppo, tr("how_computed", lang))
          ),
          value = if (is.finite(ppo)) as.character(round(ppo)) else "--",
          showcase = shiny::icon("mountain"),
          theme = bslib::value_box_theme(bg = "var(--bs-tertiary-bg)", fg = "var(--bs-body-color)"),
          min_height = "150px",
          shiny::p(tr("unit_watts", lang)),
          shiny::tags$small(
            class = "text-muted d-block",
            if (is.finite(kfrac)) {
              sprintf("%s %.2f", tr("last_stage_fraction_label", lang), kfrac)
            } else "\u2014"
          )
        )
      )
    })

    # -- Detailed peak values (legacy row) -------------------------------

    output$peak_display <- shiny::renderUI({
      a <- analysis()
      lang <- language()
      if (is.null(a)) return(NULL)

      peaks <- a@peaks
      is_treadmill <- !is.null(peaks@speed_peak) ||
        (!is.null(a@protocol_config) && a@protocol_config@modality == "treadmill") ||
        has_signal(a@data@breaths, "speed_kmh")

      fmt <- function(x, digits = 1) {
        if (is.null(x) || !is.finite(x)) "--" else sprintf(paste0("%.", digits, "f"), x)
      }
      int_fmt <- function(x) {
        if (is.null(x) || !is.finite(x)) "--" else as.character(round(x))
      }

      stat_cell <- function(label, value, unit) {
        shiny::div(
          class = "stat-cell",
          shiny::div(class = "stat-label", label),
          shiny::div(class = "stat-value", value),
          shiny::div(class = "stat-unit", unit)
        )
      }

      third <- if (is_treadmill) {
        stat_cell(tr("speed", lang), fmt(peaks@speed_peak, 1), tr("unit_kmh", lang))
      } else {
        stat_cell(tr("power", lang), int_fmt(peaks@power_peak), tr("unit_watts", lang))
      }

      shiny::div(
        class = "stat-strip",
        stat_cell(tr("vo2_kg", lang), fmt(peaks@vo2_kg_peak, 1),
                  tr("unit_ml_kg_min", lang)),
        stat_cell(tr("hr", lang), int_fmt(peaks@hr_peak), tr("unit_bpm", lang)),
        third,
        stat_cell(tr("ve", lang), fmt(peaks@ve_peak, 1), tr("unit_l_min", lang)),
        stat_cell(tr("rer", lang), fmt(peaks@rer_peak, 2), "")
      )
    })

    # -- Resting values (leading on-bike rest block) ---------------------

    output$resting_display <- shiny::renderUI({
      a <- analysis()
      lang <- language()
      if (is.null(a) || !has_resting(a)) return(NULL)

      r <- a@resting

      fmt <- function(x, digits = 1) {
        if (is.null(x) || !is.finite(x)) "--" else sprintf(paste0("%.", digits, "f"), x)
      }
      int_fmt <- function(x) {
        if (is.null(x) || !is.finite(x)) "--" else as.character(round(x))
      }
      dur_fmt <- function(s) {
        if (is.null(s) || !is.finite(s)) "--"
        else sprintf("%d:%02d", as.integer(s) %/% 60L, as.integer(s) %% 60L)
      }
      stat_cell <- function(label, value, unit) {
        shiny::div(
          class = "stat-cell",
          shiny::div(class = "stat-label", label),
          shiny::div(class = "stat-value", value),
          shiny::div(class = "stat-unit", unit)
        )
      }

      caption_tpl <- tr("resting_values_caption", lang)
      caption_txt <- tryCatch(
        sprintf(caption_tpl,
                as.integer(round(r$window_s %||% NA_real_)),
                as.integer(r$n_breaths %||% 0L)),
        error = function(e) caption_tpl
      )

      shiny::tagList(
        shiny::tags$hr(class = "section-divider"),
        shiny::h6(shiny::span(id = ns("resting_title"),
                              tr("resting_values_title", lang))),
        shiny::div(
          class = "stat-strip",
          style = "--app-stat-cols: 6;",
          stat_cell("VO2", int_fmt(r$vo2_rest), tr("unit_ml_min", lang)),
          stat_cell(tr("vo2_kg", lang), fmt(r$vo2_kg_rest, 1),
                    tr("unit_ml_kg_min", lang)),
          stat_cell(tr("hr", lang), int_fmt(r$hr_rest), tr("unit_bpm", lang)),
          stat_cell(tr("ve", lang), fmt(r$ve_rest, 1), tr("unit_l_min", lang)),
          stat_cell(tr("rer", lang), fmt(r$rer_rest, 2), ""),
          stat_cell(tr("resting_rest_duration", lang),
                    dur_fmt(r$duration_s), "m:ss")
        ),
        shiny::tags$small(class = "text-muted d-block mt-1", caption_txt)
      )
    })

    # -- Secondary right-column slot (rendered inside mod_plots_ui) ------
    output$secondary_right <- shiny::renderUI({
      a <- analysis()
      lang <- language()
      if (is.null(a)) return(NULL)

      shiny::tagList(
        bslib::card(
          bslib::card_header(
            shiny::icon("chart-line"),
            shiny::span(id = ns("norm_header"), tr("normative_comparison", lang))
          ),
          bslib::card_body(
            shiny::uiOutput(ns("norms_info")),
            shiny::plotOutput(ns("zscore_strip_plot"), height = "200px"),
            shiny::plotOutput(ns("vo2_power_slope_plot"), height = "240px")
          )
        ),
        shiny::uiOutput(ns("estimates_accordion")),
        bslib::accordion(
          open = TRUE,
          bslib::accordion_panel(
            title = tr("stage_results", lang),
            icon = shiny::icon("table"),
            DT::dataTableOutput(ns("stage_table")),
            shiny::downloadButton(
              ns("download_data"),
              label = shiny::tagList(
                shiny::icon("file-csv"),
                shiny::span(id = ns("export_label"), tr("export_csv", lang))
              ),
              class = "btn-outline-secondary btn-sm mt-2"
            )
          )
        ),
        shiny::uiOutput(ns("longitudinal_panel"))
      )
    })

    # -- Normative comparison block --------------------------------------

    output$norms_info <- shiny::renderUI({
      a <- analysis()
      s <- settings()
      if (is.null(a)) return(NULL)
      athlete_sport <- s$athlete_sport
      if (is.null(athlete_sport) || !nzchar(athlete_sport) || athlete_sport == "general") {
        return(NULL)
      }
      athlete_level <- s$athlete_level %||% "recreational"
      participant_obj <- a@data@participant
      norms <- tryCatch(
        get_normative_data(athlete_sport, athlete_level,
                           participant_obj@sex, participant_obj@age),
        error = function(e) NULL
      )
      if (is.null(norms)) return(NULL)
      shiny::tags$small(
        class = "text-muted d-block mb-2",
        shiny::icon("chart-bar"),
        paste0(" ", norms$description, " (", norms$citation_short, ")")
      )
    })

    is_treadmill_reactive <- shiny::reactive({
      a <- analysis()
      if (is.null(a)) return(FALSE)
      tryCatch(!is.null(a@protocol_config) &&
                 a@protocol_config@modality == "treadmill",
               error = function(e) FALSE)
    })

    output$zscore_strip_plot <- shiny::renderPlot({
      a <- analysis()
      shiny::req(a)
      tryCatch(
        plot_zscore_strip(a,
                          metrics = c("vo2_peak", "map_per_kg", "ppo"),
                          language = language(),
                          dark = isTRUE(dark_mode())),
        error = function(e) {
          ggplot2::ggplot() +
            ggplot2::annotate("text", x = 0.5, y = 0.5,
                              label = tr("insufficient_stratum_data", language()),
                              color = "gray50") +
            theme_cardiometr(dark = isTRUE(dark_mode()))
        }
      )
    }, bg = "transparent")

    output$vo2_power_slope_plot <- shiny::renderPlot({
      a <- analysis()
      shiny::req(a)
      # Hide VO2-Power slope for treadmill (no power_w); plot fn also returns
      # a placeholder in that case — belt and suspenders.
      if (isTRUE(is_treadmill_reactive())) {
        return(
          ggplot2::ggplot() +
            ggplot2::annotate("text", x = 0.5, y = 0.5,
                              label = tr("insufficient_stratum_data", language()),
                              color = "gray50") +
            theme_cardiometr(dark = isTRUE(dark_mode())) +
            ggplot2::theme(axis.text = ggplot2::element_blank(),
                           axis.ticks = ggplot2::element_blank())
        )
      }
      tryCatch(
        plot_vo2_power_slope(a, language = language(),
                             dark = isTRUE(dark_mode())),
        error = function(e) {
          ggplot2::ggplot() +
            ggplot2::annotate("text", x = 0.5, y = 0.5,
                              label = tr("insufficient_stratum_data", language()),
                              color = "gray50") +
            theme_cardiometr(dark = isTRUE(dark_mode()))
        }
      )
    }, bg = "transparent")

    # -- Thresholds table (legacy) ---------------------------------------

    output$threshold_display <- shiny::renderUI({
      a <- analysis()
      lang <- language()

      if (is.null(a) || is.null(a@thresholds)) {
        return(shiny::div(class = "text-muted small",
                          tr("message_no_thresholds", lang)))
      }

      th <- a@thresholds
      peak_vo2 <- if (!is.null(a@peaks)) a@peaks@vo2_peak else NULL
      pct_peak <- function(vo2) {
        if (is.null(peak_vo2) || !is.finite(peak_vo2) || peak_vo2 <= 0) return("--")
        paste0(round(vo2 / peak_vo2 * 100), " %")
      }

      shiny::tagList(
        shiny::tags$table(
          class = "table table-sm table-clean",
          shiny::tags$thead(
            shiny::tags$tr(
              shiny::tags$th(""),
              shiny::tags$th(paste("VO2", tr("unit_ml_min", lang))),
              shiny::tags$th(tr("pct_of_peak", lang)),
              shiny::tags$th(tr("hr", lang)),
              shiny::tags$th(tr("power", lang))
            )
          ),
          shiny::tags$tbody(
            if (!is.null(th@vt1_vo2) && !is.na(th@vt1_vo2)) {
              shiny::tags$tr(
                shiny::tags$td(shiny::strong(tr("vt1", lang))),
                shiny::tags$td(round(th@vt1_vo2)),
                shiny::tags$td(pct_peak(th@vt1_vo2)),
                shiny::tags$td(if (!is.null(th@vt1_hr)) round(th@vt1_hr) else "--"),
                shiny::tags$td(if (!is.null(th@vt1_power)) round(th@vt1_power) else "--")
              )
            },
            if (!is.null(th@vt2_vo2) && !is.na(th@vt2_vo2)) {
              shiny::tags$tr(
                shiny::tags$td(shiny::strong(tr("vt2", lang))),
                shiny::tags$td(round(th@vt2_vo2)),
                shiny::tags$td(pct_peak(th@vt2_vo2)),
                shiny::tags$td(if (!is.null(th@vt2_hr)) round(th@vt2_hr) else "--"),
                shiny::tags$td(if (!is.null(th@vt2_power)) round(th@vt2_power) else "--")
              )
            }
          )
        ),
        shiny::tags$small(class = "text-muted d-block",
                          tr("threshold_table_caption", lang))
      )
    })

    # -- Estimates & caveats accordion ----------------------------------

    output$estimates_accordion <- shiny::renderUI({
      a <- analysis()
      lang <- language()
      if (is.null(a)) return(NULL)

      # 1. VT ranges (enriched with method/smoothing per endpoint)
      vt1_r <- a@vt1_range
      vt2_r <- a@vt2_range
      vt_details <- tryCatch(
        detect_threshold_range(a@data@breaths),
        error = function(e) NULL
      )
      endpoint_label <- function(values_df, end = c("low", "high")) {
        end <- match.arg(end)
        if (is.null(values_df) || !nrow(values_df)) return("--")
        v <- values_df$vo2
        idx <- if (end == "low") which.min(v) else which.max(v)
        if (!length(idx)) return("--")
        sprintf("%s / %s", values_df$method[idx], values_df$smoothing[idx])
      }
      vt1_low_lbl  <- endpoint_label(vt_details$vt1_values, "low")
      vt1_high_lbl <- endpoint_label(vt_details$vt1_values, "high")
      vt2_low_lbl  <- endpoint_label(vt_details$vt2_values, "low")
      vt2_high_lbl <- endpoint_label(vt_details$vt2_values, "high")
      vt_table <- shiny::tags$table(
        class = "table table-sm",
        shiny::tags$thead(shiny::tags$tr(
          shiny::tags$th(""),
          shiny::tags$th("Low"),
          shiny::tags$th(tr("vt_method_low", lang)),
          shiny::tags$th("High"),
          shiny::tags$th(tr("vt_method_high", lang)),
          shiny::tags$th("Point")
        )),
        shiny::tags$tbody(
          shiny::tags$tr(
            shiny::tags$td(shiny::strong(tr("vt1", lang))),
            shiny::tags$td(if (length(vt1_r) >= 1 && is.finite(vt1_r[1])) round(vt1_r[1]) else "--"),
            shiny::tags$td(shiny::tags$small(class = "text-muted", vt1_low_lbl)),
            shiny::tags$td(if (length(vt1_r) >= 2 && is.finite(vt1_r[2])) round(vt1_r[2]) else "--"),
            shiny::tags$td(shiny::tags$small(class = "text-muted", vt1_high_lbl)),
            shiny::tags$td(if (!is.null(a@thresholds) && !is.null(a@thresholds@vt1_vo2) &&
                               !is.na(a@thresholds@vt1_vo2)) round(a@thresholds@vt1_vo2) else "--")
          ),
          shiny::tags$tr(
            shiny::tags$td(shiny::strong(tr("vt2", lang))),
            shiny::tags$td(if (length(vt2_r) >= 1 && is.finite(vt2_r[1])) round(vt2_r[1]) else "--"),
            shiny::tags$td(shiny::tags$small(class = "text-muted", vt2_low_lbl)),
            shiny::tags$td(if (length(vt2_r) >= 2 && is.finite(vt2_r[2])) round(vt2_r[2]) else "--"),
            shiny::tags$td(shiny::tags$small(class = "text-muted", vt2_high_lbl)),
            shiny::tags$td(if (!is.null(a@thresholds) && !is.null(a@thresholds@vt2_vo2) &&
                               !is.na(a@thresholds@vt2_vo2)) round(a@thresholds@vt2_vo2) else "--")
          )
        )
      )
      vt_panel <- bslib::accordion_panel(
        title = tr("vt_range", lang),
        icon = shiny::icon("wave-square"),
        vt_table,
        shiny::tags$small(class = "text-muted", tr("vt_caveat", lang))
      )

      # 2. FTP range
      ftp_low <- if (!is.null(a@map_watts) && is.finite(a@map_watts)) 0.72 * a@map_watts else NA_real_
      ftp_high <- if (!is.null(a@map_watts) && is.finite(a@map_watts)) 0.77 * a@map_watts else NA_real_
      ftp_panel <- bslib::accordion_panel(
        title = tr("ftp_range", lang),
        icon = shiny::icon("gauge-high"),
        shiny::checkboxInput(ns("show_ftp"), tr("ftp_range", lang), value = FALSE),
        shiny::conditionalPanel(
          condition = "input.show_ftp == true", ns = ns,
          shiny::div(
            if (is.finite(ftp_low) && is.finite(ftp_high)) {
              shiny::tags$p(sprintf("FTP \u2248 %.0f\u2013%.0f W (0.72\u20130.77 \u00d7 MAP)",
                                    ftp_low, ftp_high))
            } else {
              shiny::tags$p(class = "text-muted", "--")
            },
            shiny::tags$small(class = "text-muted", tr("ftp_caveat", lang))
          )
        )
      )

      # 3. CP explainer
      cp_panel <- bslib::accordion_panel(
        title = tr("cp_explainer_title", lang),
        icon = shiny::icon("stopwatch"),
        shiny::tags$p(tr("cp_explainer", lang))
      )

      # 4. Substrate oxidation
      ss <- a@steady_state_stages
      has_steady <- !is.null(ss) && is.data.frame(ss) &&
        "steady_state_ok" %in% names(ss) && any(isTRUE(ss$steady_state_ok) |
                                                 ss$steady_state_ok %in% TRUE)
      substrate_body <- if (has_steady) {
        qualifying <- ss |> dplyr::filter(.data$steady_state_ok %in% TRUE)
        # Péronnet-Massicotte 1991: fractional fat energy from RER
        compute_fat_pct <- function(rer) {
          rer <- pmin(pmax(rer, 0.70), 1.00)
          pmax(0, pmin(100, (1.00 - rer) / (1.00 - 0.70) * 100))
        }
        if ("rer" %in% names(qualifying)) {
          qualifying <- qualifying |>
            dplyr::mutate(
              fat_pct = compute_fat_pct(.data$rer),
              cho_pct = 100 - .data$fat_pct
            )
          tbl_rows <- purrr::pmap(
            list(qualifying$stage %||% seq_len(nrow(qualifying)),
                 qualifying$rer,
                 qualifying$fat_pct,
                 qualifying$cho_pct),
            function(stg, rer, fat, cho) {
              shiny::tags$tr(
                shiny::tags$td(stg),
                shiny::tags$td(sprintf("%.2f", rer)),
                shiny::tags$td(sprintf("%.0f%%", fat)),
                shiny::tags$td(sprintf("%.0f%%", cho))
              )
            }
          )
          shiny::tags$table(
            class = "table table-sm",
            shiny::tags$thead(shiny::tags$tr(
              shiny::tags$th(tr("stage", lang)),
              shiny::tags$th("RER"),
              shiny::tags$th("Fat %"),
              shiny::tags$th("CHO %")
            )),
            shiny::tags$tbody(tbl_rows)
          )
        } else {
          shiny::tags$small(class = "text-muted", tr("substrate_explainer", lang))
        }
      } else {
        shiny::tags$small(class = "text-muted", tr("substrate_explainer", lang))
      }
      substrate_panel <- bslib::accordion_panel(
        title = tr("substrate_explainer_title", lang),
        icon = shiny::icon("fire"),
        substrate_body
      )

      shiny::tagList(
        shiny::h6(tr("estimates_and_caveats", lang)),
        bslib::accordion(
          open = FALSE,
          vt_panel, ftp_panel, cp_panel, substrate_panel
        )
      )
    })

    # -- Stage summary table ---------------------------------------------

    output$stage_table <- DT::renderDataTable({
      a <- analysis()
      lang <- language()

      if (is.null(a) || is.null(a@stage_summary)) return(NULL)

      is_treadmill <- (!is.null(a@protocol_config) && a@protocol_config@modality == "treadmill") ||
        has_signal(a@data@breaths, "speed_kmh") ||
        (!is.null(a@peaks) && !is.null(a@peaks@speed_peak))

      df <- if (is_treadmill && "speed_kmh" %in% names(a@stage_summary)) {
        a@stage_summary |>
          dplyr::select(
            Stage = dplyr::any_of("stage"),
            Speed = dplyr::any_of("speed_kmh"),
            VO2 = dplyr::any_of("vo2_ml"),
            HR = dplyr::any_of("hr_bpm"),
            VE = dplyr::any_of("ve_l"),
            RER = dplyr::any_of("rer")
          )
      } else {
        a@stage_summary |>
          dplyr::select(
            Stage = dplyr::any_of("stage"),
            Power = dplyr::any_of("power_w"),
            VO2 = dplyr::any_of("vo2_ml"),
            HR = dplyr::any_of("hr_bpm"),
            VE = dplyr::any_of("ve_l"),
            RER = dplyr::any_of("rer")
          )
      }

      df <- df |>
        dplyr::mutate(dplyr::across(dplyr::where(is.numeric), ~ round(.x, 1)))

      dt <- DT::datatable(
        df,
        options = list(pageLength = 10, scrollY = "200px", dom = "t", ordering = FALSE),
        rownames = FALSE,
        class = "compact stripe"
      )

      if ("RER" %in% names(df)) {
        dt <- dt |>
          DT::formatStyle("RER",
            backgroundColor = DT::styleInterval(1.10, c("transparent", "#fff3cd"))
          )
      }
      dt
    })

    output$download_data <- shiny::downloadHandler(
      filename = function() {
        paste0("cpet_data_", format(Sys.Date(), "%Y%m%d"), ".csv")
      },
      content = function(file) {
        a <- analysis()
        shiny::req(a)
        utils::write.csv(a@data@breaths, file, row.names = FALSE)
      }
    )

    # -- Longitudinal panel ----------------------------------------------

    output$longitudinal_panel <- shiny::renderUI({
      a <- analysis()
      lang <- language()
      if (is.null(a)) return(NULL)

      pid <- tryCatch(a@data@participant@id, error = function(e) NA_character_)
      prior <- tryCatch(longitudinal_cache_read(pid), error = function(e) NULL)
      has_prior <- !is.null(prior) && nrow(prior) > 0

      bslib::card(
        bslib::card_header(
          shiny::icon("clock-rotate-left"),
          tr("longitudinal_title", lang)
        ),
        bslib::card_body(
          shiny::checkboxInput(ns("save_longitudinal"),
                               tr("save_longitudinal", lang), value = FALSE),
          if (has_prior) {
            shiny::plotOutput(ns("longitudinal_plot"), height = "240px")
          } else {
            shiny::div(class = "text-muted small", tr("no_prior_tests", lang))
          }
        )
      )
    })

    shiny::observeEvent(input$save_longitudinal, {
      if (!isTRUE(input$save_longitudinal)) return()
      a <- analysis()
      if (is.null(a)) return()
      p <- tryCatch(a@data@participant, error = function(e) NULL)
      pid <- tryCatch(p@id, error = function(e) NA_character_)
      if (is.null(pid) || is.na(pid) || !nzchar(as.character(pid))) return()
      wt <- tryCatch(p@weight_kg, error = function(e) NA_real_)
      age <- tryCatch(as.numeric(p@age), error = function(e) NA_real_)
      sex <- tryCatch(as.character(p@sex), error = function(e) NA_character_)
      row <- list(
        timestamp = format(Sys.time(), "%Y-%m-%dT%H:%M:%S"),
        vo2_peak = tryCatch(a@peaks@vo2_kg_peak, error = function(e) NA_real_),
        map_per_kg = a@map_per_kg %||% NA_real_,
        ppo = a@ppo_watts %||% NA_real_,
        weight_kg = wt,
        age = age,
        sex = sex
      )
      tryCatch(
        longitudinal_cache_write(pid, row),
        error = function(e) cli::cli_warn("Longitudinal save failed: {e$message}")
      )
    }, ignoreInit = TRUE)

    output$longitudinal_plot <- shiny::renderPlot({
      a <- analysis()
      shiny::req(a)
      pid <- tryCatch(a@data@participant@id, error = function(e) NA_character_)
      prior <- tryCatch(longitudinal_cache_read(pid), error = function(e) NULL)
      shiny::req(prior)
      # Take most recent prior as the "prior analysis" summary surrogate.
      latest <- prior |> dplyr::slice_tail(n = 1)
      # plot_longitudinal_delta expects an analysis-like object; since we only
      # persisted summary fields, pass a minimal environment with the same
      # accessors.
      # Build a minimal prior analysis by cloning the current one and
      # overwriting only the summary slots consumed by the plot helper.
      prior_stub <- a
      prior_stub@map_per_kg <- as.numeric(latest$map_per_kg[1])
      prior_stub@ppo_watts <- as.numeric(latest$ppo[1])
      prior_peaks <- a@peaks
      prior_peaks@vo2_kg_peak <- as.numeric(latest$vo2_peak[1])
      prior_stub@peaks <- prior_peaks
      plot_longitudinal_delta(a, prior_stub, language = language(),
                              dark = isTRUE(dark_mode()))
    }, bg = "transparent")

    list(
      analysis = analysis
    )
  })
}

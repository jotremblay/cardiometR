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
      tr("peak_values", language)
    ),
    bslib::card_body(
      class = "overflow-auto",
      # Peak values
      shiny::uiOutput(ns("peak_display")),

      # Comparison with predicted values
      shiny::uiOutput(ns("comparison_display")),

      shiny::hr(),

      # Thresholds
      shiny::h6(tr("threshold_results", language)),
      shiny::uiOutput(ns("threshold_display")),

      shiny::hr(),

      # Stage summary table
      shiny::h6(tr("stage_results", language)),
      DT::dataTableOutput(ns("stage_table")),
      shiny::downloadButton(
        ns("download_data"),
        label = tr("export_csv", language),
        icon = shiny::icon("file-csv"),
        class = "btn-outline-secondary btn-sm mt-2"
      )
    )
  )
}

#' Results Module Server
#'
#' @param id Module namespace ID.
#' @param language Reactive language value.
#' @param cpet_data Reactive CpetData object from upload module.
#' @param participant Reactive Participant object from participant module.
#' @param settings Reactive settings list from settings module.
#'
#' @return A list with reactive values:
#'   - `analysis`: Reactive CpetAnalysis object.
#'
#' @keywords internal
mod_results_server <- function(id, language, cpet_data, participant, settings) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

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

    # Computed analysis
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

          # Update participant in data if edited
          if (!is.null(p)) {
            data@participant <- p
            data_avg@participant <- p
          }

          # Find peaks with specified averaging
          shiny::incProgress(0.2, detail = tr("step_peaks", lang))
          peaks <- find_peaks(data, averaging = s$averaging_window)

          # Detect thresholds (if methods specified)
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

          # Extract and summarize stages
          shiny::incProgress(0.2, detail = tr("step_stages", lang))
          stage_summary <- NULL
          tryCatch({
            data_with_stages <- extract_stages(data, protocol = s$protocol,
                                               stage_duration = s$stage_duration)
            stage_summary <- summarize_stages(data_with_stages)
          }, error = function(e) {
            shiny::showNotification(
              tr("warning_stages_failed", lang),
              type = "warning",
              duration = 5
            )
            NULL
          })

          shiny::incProgress(0.2, detail = tr("step_complete", lang))

          # Build protocol config from settings
          protocol_config <- tryCatch({
            ProtocolConfig(
              modality = s$modality %||% "cycling",
              starting_intensity = s$starting_intensity,
              increment_size = s$increment_size,
              stage_duration_s = s$stage_duration
            )
          }, error = function(e) NULL)

          # Create analysis object
          CpetAnalysis(
            data = data,
            peaks = peaks,
            thresholds = thresholds,
            stage_summary = stage_summary,
            protocol_config = protocol_config
          )
        })
      }
    )

    # Render peak values display
    output$peak_display <- shiny::renderUI({
      a <- analysis()
      lang <- language()

      if (is.null(a)) {
        return(
          shiny::div(
            class = "text-muted text-center py-4",
            tr("upload_prompt", lang)
          )
        )
      }

      peaks <- a@peaks

      # Detect modality: treadmill if speed data exists or protocol says treadmill
      is_treadmill <- !is.null(peaks@speed_peak) ||
        (!is.null(a@protocol_config) && a@protocol_config@modality == "treadmill") ||
        ("speed_kmh" %in% names(a@data@breaths))

      bslib::layout_columns(
        col_widths = 12,
        fill = FALSE,

        # VO2 peak - primary metric
        bslib::value_box(
          title = tr("vo2_kg", lang),
          value = sprintf("%.1f", peaks@vo2_kg_peak),
          showcase = shiny::icon("lungs"),
          theme = "primary",
          p(tr("unit_ml_kg_min", lang))
        ),

        bslib::layout_columns(
          col_widths = c(6, 6),
          fill = FALSE,

          # HR peak
          bslib::value_box(
            title = tr("hr", lang),
            value = if (!is.null(peaks@hr_peak)) round(peaks@hr_peak) else "--",
            showcase = shiny::icon("heart-pulse"),
            theme = "danger",
            p(tr("unit_bpm", lang)),
            min_height = "120px"
          ),

          # Power or Speed peak (depending on modality)
          if (is_treadmill) {
            bslib::value_box(
              title = tr("speed", lang),
              value = if (!is.null(peaks@speed_peak)) sprintf("%.1f", peaks@speed_peak) else "--",
              showcase = shiny::icon("person-running"),
              theme = "success",
              p(tr("unit_kmh", lang)),
              min_height = "120px"
            )
          } else {
            bslib::value_box(
              title = tr("power", lang),
              value = if (!is.null(peaks@power_peak)) round(peaks@power_peak) else "--",
              showcase = shiny::icon("bolt"),
              theme = "success",
              p(tr("unit_watts", lang)),
              min_height = "120px"
            )
          }
        ),

        bslib::layout_columns(
          col_widths = c(6, 6),
          fill = FALSE,

          # VE peak
          bslib::value_box(
            title = tr("ve", lang),
            value = sprintf("%.1f", peaks@ve_peak),
            showcase = shiny::icon("wind"),
            theme = "light",
            p(tr("unit_l_min", lang)),
            min_height = "120px"
          ),

          # RER peak
          bslib::value_box(
            title = tr("rer", lang),
            value = sprintf("%.2f", peaks@rer_peak),
            showcase = shiny::icon("lungs"),
            theme = if (peaks@rer_peak >= 1.10) "warning" else "light",
            min_height = "120px"
          )
        )
      )
    })

    # Render comparison with predicted/normative values
    output$comparison_display <- shiny::renderUI({
      a <- analysis()
      lang <- language()
      s <- settings()

      if (is.null(a) || is.null(a@peaks) || length(a@peaks@vo2_peak) == 0) {
        return(NULL)
      }

      peaks <- a@peaks
      participant <- a@data@participant
      predicted <- calculate_predicted_values(participant)

      # Compute percentages
      vo2_pct <- round(100 * peaks@vo2_peak / predicted$vo2_max, 0)
      hr_pct <- if (!is.null(peaks@hr_peak) && length(peaks@hr_peak) > 0) {
        round(100 * peaks@hr_peak / predicted$hr_max, 0)
      } else {
        NA
      }
      # Scale RER: 0.7-1.3 -> 0-100%
      rer_pct <- min(100, max(0, round((peaks@rer_peak - 0.7) / 0.6 * 100, 0)))

      # Color helper
      pct_color <- function(pct) {
        if (pct >= 100) "success"
        else if (pct >= 85) "primary"
        else if (pct >= 70) "warning"
        else "danger"
      }

      # Build gauge helper
      make_gauge <- function(label, pct, display_text = NULL) {
        color_class <- pct_color(pct)
        bar_width <- min(100, pct)
        display <- display_text %||% paste0(pct, "%")
        shiny::div(
          shiny::tags$small(class = "text-muted", label),
          shiny::div(class = "progress mb-2",
            shiny::div(
              class = paste0("progress-bar bg-", color_class),
              style = paste0("width:", bar_width, "%"),
              role = "progressbar",
              display
            )
          )
        )
      }

      # Normative context if athlete sport is set
      norms_info <- NULL
      athlete_sport <- s$athlete_sport
      if (!is.null(athlete_sport) && nchar(athlete_sport) > 0 && athlete_sport != "general") {
        athlete_level <- s$athlete_level %||% "recreational"
        norms <- get_normative_data(athlete_sport, athlete_level, participant@sex, participant@age)
        norms_info <- shiny::tags$small(
          class = "text-muted d-block mb-1",
          shiny::icon("chart-bar"),
          paste0(" ", norms$description, " (", norms$citation_short, ")")
        )
      }

      shiny::tagList(
        shiny::hr(),
        shiny::h6(tr("comparison_title", lang)),
        norms_info,
        make_gauge(
          tr("aerobic_capacity", lang),
          vo2_pct,
          paste0(vo2_pct, "% ", tr("pct_predicted", lang))
        ),
        if (!is.na(hr_pct)) {
          make_gauge(
            tr("cardiovascular_response", lang),
            hr_pct,
            paste0(hr_pct, "% ", tr("pct_predicted", lang))
          )
        },
        make_gauge(
          tr("ventilatory_response", lang),
          rer_pct,
          sprintf("RER %.2f", peaks@rer_peak)
        )
      )
    })

    # Render thresholds display
    output$threshold_display <- shiny::renderUI({
      a <- analysis()
      lang <- language()

      if (is.null(a) || is.null(a@thresholds)) {
        return(
          shiny::div(
            class = "text-muted small",
            tr("message_no_thresholds", lang)
          )
        )
      }

      th <- a@thresholds

      shiny::tags$table(
        class = "table table-sm",
        shiny::tags$thead(
          shiny::tags$tr(
            shiny::tags$th(""),
            shiny::tags$th(paste("VO2", tr("unit_ml_min", lang))),
            shiny::tags$th(tr("hr", lang)),
            shiny::tags$th(tr("power", lang))
          )
        ),
        shiny::tags$tbody(
          # VT1
          if (!is.null(th@vt1_vo2) && !is.na(th@vt1_vo2)) {
            shiny::tags$tr(
              shiny::tags$td(shiny::strong(tr("vt1", lang))),
              shiny::tags$td(round(th@vt1_vo2)),
              shiny::tags$td(if (!is.null(th@vt1_hr)) round(th@vt1_hr) else "--"),
              shiny::tags$td(if (!is.null(th@vt1_power)) round(th@vt1_power) else "--")
            )
          },
          # VT2
          if (!is.null(th@vt2_vo2) && !is.na(th@vt2_vo2)) {
            shiny::tags$tr(
              shiny::tags$td(shiny::strong(tr("vt2", lang))),
              shiny::tags$td(round(th@vt2_vo2)),
              shiny::tags$td(if (!is.null(th@vt2_hr)) round(th@vt2_hr) else "--"),
              shiny::tags$td(if (!is.null(th@vt2_power)) round(th@vt2_power) else "--")
            )
          }
        )
      )
    })

    # Render stage summary table
    output$stage_table <- DT::renderDataTable({
      a <- analysis()
      lang <- language()

      if (is.null(a) || is.null(a@stage_summary)) {
        return(NULL)
      }

      # Detect modality for column selection
      is_treadmill <- (!is.null(a@protocol_config) && a@protocol_config@modality == "treadmill") ||
        ("speed_kmh" %in% names(a@data@breaths)) ||
        (!is.null(a@peaks) && !is.null(a@peaks@speed_peak))

      # Format the stage summary - show Speed for treadmill, Power for cycling
      if (is_treadmill && "speed_kmh" %in% names(a@stage_summary)) {
        df <- a@stage_summary |>
          dplyr::select(
            Stage = dplyr::any_of("stage"),
            Speed = dplyr::any_of("speed_kmh"),
            VO2 = dplyr::any_of("vo2_ml"),
            HR = dplyr::any_of("hr_bpm"),
            VE = dplyr::any_of("ve_l"),
            RER = dplyr::any_of("rer")
          )
      } else {
        df <- a@stage_summary |>
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
        dplyr::mutate(
          dplyr::across(dplyr::where(is.numeric), ~ round(.x, 1))
        )

      dt <- DT::datatable(
        df,
        options = list(
          pageLength = 10,
          scrollY = "200px",
          dom = "t",
          ordering = FALSE
        ),
        rownames = FALSE,
        class = "compact stripe"
      )

      # Color-code RER values above 1.10 (high intensity)
      if ("RER" %in% names(df)) {
        dt <- dt |>
          DT::formatStyle("RER",
            backgroundColor = DT::styleInterval(1.10, c("transparent", "#fff3cd"))
          )
      }

      dt
    })

    # CSV data export handler
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

    # Return analysis
    list(
      analysis = analysis
    )
  })
}

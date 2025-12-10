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
      # Peak values
      shiny::uiOutput(ns("peak_display")),

      shiny::hr(),

      # Thresholds
      shiny::h6(tr("threshold_results", language)),
      shiny::uiOutput(ns("threshold_display")),

      shiny::hr(),

      # Stage summary table
      shiny::h6(tr("stage_results", language)),
      DT::dataTableOutput(ns("stage_table"))
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

    # Computed analysis
    analysis <- shiny::reactive({
      data <- cpet_data()
      shiny::req(data)

      s <- settings()
      p <- participant()

      # Update participant in data if edited
      if (!is.null(p)) {
        data@participant <- p
      }

      # Find peaks with specified averaging
      peaks <- find_peaks(data, averaging = s$averaging_window)

      # Detect thresholds (if methods specified)
      thresholds <- NULL
      if (length(s$threshold_methods) > 0) {
        tryCatch({
          thresholds <- detect_thresholds(data)
        }, error = function(e) {
          # Thresholds are optional
          NULL
        })
      }

      # Extract and summarize stages
      stage_summary <- NULL
      tryCatch({
        data_with_stages <- extract_stages(data, protocol = s$protocol,
                                           stage_duration = s$stage_duration)
        stage_summary <- summarize_stages(data_with_stages)
      }, error = function(e) {
        # Stage summary is optional
        NULL
      })

      # Create analysis object
      CpetAnalysis(
        data = data,
        peaks = peaks,
        thresholds = thresholds,
        stage_summary = stage_summary
      )
    })

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
            height = "120px"
          ),

          # Power peak
          bslib::value_box(
            title = tr("power", lang),
            value = if (!is.null(peaks@power_peak)) round(peaks@power_peak) else "--",
            showcase = shiny::icon("bolt"),
            theme = "success",
            p(tr("unit_watts", lang)),
            height = "120px"
          )
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
            height = "120px"
          ),

          # RER peak
          bslib::value_box(
            title = tr("rer", lang),
            value = sprintf("%.2f", peaks@rer_peak),
            showcase = shiny::icon("lungs"),
            theme = if (peaks@rer_peak >= 1.10) "warning" else "light",
            height = "120px"
          )
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

      # Format the stage summary
      df <- a@stage_summary |>
        dplyr::select(
          Stage = dplyr::any_of("stage"),
          Power = dplyr::any_of("power_w"),
          VO2 = dplyr::any_of("vo2_ml"),
          HR = dplyr::any_of("hr_bpm"),
          VE = dplyr::any_of("ve_l"),
          RER = dplyr::any_of("rer")
        ) |>
        dplyr::mutate(
          dplyr::across(dplyr::where(is.numeric), ~ round(.x, 1))
        )

      DT::datatable(
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
    })

    # Return analysis
    list(
      analysis = analysis
    )
  })
}

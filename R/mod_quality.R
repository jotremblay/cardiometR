#' Quality Assessment Module UI
#'
#' @description
#' Displays comprehensive data quality assessment including ACSM maximal
#' exercise criteria, protocol quality, and data quality metrics.
#'
#' @param id Module namespace ID.
#' @param language Language code ("en" or "fr").
#'
#' @return A Shiny UI element.
#'
#' @keywords internal
mod_quality_ui <- function(id, language = "en") {
  ns <- shiny::NS(id)

  bslib::card(
    fill = TRUE,
    bslib::card_header(
      class = "d-flex justify-content-between align-items-center",
      shiny::div(
        shiny::icon("clipboard-check"),
        tr("quality_dashboard", language)
      )
    ),
    bslib::card_body(
      fillable = TRUE,
      class = "p-3",

      # Overall summary row
      shiny::uiOutput(ns("overall_summary")),

      shiny::hr(),

      # Maximal criteria section
      shiny::h5(
        shiny::icon("heart-pulse"),
        tr("maximal_criteria", language),
        class = "mt-3"
      ),
      shiny::uiOutput(ns("criteria_checklist")),

      shiny::hr(),

      # Protocol and Data quality side by side
      bslib::layout_columns(
        col_widths = c(6, 6),
        fill = FALSE,

        # Protocol quality
        shiny::div(
          shiny::h5(
            shiny::icon("route"),
            tr("protocol_quality", language)
          ),
          shiny::uiOutput(ns("protocol_metrics"))
        ),

        # Data quality
        shiny::div(
          shiny::h5(
            shiny::icon("wave-square"),
            tr("data_quality", language)
          ),
          shiny::uiOutput(ns("data_metrics"))
        )
      ),

      shiny::hr(),

      # Stage details (collapsible)
      bslib::accordion(
        id = ns("stage_accordion"),
        bslib::accordion_panel(
          title = tr("stage_details", language),
          icon = shiny::icon("table"),
          DT::dataTableOutput(ns("stage_table"))
        ),
        bslib::accordion_panel(
          title = tr("recommendations", language),
          icon = shiny::icon("lightbulb"),
          shiny::uiOutput(ns("recommendations"))
        )
      ),

      # Optional RPE and Lactate inputs
      shiny::hr(),
      shiny::h6(tr("optional_criteria", language)),
      bslib::layout_columns(
        col_widths = c(4, 4, 4),
        shiny::numericInput(
          ns("rpe_input"),
          tr("rpe_label", language),
          value = NA,
          min = 6,
          max = 20,
          step = 1
        ),
        shiny::numericInput(
          ns("lactate_input"),
          tr("lactate_label", language),
          value = NA,
          min = 0,
          max = 30,
          step = 0.1
        ),
        shiny::div(
          class = "d-flex align-items-end h-100",
          shiny::actionButton(
            ns("update_criteria"),
            tr("update_assessment", language),
            class = "btn-primary btn-sm"
          )
        )
      )
    )
  )
}


#' Quality Assessment Module Server
#'
#' @param id Module namespace ID.
#' @param language Reactive language value.
#' @param cpet_data Reactive CpetData object from upload module.
#' @param analysis Reactive CpetAnalysis object from results module.
#'
#' @return A list with reactive values:
#'   - `quality`: Reactive QualityAssessment object.
#'
#' @keywords internal
mod_quality_server <- function(id, language, cpet_data, analysis) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Reactive for optional criteria
    rpe_value <- shiny::reactiveVal(NULL)
    lactate_value <- shiny::reactiveVal(NULL)

    # Update optional criteria when button clicked
    shiny::observeEvent(input$update_criteria, {
      rpe_value(if (is.na(input$rpe_input)) NULL else input$rpe_input)
      lactate_value(if (is.na(input$lactate_input)) NULL else input$lactate_input)
    })

    # Compute quality assessment
    quality <- shiny::reactive({
      data <- cpet_data()
      shiny::req(data)

      assess_quality(data, rpe = rpe_value(), lactate = lactate_value())
    })

    # Render overall summary
    output$overall_summary <- shiny::renderUI({
      q <- quality()
      lang <- language()

      if (is.null(q)) {
        return(
          shiny::div(
            class = "text-muted text-center py-4",
            tr("upload_prompt", lang)
          )
        )
      }

      # Grade color
      grade_color <- switch(q@overall_grade,
        "A" = "success",
        "B" = "primary",
        "C" = "warning",
        "D" = "warning",
        "F" = "danger"
      )

      # Determination color
      det_color <- switch(q@exercise_criteria@determination,
        "maximal" = "success",
        "likely_maximal" = "primary",
        "submaximal" = "warning",
        "indeterminate" = "secondary"
      )

      bslib::layout_columns(
        col_widths = c(4, 4, 4),
        fill = FALSE,

        # Overall grade
        bslib::value_box(
          title = tr("overall_grade", lang),
          value = q@overall_grade,
          showcase = shiny::icon("award"),
          theme = grade_color,
          p(sprintf("%.0f/100", q@overall_score))
        ),

        # Data quality score
        bslib::value_box(
          title = tr("data_score", lang),
          value = sprintf("%.0f", q@data_quality@overall_score),
          showcase = shiny::icon("chart-simple"),
          theme = if (q@data_quality@overall_score >= 75) "primary" else "warning",
          p(tr(paste0("rating_", q@data_quality@overall_rating), lang))
        ),

        # Exercise determination
        bslib::value_box(
          title = tr("effort_level", lang),
          value = tr(paste0("determination_", q@exercise_criteria@determination), lang),
          showcase = shiny::icon("person-running"),
          theme = det_color,
          p(sprintf("%d/%d %s",
                    q@exercise_criteria@criteria_met,
                    q@exercise_criteria@criteria_available,
                    tr("criteria_met_short", lang)))
        )
      )
    })

    # Render criteria checklist
    output$criteria_checklist <- shiny::renderUI({
      q <- quality()
      lang <- language()

      if (is.null(q)) return(NULL)

      ec <- q@exercise_criteria

      # Build checklist items
      items <- list()

      # RER criterion
      items[[length(items) + 1]] <- criterion_item(
        met = ec@rer_met,
        label = sprintf("%s >= 1.10", tr("rer", lang)),
        value = sprintf("%.2f", ec@peak_rer),
        lang = lang
      )

      # HR criterion
      if (!is.null(ec@hr_met)) {
        items[[length(items) + 1]] <- criterion_item(
          met = ec@hr_met,
          label = sprintf("%s >= 85%% %s", tr("hr", lang), tr("of_predicted", lang)),
          value = sprintf("%.0f %s (%.0f%%)",
                          ec@peak_hr, tr("unit_bpm", lang), ec@hr_pct_predicted),
          lang = lang
        )
      }

      # Plateau criterion
      items[[length(items) + 1]] <- criterion_item(
        met = ec@plateau_met,
        label = tr("vo2_plateau", lang),
        value = if (ec@vo2_plateau_detected) {
          sprintf("%s = %.0f %s", "\u0394VO2", ec@vo2_plateau_delta, tr("unit_ml_min", lang))
        } else {
          tr("not_detected", lang)
        },
        lang = lang
      )

      # RPE criterion (optional)
      if (!is.null(ec@rpe_met)) {
        items[[length(items) + 1]] <- criterion_item(
          met = ec@rpe_met,
          label = sprintf("%s >= 17", tr("rpe", lang)),
          value = sprintf("%.0f", ec@rpe_reported),
          lang = lang
        )
      } else {
        items[[length(items) + 1]] <- criterion_item(
          met = NULL,
          label = sprintf("%s >= 17", tr("rpe", lang)),
          value = tr("not_recorded", lang),
          lang = lang
        )
      }

      # Lactate criterion (optional)
      if (!is.null(ec@lactate_met)) {
        items[[length(items) + 1]] <- criterion_item(
          met = ec@lactate_met,
          label = sprintf("%s >= 8.0 mmol/L", tr("lactate", lang)),
          value = sprintf("%.1f mmol/L", ec@lactate_mmol),
          lang = lang
        )
      } else {
        items[[length(items) + 1]] <- criterion_item(
          met = NULL,
          label = sprintf("%s >= 8.0 mmol/L", tr("lactate", lang)),
          value = tr("not_recorded", lang),
          lang = lang
        )
      }

      # Summary line
      summary_class <- if (ec@is_maximal) "text-success fw-bold" else "text-warning"
      summary_text <- if (ec@is_maximal) {
        tr("maximal_test_confirmed", lang)
      } else {
        tr("maximal_test_not_confirmed", lang)
      }

      shiny::tagList(
        shiny::div(class = "criteria-list", items),
        shiny::hr(),
        shiny::div(
          class = summary_class,
          shiny::icon(if (ec@is_maximal) "check-circle" else "exclamation-triangle"),
          sprintf("%d/%d %s - %s",
                  ec@criteria_met, ec@criteria_available,
                  tr("criteria_met", lang), summary_text)
        )
      )
    })

    # Render protocol metrics
    output$protocol_metrics <- shiny::renderUI({
      q <- quality()
      lang <- language()

      if (is.null(q)) return(NULL)

      pq <- q@protocol_quality

      items <- list()

      # VO2/W slope
      if (!is.null(pq@actual_vo2_slope)) {
        items[[length(items) + 1]] <- metric_row(
          label = tr("vo2_slope", lang),
          value = sprintf("%.1f %s", pq@actual_vo2_slope, tr("unit_ml_min_w", lang)),
          status = if (!is.null(pq@slope_acceptable) && pq@slope_acceptable) "success" else "warning"
        )

        if (!is.null(pq@expected_vo2_slope)) {
          items[[length(items) + 1]] <- metric_row(
            label = tr("expected_slope", lang),
            value = sprintf("%.1f %s", pq@expected_vo2_slope, tr("unit_ml_min_w", lang)),
            status = "secondary"
          )
        }

        if (!is.null(pq@slope_deviation_pct)) {
          items[[length(items) + 1]] <- metric_row(
            label = tr("deviation", lang),
            value = sprintf("%+.1f%%", pq@slope_deviation_pct),
            status = if (abs(pq@slope_deviation_pct) <= 15) "success" else "warning"
          )
        }
      }

      # R-squared
      if (!is.null(pq@r_squared)) {
        items[[length(items) + 1]] <- metric_row(
          label = "R\u00B2",
          value = sprintf("%.3f", pq@r_squared),
          status = if (pq@r_squared >= 0.90) "success" else "warning"
        )
      }

      # Exercise duration
      if (!is.null(pq@exercise_duration_s)) {
        duration_min <- pq@exercise_duration_s / 60
        items[[length(items) + 1]] <- metric_row(
          label = tr("exercise_duration", lang),
          value = sprintf("%.1f %s", duration_min, tr("unit_minutes", lang)),
          status = if (!is.null(pq@duration_optimal) && pq@duration_optimal) "success" else "warning"
        )
      }

      # Overall rating
      items[[length(items) + 1]] <- shiny::hr()
      items[[length(items) + 1]] <- metric_row(
        label = tr("rating", lang),
        value = tr(paste0("rating_", pq@overall_rating), lang),
        status = switch(pq@overall_rating,
          "excellent" = "success",
          "good" = "primary",
          "acceptable" = "warning",
          "danger"
        ),
        bold = TRUE
      )

      shiny::div(class = "metrics-list", items)
    })

    # Render data metrics
    output$data_metrics <- shiny::renderUI({
      q <- quality()
      lang <- language()

      if (is.null(q)) return(NULL)

      dq <- q@data_quality

      items <- list()

      # Aberrant breaths
      items[[length(items) + 1]] <- metric_row(
        label = tr("aberrant_breaths", lang),
        value = sprintf("%.1f%% (n=%d)", dq@pct_aberrant, dq@n_aberrant),
        status = if (dq@aberrant_acceptable) "success" else "warning"
      )

      # Missing HR
      if (!is.null(dq@pct_missing_hr)) {
        items[[length(items) + 1]] <- metric_row(
          label = tr("missing_hr", lang),
          value = sprintf("%.1f%%", dq@pct_missing_hr),
          status = if (!is.null(dq@hr_acceptable) && dq@hr_acceptable) "success" else "warning"
        )
      }

      # Baseline CV
      if (!is.null(dq@baseline_vo2_cv)) {
        items[[length(items) + 1]] <- metric_row(
          label = tr("baseline_cv", lang),
          value = sprintf("%.1f%%", dq@baseline_vo2_cv),
          status = if (!is.null(dq@baseline_stable) && dq@baseline_stable) "success" else "warning"
        )
      }

      # Calibration drift
      items[[length(items) + 1]] <- metric_row(
        label = tr("calibration_drift", lang),
        value = if (dq@calibration_drift_detected) tr("detected", lang) else tr("not_detected", lang),
        status = if (dq@calibration_drift_detected) "warning" else "success"
      )

      # Signal quality
      items[[length(items) + 1]] <- metric_row(
        label = tr("signal_quality", lang),
        value = tr(paste0("rating_", dq@signal_quality_rating), lang),
        status = switch(dq@signal_quality_rating,
          "excellent" = "success",
          "good" = "primary",
          "acceptable" = "warning",
          "danger"
        )
      )

      # Overall score
      items[[length(items) + 1]] <- shiny::hr()
      items[[length(items) + 1]] <- metric_row(
        label = tr("score", lang),
        value = sprintf("%.0f/100", dq@overall_score),
        status = switch(dq@overall_rating,
          "excellent" = "success",
          "good" = "primary",
          "acceptable" = "warning",
          "danger"
        ),
        bold = TRUE
      )

      shiny::div(class = "metrics-list", items)
    })

    # Render stage table
    output$stage_table <- DT::renderDataTable({
      q <- quality()
      lang <- language()

      if (is.null(q) || is.null(q@protocol_quality@stage_details)) {
        return(NULL)
      }

      df <- q@protocol_quality@stage_details |>
        dplyr::mutate(
          dplyr::across(dplyr::where(is.numeric), ~ round(.x, 1))
        ) |>
        dplyr::rename(
          !!tr("power", lang) := power_w,
          !!tr("expected_vo2", lang) := expected_vo2,
          !!tr("actual_vo2", lang) := actual_vo2,
          !!paste0(tr("deviation", lang), " (%)") := deviation_pct
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
      ) |>
        DT::formatStyle(
          columns = ncol(df),
          color = DT::styleInterval(c(-15, 15), c("red", "green", "red"))
        )
    })

    # Render recommendations
    output$recommendations <- shiny::renderUI({
      q <- quality()
      lang <- language()

      if (is.null(q)) return(NULL)

      recs <- q@data_quality@recommendations

      if (length(recs) == 0) {
        return(shiny::div(class = "text-muted", tr("no_recommendations", lang)))
      }

      shiny::tags$ul(
        class = "recommendations-list",
        lapply(recs, function(r) {
          shiny::tags$li(
            shiny::icon("lightbulb", class = "text-warning me-2"),
            r
          )
        })
      )
    })

    # Return quality assessment
    list(
      quality = quality
    )
  })
}


#' Create Criterion Item
#'
#' @param met Logical or NULL indicating if criterion was met
#' @param label Criterion label
#' @param value Achieved value
#' @param lang Language code
#' @return Shiny tag
#' @keywords internal
criterion_item <- function(met, label, value, lang) {
  if (is.null(met)) {
    icon_tag <- shiny::icon("circle", class = "text-secondary")
    text_class <- "text-secondary"
  } else if (met) {
    icon_tag <- shiny::icon("check-circle", class = "text-success")
    text_class <- "text-success"
  } else {
    icon_tag <- shiny::icon("times-circle", class = "text-danger")
    text_class <- "text-danger"
  }

  shiny::div(
    class = paste("criterion-item d-flex justify-content-between py-1", text_class),
    shiny::span(icon_tag, " ", label),
    shiny::span(class = "fw-bold", value)
  )
}


#' Create Metric Row
#'
#' @param label Metric label
#' @param value Metric value
#' @param status Bootstrap color (success, warning, danger, etc.)
#' @param bold Whether to bold the value
#' @return Shiny tag
#' @keywords internal
metric_row <- function(label, value, status = "secondary", bold = FALSE) {
  value_class <- paste0("text-", status)
  if (bold) value_class <- paste(value_class, "fw-bold")

  icon_tag <- switch(status,
    success = shiny::icon("check-circle", class = "text-success me-1"),
    warning = shiny::icon("exclamation-triangle", class = "text-warning me-1"),
    danger = shiny::icon("times-circle", class = "text-danger me-1"),
    NULL
  )

  shiny::div(
    class = "d-flex justify-content-between py-1",
    shiny::span(label),
    shiny::span(class = value_class, icon_tag, value)
  )
}

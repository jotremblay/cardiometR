#' Upload Module UI
#'
#' @param id Module namespace ID.
#' @param language Language code ("en" or "fr").
#'
#' @return A Shiny UI element.
#'
#' @keywords internal
mod_upload_ui <- function(id, language = "en") {
  ns <- shiny::NS(id)

  bslib::layout_columns(
    col_widths = c(6, 6, 12),
    fill = FALSE,

    # Upload card
    bslib::card(
      bslib::card_header(
        class = "bg-primary text-white",
        shiny::icon("upload"),
        shiny::span(id = ns("upload_header"), tr("nav_upload", language))
      ),
      bslib::card_body(
        shiny::div(
          class = "upload-dropzone",
          shiny::fileInput(
            ns("file"),
            label = tr("upload_prompt", language),
            accept = c(".xlsx", ".xls"),
            buttonLabel = tr("browse", language),
            placeholder = if (identical(language, "fr")) "Aucun fichier s\u00e9lectionn\u00e9" else "No file selected",
            width = "100%"
          ),
          shiny::tags$small(
            id = ns("upload_hint"),
            class = "text-muted",
            tr("upload_hint", language)
          )
        )
      )
    ),

    # Validation status card
    bslib::card(
      bslib::card_header(
        shiny::icon("check-circle"),
        shiny::span(id = ns("validation_header"), tr("validation_status", language))
      ),
      bslib::card_body(
        shiny::uiOutput(ns("validation_display"))
      )
    ),

    # How the file was read. Collapsed by default: it only matters when
    # something looks wrong, but then it is the first place to look.
    shiny::uiOutput(ns("import_report_display"))
  )
}

#' Upload Module Server
#'
#' @param id Module namespace ID.
#' @param language Reactive language value.
#'
#' @return A list with reactive values:
#'   - `cpet_data`: Reactive CpetData object (or NULL).
#'   - `validation`: Reactive ValidationReport object (or NULL).
#'
#' @keywords internal
mod_upload_server <- function(id, language) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Reactive values to store results
    cpet_data <- shiny::reactiveVal(NULL)
    validation <- shiny::reactiveVal(NULL)

    # Update static text when language changes
    shiny::observeEvent(language(), {
      lang <- language()
      session$sendCustomMessage("update_text", as.list(stats::setNames(
        c(tr("nav_upload", lang), tr("validation_status", lang), tr("upload_hint", lang)),
        c(ns("upload_header"), ns("validation_header"), ns("upload_hint"))
      )))
      session$sendCustomMessage("update_input_label", list(
        id = ns("file"),
        label = tr("upload_prompt", lang)
      ))
      session$sendCustomMessage("update_button_label", list(
        selector = sprintf(".shiny-input-container:has(#%s) .btn-file", ns("file")),
        label = tr("browse", lang)
      ))
      # Re-run validation so the already-uploaded file's messages switch
      # language along with the UI.
      if (!is.null(cpet_data())) {
        validation(validate(cpet_data(), language = lang))
      }
    })

    # Process uploaded file
    shiny::observeEvent(input$file, {
      shiny::req(input$file)

      # Localize Shiny's built-in "Upload complete" progress-bar text.
      session$sendCustomMessage("localize_upload_progress", list(
        id = ns("file"),
        text = tr("upload_complete", language())
      ))

      tryCatch({
        # Read CPET data
        data <- read_cpet(input$file$datapath)

        # Validate data
        val <- validate(data, language = language())

        # Store results
        cpet_data(data)
        validation(val)

        # Show success notification
        shiny::showNotification(
          tr("file_valid", language()),
          type = "message",
          duration = 3
        )

      }, error = function(e) {
        # Show error notification
        shiny::showNotification(
          paste(tr("file_invalid", language()), e$message),
          type = "error",
          duration = 5
        )
        cpet_data(NULL)
        validation(NULL)
      })
    })

    # Render the import report
    output$import_report_display <- shiny::renderUI({
      report <- cpet_import_report(cpet_data())
      if (is.null(report)) {
        return(NULL)
      }
      import_report_panel(report, language())
    })

    # Render validation display
    output$validation_display <- shiny::renderUI({
      val <- validation()
      lang <- language()

      if (is.null(val)) {
        return(
          shiny::div(
            class = "empty-state",
            shiny::icon("cloud-arrow-up"),
            shiny::p(tr("upload_prompt", lang))
          )
        )
      }

      # Build validation summary
      shiny::tagList(
        # Overall status
        if (val@is_valid) {
          shiny::div(
            class = "alert alert-success",
            shiny::icon("check-circle"),
            tr("validation_passed", lang)
          )
        } else {
          shiny::div(
            class = "alert alert-danger",
            shiny::icon("exclamation-circle"),
            tr("validation_errors", lang)
          )
        },

        # Errors
        if (length(val@errors) > 0) {
          shiny::div(
            class = "mb-3",
            shiny::tags$strong(class = "text-danger", tr("validation_errors", lang)),
            shiny::tags$ul(
              class = "text-danger",
              lapply(val@errors, function(e) shiny::tags$li(e))
            )
          )
        },

        # Warnings
        if (length(val@warnings) > 0) {
          shiny::div(
            class = "mb-3",
            shiny::tags$strong(class = "text-warning", tr("validation_warnings", lang)),
            shiny::tags$ul(
              class = "text-warning",
              lapply(val@warnings, function(w) shiny::tags$li(w))
            )
          )
        },

        # Info
        if (length(val@info) > 0) {
          shiny::div(
            class = "mb-3",
            shiny::tags$strong(class = "text-info", tr("info_label", lang)),
            shiny::tags$ul(
              class = "text-info small",
              lapply(val@info, function(i) shiny::tags$li(i))
            )
          )
        },

        # File info
        if (!is.null(cpet_data())) {
          data <- cpet_data()
          shiny::div(
            class = "mt-3 pt-3 border-top",
            shiny::tags$dl(
              class = "row mb-0",
              shiny::tags$dt(class = "col-sm-4", tr("participant_name", lang)),
              shiny::tags$dd(class = "col-sm-8", data@participant@name),
              shiny::tags$dt(class = "col-sm-4", tr("test_date", lang)),
              shiny::tags$dd(class = "col-sm-8", format(data@metadata@test_date, "%Y-%m-%d")),
              shiny::tags$dt(class = "col-sm-4", tr("breaths_label", lang)),
              shiny::tags$dd(class = "col-sm-8", nrow(data@breaths)),
              shiny::tags$dt(class = "col-sm-4", tr("device", lang)),
              shiny::tags$dd(class = "col-sm-8", data@metadata@device)
            )
          )
        },

        # Move on to the analysis settings.
        if (!is.null(cpet_data())) {
          shiny::div(
            class = "d-flex justify-content-end mt-3",
            shiny::actionButton(
              ns("go_configure"),
              label = tr("configure_analysis", lang),
              icon = shiny::icon("arrow-right"),
              class = "btn-primary btn-sm"
            )
          )
        }
      )
    })


    # Return reactive values
    list(
      cpet_data = cpet_data,
      validation = validation,
      go_configure = shiny::reactive(input$go_configure)
    )
  })
}


#' Build the "how this file was read" panel
#'
#' Kept out of the server function so it can be tested on its own, and so the
#' server stays readable.
#'
#' @param report A [CpetImportReport].
#' @param language Language code.
#'
#' @return A collapsed bslib accordion, or `NULL` when there is no report.
#'
#' @keywords internal
import_report_panel <- function(report, language = "en") {
  if (is.null(report)) {
    return(NULL)
  }

  mapped <- report@columns[report@columns$status == "mapped", , drop = FALSE]
  converted <- mapped[!is.na(mapped$factor) & mapped$factor != 1, , drop = FALSE]

  header_line <- function(label, value) {
    shiny::tags$div(
      class = "d-flex gap-2",
      shiny::tags$span(class = "text-muted", label),
      shiny::tags$span(value)
    )
  }

  summary_block <- shiny::tagList(
    header_line(tr("import_format", language), report@dialect_label),
    if (!is.na(report@sheet)) {
      header_line(tr("import_sheet", language), report@sheet)
    },
    header_line(
      tr("import_layout", language),
      sprintf(tr("import_layout_detail", language),
              report@layout$header_row, report@layout$data_row)
    ),
    shiny::tags$p(
      class = "mt-2 mb-1 fw-semibold",
      sprintf(tr("import_columns_recognised", language), nrow(mapped))
    )
  )

  columns_table <- shiny::tags$table(
    class = "table table-sm mb-2",
    shiny::tags$thead(shiny::tags$tr(
      shiny::tags$th(tr("import_col_source", language)),
      shiny::tags$th(tr("import_col_canonical", language)),
      shiny::tags$th(tr("import_col_unit", language))
    )),
    shiny::tags$tbody(lapply(seq_len(nrow(mapped)), function(i) {
      row <- mapped[i, ]
      unit <- if (!is.na(row$factor) && row$factor != 1) {
        sprintf("%s to %s", row$unit_from, row$unit_to)
      } else {
        row$unit_to %||% ""
      }
      shiny::tags$tr(
        shiny::tags$td(shiny::tags$code(row$source)),
        shiny::tags$td(row$canonical),
        shiny::tags$td(class = "text-muted", if (is.na(unit)) "" else unit)
      )
    }))
  )

  converted_block <- if (nrow(converted) > 0) {
    shiny::tagList(
      shiny::tags$p(class = "fw-semibold mb-1",
                    tr("import_units_converted", language)),
      shiny::tags$ul(lapply(seq_len(nrow(converted)), function(i) {
        row <- converted[i, ]
        shiny::tags$li(sprintf(
          "%s: %s to %s (x%s)",
          row$source, row$unit_from, row$unit_to, signif(row$factor, 6)
        ))
      }))
    )
  }

  vocab_block <- if (!is.null(report@vocab) && nrow(report@vocab) > 0) {
    shiny::tagList(
      shiny::tags$p(class = "fw-semibold mb-1",
                    tr("import_phase_labels", language)),
      shiny::tags$ul(lapply(seq_len(nrow(report@vocab)), function(i) {
        row <- report@vocab[i, ]
        target <- if (is.na(row$canonical)) {
          tr("import_unmapped_phase", language)
        } else {
          row$canonical
        }
        unit <- if (row$n == 1L) {
          tr("import_phase_row", language)
        } else {
          tr("import_phase_rows", language)
        }
        shiny::tags$li(sprintf("%s to %s (%d %s)", row$raw, target,
                               row$n, unit))
      }))
    )
  }

  unknown_block <- shiny::tagList(
    shiny::tags$p(class = "fw-semibold mb-1", tr("import_unrecognised", language)),
    if (length(report@unknown) == 0) {
      shiny::tags$p(class = "text-muted",
                    tr("import_unrecognised_none", language))
    } else {
      shiny::tags$ul(lapply(report@unknown, function(name) {
        hint <- report@suggestions[[name]]
        shiny::tags$li(
          shiny::tags$code(name),
          if (!is.null(hint)) {
            shiny::tags$span(class = "text-muted",
                             sprintf(" %s %s?", tr("import_suggestion", language),
                                     paste(hint, collapse = ", ")))
          }
        )
      }))
    },
    if (length(report@ignored) > 0) {
      shiny::tags$p(
        class = "text-muted small",
        sprintf(tr("import_ignored", language), length(report@ignored))
      )
    }
  )

  warnings_block <- if (length(report@warnings) > 0) {
    shiny::tagList(
      shiny::tags$p(class = "fw-semibold mb-1", tr("import_warnings", language)),
      shiny::tags$ul(lapply(report@warnings, shiny::tags$li))
    )
  }

  bslib::accordion(
    open = FALSE,
    bslib::accordion_panel(
      title = tr("import_report_title", language),
      icon = shiny::icon("file-import"),
      summary_block,
      columns_table,
      converted_block,
      vocab_block,
      unknown_block,
      warnings_block
    )
  )
}

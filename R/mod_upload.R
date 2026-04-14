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
    col_widths = c(6, 6),
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
    )
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
            shiny::tags$strong(class = "text-info", "Info"),
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
              shiny::tags$dt(class = "col-sm-4", "Breaths"),
              shiny::tags$dd(class = "col-sm-8", nrow(data@breaths))
            )
          )
        }
      )
    })

    # Return reactive values
    list(
      cpet_data = cpet_data,
      validation = validation
    )
  })
}

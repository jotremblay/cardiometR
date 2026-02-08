#' Report Module UI
#'
#' @param id Module namespace ID.
#' @param language Language code ("en" or "fr").
#'
#' @return A Shiny UI element.
#'
#' @keywords internal
mod_report_ui <- function(id, language = "en") {
  ns <- shiny::NS(id)

  bslib::layout_columns(
    col_widths = c(5, 7),
    fill = FALSE,

    # Configuration panel
    bslib::card(
      bslib::card_header(
        shiny::icon("cog"),
        tr("report_config", language)
      ),
      bslib::card_body(
        shiny::textInput(
          ns("institution"),
          label = tr("institution", language),
          placeholder = "e.g., University of Montreal"
        ),
        shiny::textInput(
          ns("lab_name"),
          label = tr("lab_name", language),
          value = "Laboratoire de physiologie de l'exercice et de bio\u00e9nerg\u00e9tique appliqu\u00e9e (LPEBA)",
          placeholder = "e.g., Exercise Physiology Lab"
        ),
        shiny::textInput(
          ns("lab_url"),
          label = "URL",
          value = "https://bioenergeticslab.ca/",
          placeholder = "https://..."
        ),

        # Logo selection
        shiny::selectInput(
          ns("logo_choice"),
          label = tr("logo", language),
          choices = c(
            "Universit\u00e9 de Montr\u00e9al" = "udem",
            "Centre \u00c9PIC - ICM" = "epic",
            "None" = "none",
            "Custom..." = "custom"
          ),
          selected = "udem"
        ),

        # Conditional file upload for custom logo
        shiny::conditionalPanel(
          condition = sprintf("input['%s'] == 'custom'", ns("logo_choice")),
          shiny::fileInput(
            ns("logo_custom"),
            label = tr("logo_upload", language),
            accept = c(".png", ".jpg", ".jpeg")
          )
        ),

        # Logo preview
        shiny::uiOutput(ns("logo_preview")),

        shiny::textInput(
          ns("technician"),
          label = tr("technician", language)
        ),
        shiny::dateInput(
          ns("signature_date"),
          label = tr("signature_date", language),
          value = Sys.Date(),
          language = if (language == "fr") "fr" else "en"
        ),
        shiny::textAreaInput(
          ns("clinical_notes"),
          label = tr("clinical_notes", language),
          rows = 4,
          placeholder = tr("clinical_notes_placeholder", language)
        )
      ),
      bslib::card_footer(
        shiny::uiOutput(ns("generate_report_btn"))
      )
    ),

    # Preview panel
    bslib::card(
      bslib::card_header(
        shiny::icon("eye"),
        tr("report_preview", language)
      ),
      bslib::card_body(
        shiny::uiOutput(ns("preview_content"))
      )
    )
  )
}

#' Report Module Server
#'
#' @param id Module namespace ID.
#' @param language Reactive language value.
#' @param analysis Reactive CpetAnalysis object from results module.
#'
#' @keywords internal
mod_report_server <- function(id, language, analysis, settings = shiny::reactive(list())) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Get the selected logo path
    logo_path <- shiny::reactive({
      choice <- input$logo_choice %||% "udem"

      if (choice == "udem") {
        system.file("assets", "Ec-_kinesiologie_-act_-phy_officiel-RVB.png", package = "cardiometR")
      } else if (choice == "epic") {
        system.file("assets", "Centre_EPIC_ICM.jpg", package = "cardiometR")
      } else if (choice == "custom" && !is.null(input$logo_custom)) {
        input$logo_custom$datapath
      } else {
        NULL
      }
    })

    # Validate custom logo upload (PNG/JPEG magic bytes)
    shiny::observeEvent(input$logo_custom, {
      file_info <- input$logo_custom
      shiny::req(file_info)
      raw_bytes <- readBin(file_info$datapath, "raw", n = 8)
      is_png <- identical(raw_bytes[1:4], as.raw(c(0x89, 0x50, 0x4E, 0x47)))
      is_jpeg <- identical(raw_bytes[1:2], as.raw(c(0xFF, 0xD8)))
      if (!is_png && !is_jpeg) {
        shiny::showNotification(
          tr("file_invalid", language()),
          type = "warning",
          duration = 5
        )
      }
    })

    # Render logo preview
    output$logo_preview <- shiny::renderUI({
      path <- logo_path()

      if (is.null(path) || path == "" || !file.exists(path)) {
        return(NULL)
      }

      if (input$logo_choice %in% c("udem", "epic")) {
        src <- paste0("cardiometR/", basename(path))
      } else {
        # Encode custom upload as base64 data URI
        img_bytes <- readBin(path, "raw", file.info(path)$size)
        img_b64 <- jsonlite::base64_enc(img_bytes)
        ext <- tolower(tools::file_ext(input$logo_custom$name))
        mime <- switch(ext,
          png = "image/png",
          jpg = "image/jpeg",
          jpeg = "image/jpeg",
          "image/png"
        )
        src <- paste0("data:", mime, ";base64,", img_b64)
      }

      shiny::div(
        class = "mt-2 mb-3 p-2 bg-light rounded text-center",
        shiny::img(
          src = src,
          alt = "Logo preview",
          style = "max-height: 60px; max-width: 100%;",
          class = "img-fluid"
        )
      )
    })

    output$generate_report_btn <- shiny::renderUI({
      if (is_typst_available()) {
        shiny::downloadButton(
          ns("generate_report"),
          label = tr("generate_report", language()),
          class = "btn-primary w-100",
          icon = shiny::icon("file-pdf")
        )
      } else {
        shiny::tagList(
          shiny::tags$button(
            type = "button",
            class = "btn btn-secondary w-100",
            disabled = "disabled",
            shiny::icon("file-pdf"),
            tr("generate_report", language())
          ),
          shiny::tags$small(
            class = "text-muted d-block mt-2",
            tr("report_pdf_unavailable", language())
          )
        )
      }
    })

    # Build ReportConfig from inputs
    report_config <- shiny::reactive({
      ReportConfig(
        language = language(),
        institution = if (nchar(input$institution %||% "") > 0) input$institution else NULL,
        lab_name = if (nchar(input$lab_name %||% "") > 0) input$lab_name else NULL,
        lab_url = if (nchar(input$lab_url %||% "") > 0) input$lab_url else NULL,
        logo_path = logo_path(),
        technician = if (nchar(input$technician %||% "") > 0) input$technician else NULL
      )
    })

    # Preview content
    output$preview_content <- shiny::renderUI({
      a <- analysis()
      lang <- language()

      if (is.null(a)) {
        return(
          shiny::div(
            class = "text-muted text-center py-5",
            shiny::icon("file-circle-question", class = "fa-3x mb-3"),
            shiny::p(tr("upload_prompt", lang))
          )
        )
      }

      p <- a@data@participant
      m <- a@data@metadata
      peaks <- a@peaks

      shiny::tagList(
        # Header preview
        shiny::div(
          class = "border-bottom pb-3 mb-3",
          if (nchar(input$institution %||% "") > 0) {
            shiny::h5(class = "text-primary", input$institution)
          },
          if (nchar(input$lab_name %||% "") > 0) {
            shiny::p(class = "text-muted mb-0", input$lab_name)
          }
        ),

        # Title
        shiny::h4(class = "text-center mb-3", tr("title", lang)),

        # Patient info
        shiny::div(
          class = "row mb-3",
          shiny::div(
            class = "col-6",
            shiny::tags$strong(tr("section_patient", lang)),
            shiny::tags$dl(
              class = "row mb-0 small",
              shiny::tags$dt(class = "col-5", tr("participant_name", lang)),
              shiny::tags$dd(class = "col-7", p@name),
              shiny::tags$dt(class = "col-5", tr("participant_age", lang)),
              shiny::tags$dd(class = "col-7", paste(p@age, tr("unit_years", lang))),
              shiny::tags$dt(class = "col-5", tr("participant_sex", lang)),
              shiny::tags$dd(class = "col-7", p@sex)
            )
          ),
          shiny::div(
            class = "col-6",
            shiny::tags$strong(tr("section_test", lang)),
            shiny::tags$dl(
              class = "row mb-0 small",
              shiny::tags$dt(class = "col-5", tr("test_date", lang)),
              shiny::tags$dd(class = "col-7", format(m@test_date, "%Y-%m-%d")),
              shiny::tags$dt(class = "col-5", tr("protocol", lang)),
              shiny::tags$dd(class = "col-7", m@protocol),
              shiny::tags$dt(class = "col-5", tr("device", lang)),
              shiny::tags$dd(class = "col-7", m@device)
            )
          )
        ),

        # Peak values summary
        shiny::div(
          class = "bg-light p-3 rounded",
          shiny::tags$strong(tr("peak_values", lang)),
          shiny::tags$ul(
            class = "mb-0 mt-2",
            if (!is.null(peaks)) {
              shiny::tagList(
                shiny::tags$li(sprintf("VO2 peak: %.1f mL/kg/min", peaks@vo2_kg_peak)),
                if (!is.null(peaks@hr_peak)) {
                  shiny::tags$li(sprintf("%s: %d bpm", tr("hr", lang), round(peaks@hr_peak)))
                },
                if (!is.null(peaks@power_peak)) {
                  shiny::tags$li(sprintf("%s: %d W", tr("power", lang), round(peaks@power_peak)))
                },
                shiny::tags$li(sprintf("RER: %.2f", peaks@rer_peak))
              )
            } else {
              shiny::tags$li(tr("message_no_data", lang))
            }
          )
        ),

        # Thresholds if available
        if (!is.null(a@thresholds) && !is.null(a@thresholds@vt1_vo2) && !is.na(a@thresholds@vt1_vo2)) {
          shiny::div(
            class = "mt-3 pt-3 border-top",
            shiny::tags$strong(tr("threshold_results", lang)),
            shiny::tags$ul(
              class = "mb-0 mt-2",
              shiny::tags$li(sprintf("VT1: %.0f mL/min", a@thresholds@vt1_vo2)),
              if (!is.null(a@thresholds@vt2_vo2) && !is.na(a@thresholds@vt2_vo2)) {
                shiny::tags$li(sprintf("VT2: %.0f mL/min", a@thresholds@vt2_vo2))
              }
            )
          )
        },

        # Clinical notes preview
        if (nchar(input$clinical_notes %||% "") > 0) {
          shiny::div(
            class = "mt-3 pt-3 border-top",
            shiny::tags$strong(tr("clinical_notes", lang)),
            shiny::p(class = "small mt-2 mb-0 fst-italic", input$clinical_notes)
          )
        }
      )
    })

    # Download handler for PDF generation
    output$generate_report <- shiny::downloadHandler(
      filename = function() {
        a <- analysis()
        if (is.null(a)) {
          return("cpet_report.pdf")
        }
        sprintf(
          "cpet_report_%s_%s.pdf",
          gsub("[^a-zA-Z0-9]", "_", a@data@participant@name),
          format(Sys.Date(), "%Y%m%d")
        )
      },
      contentType = "application/pdf",
      content = function(file) {
        a <- analysis()
        shiny::req(a)
        if (!is_typst_available()) {
          shiny::showNotification(
            tr("report_error", language()),
            type = "error",
            duration = 6
          )
          stop("Typst/typr not available to render PDF")
        }

        shiny::withProgress(
          message = tr("generating_report", language()),
          value = 0.3,
          {
            shiny::incProgress(0.2, detail = tr("generating_graphs", language()))

            tryCatch({
              s <- settings()
              generate_report(
                analysis = a,
                output_file = file,
                config = report_config(),
                include_graphs = TRUE,
                athlete_sport = s$athlete_sport,
                athlete_level = s$athlete_level %||% "recreational",
                clinical_notes = input$clinical_notes,
                report_sections = s$report_sections,
                signature_date = input$signature_date
              )
            }, error = function(e) {
              err_msg <- conditionMessage(e)
              shiny::showNotification(
                shiny::tags$div(
                  shiny::tags$strong(tr("report_error", language())),
                  shiny::tags$pre(
                    style = "white-space: pre-wrap; font-size: 0.85em; max-height: 200px; overflow-y: auto;",
                    err_msg
                  )
                ),
                type = "error",
                duration = 10
              )
              stop(e)
            })

            shiny::incProgress(0.5, detail = tr("report_generated", language()))
          }
        )
      }
    )
  })
}

is_typst_available <- function() {
  tryCatch({
    if (requireNamespace("typr", quietly = TRUE)) {
      return(typr::typr_has_typst())
    }
    nzchar(Sys.which("typst"))
  }, error = function(e) FALSE)
}

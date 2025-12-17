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
          placeholder = "e.g., Exercise Physiology Lab"
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
        shiny::textAreaInput(
          ns("clinical_notes"),
          label = tr("clinical_notes", language),
          rows = 4,
          placeholder = tr("clinical_notes_placeholder", language)
        )
      ),
      bslib::card_footer(
        shiny::downloadButton(
          ns("generate_report"),
          label = tr("generate_report", language),
          class = "btn-primary w-100",
          icon = shiny::icon("file-pdf")
        )
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
mod_report_server <- function(id, language, analysis) {
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

    # Render logo preview
    output$logo_preview <- shiny::renderUI({
      path <- logo_path()

      if (is.null(path) || path == "" || !file.exists(path)) {
        return(NULL)
      }

      shiny::div(
        class = "mt-2 mb-3 p-2 bg-light rounded text-center",
        shiny::img(
          src = if (input$logo_choice %in% c("udem", "epic")) {
            # Use the www path for package assets
            paste0("cardiometR/", basename(path))
          } else {
            # For custom uploads, use datapath
            path
          },
          alt = "Logo preview",
          style = "max-height: 60px; max-width: 100%;",
          class = "img-fluid"
        )
      )
    })

    # Build ReportConfig from inputs
    report_config <- shiny::reactive({
      ReportConfig(
        language = language(),
        institution = if (nchar(input$institution %||% "") > 0) input$institution else NULL,
        lab_name = if (nchar(input$lab_name %||% "") > 0) input$lab_name else NULL,
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
            shiny::tags$li(sprintf("VO2 peak: %.1f mL/kg/min", peaks@vo2_kg_peak)),
            if (!is.null(peaks@hr_peak)) {
              shiny::tags$li(sprintf("%s: %d bpm", tr("hr", lang), round(peaks@hr_peak)))
            },
            if (!is.null(peaks@power_peak)) {
              shiny::tags$li(sprintf("%s: %d W", tr("power", lang), round(peaks@power_peak)))
            },
            shiny::tags$li(sprintf("RER: %.2f", peaks@rer_peak))
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
      content = function(file) {
        a <- analysis()
        shiny::req(a)

        shiny::withProgress(
          message = tr("generating_report", language()),
          value = 0.3,
          {
            shiny::incProgress(0.2, detail = tr("generating_graphs", language()))

            generate_report(
              analysis = a,
              output_file = file,
              config = report_config(),
              include_graphs = TRUE,
              clinical_notes = input$clinical_notes
            )

            shiny::incProgress(0.5, detail = tr("report_generated", language()))
          }
        )
      }
    )
  })
}

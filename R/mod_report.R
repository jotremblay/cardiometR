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
        shiny::span(
          shiny::icon("cog"),
          shiny::span(id = ns("report_config_header"), tr("report_config", language))
        )
      ),
      bslib::card_body(
        shiny::textInput(
          ns("institution"),
          label = tr("institution", language),
          value = paste0(
            "Universit\u00e9 de Montr\u00e9al \u2014 Centre \u00c9PIC, ",
            "Institut de Cardiologie de Montr\u00e9al"
          ),
          placeholder = "e.g., Universit\u00e9 de Montr\u00e9al"
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
          choices = stats::setNames(
            c("both", "udem", "epic", "none", "custom"),
            c(
              tr("logo_both", language),
              tr("logo_udem", language),
              tr("logo_epic", language),
              tr("logo_none", language),
              tr("logo_custom", language)
            )
          ),
          selected = "both"
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
        shiny::span(
          shiny::icon("eye"),
          shiny::span(id = ns("report_preview_header"), tr("report_preview", language))
        )
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

    udem_logo <- function() {
      system.file("assets", "Ec-_kinesiologie_-act_-phy_officiel-RVB.png",
                  package = "cardiometR")
    }
    epic_logo <- function() {
      system.file("assets", "Centre_EPIC_ICM.jpg", package = "cardiometR")
    }

    # Every logo the report header shows. The default pairs the two
    # institutions, which is how the reports are signed.
    logo_paths <- shiny::reactive({
      choice <- input$logo_choice %||% "both"

      switch(choice,
        both = c(udem_logo(), epic_logo()),
        udem = udem_logo(),
        epic = epic_logo(),
        custom = if (!is.null(input$logo_custom)) input$logo_custom$datapath else character(),
        character()
      )
    })

    # The report config carries a single path, so a paired header falls
    # back to the institutional logo.
    logo_path <- shiny::reactive({
      paths <- logo_paths()
      if (length(paths) == 0) NULL else paths[[1]]
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

    # Browser source for one logo file.
    logo_src <- function(path) {
      if (is.null(path) || !nzchar(path) || !file.exists(path)) return(NULL)

      if (identical(input$logo_choice, "custom")) {
        img_bytes <- readBin(path, "raw", file.info(path)$size)
        img_b64 <- jsonlite::base64_enc(img_bytes)
        ext <- tolower(tools::file_ext(input$logo_custom$name))
        mime <- switch(ext,
          png = "image/png",
          jpg = "image/jpeg",
          jpeg = "image/jpeg",
          "image/png"
        )
        paste0("data:", mime, ";base64,", img_b64)
      } else {
        paste0("cardiometR/", basename(path))
      }
    }

    # Render logo preview
    output$logo_preview <- shiny::renderUI({
      srcs <- Filter(Negate(is.null), lapply(logo_paths(), logo_src))
      if (length(srcs) == 0) return(NULL)

      shiny::div(
        class = "logo-preview",
        mapply(
          function(src, i) {
            shiny::tagList(
              if (i > 1) shiny::span(class = "logo-preview-divider"),
              shiny::img(src = src, alt = "Logo preview", class = "logo-preview-img")
            )
          },
          srcs, seq_along(srcs), SIMPLIFY = FALSE
        )
      )
    })

    output$generate_report_btn <- shiny::renderUI({
      if (is_typst_available()) {
        shiny::tagList(
          shiny::downloadButton(
            ns("generate_report"),
            label = tr("generate_report", language()),
            class = "btn-primary w-100",
            icon = shiny::icon("file-pdf")
          ),
          shiny::tags$small(
            class = "text-muted d-block mt-2 text-center",
            tr("report_render_note", language())
          )
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

    shiny::observeEvent(language(), {
      lang <- language()

      # Update card headers via JS
      session$sendCustomMessage("update_text", as.list(stats::setNames(
        c(tr("report_config", lang), tr("report_preview", lang)),
        c(ns("report_config_header"), ns("report_preview_header"))
      )))

      # Update input labels
      shiny::updateTextInput(session, "institution", label = tr("institution", lang))
      shiny::updateTextInput(session, "lab_name", label = tr("lab_name", lang))
      shiny::updateTextInput(session, "technician", label = tr("technician", lang))

      # dateInput and textAreaInput labels via JS (no native update* for label)
      session$sendCustomMessage("update_input_label",
        list(id = ns("signature_date"), label = tr("signature_date", lang))
      )
      session$sendCustomMessage("update_input_label",
        list(id = ns("clinical_notes"), label = tr("clinical_notes", lang))
      )

      # fileInput label via JS
      session$sendCustomMessage("update_input_label",
        list(id = ns("logo_custom"), label = tr("logo_upload", lang))
      )

      # Logo choice dropdown
      selected_logo <- input$logo_choice %||% "both"
      shiny::updateSelectInput(
        session,
        "logo_choice",
        label = tr("logo", lang),
        choices = stats::setNames(
          c("both", "udem", "epic", "none", "custom"),
          c(
            tr("logo_both", lang),
            tr("logo_udem", lang),
            tr("logo_epic", lang),
            tr("logo_none", lang),
            tr("logo_custom", lang)
          )
        ),
        selected = selected_logo
      )
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

      metric_cell <- function(label, value, unit) {
        shiny::div(
          class = "preview-metric",
          shiny::div(class = "preview-metric-label", label),
          shiny::div(class = "preview-metric-value", value),
          shiny::div(class = "preview-metric-unit", unit)
        )
      }
      kv <- function(key, value) {
        shiny::div(
          class = "preview-kv",
          shiny::span(class = "preview-kv-key", key),
          shiny::span(value)
        )
      }

      shiny::div(
        class = "report-preview-paper",
        # Header: the logos and the two institution lines, as printed.
        shiny::div(
          class = "preview-header",
          local({
            srcs <- Filter(Negate(is.null), lapply(logo_paths(), logo_src))
            if (length(srcs) == 0) return(NULL)
            shiny::div(
              class = "preview-header-logos",
              lapply(srcs, function(src) {
                shiny::img(src = src, alt = "", class = "preview-header-logo")
              })
            )
          }),
          shiny::div(
            class = "preview-header-text",
            if (nzchar(input$institution %||% "")) {
              shiny::div(class = "preview-institution", input$institution)
            },
            if (nzchar(input$lab_name %||% "")) {
              shiny::div(
                class = "preview-lab",
                input$lab_name,
                if (nzchar(input$lab_url %||% "")) {
                  shiny::tagList(" \u00b7 ", sub("^https?://", "", sub("/$", "", input$lab_url)))
                }
              )
            }
          )
        ),

        # Title
        shiny::h4(class = "text-center my-3", tr("title", lang)),

        # Patient and test blocks
        shiny::div(
          class = "row mb-3",
          shiny::div(
            class = "col-6",
            shiny::tags$strong(tr("section_patient", lang)),
            shiny::div(
              class = "mt-2",
              kv(tr("participant_name", lang), p@name),
              kv(tr("participant_age", lang),
                 paste(format_age(p@age), tr("unit_years", lang))),
              kv(tr("participant_sex", lang),
                 switch(p@sex,
                        "M" = tr("male", lang),
                        "F" = tr("female", lang),
                        tr("other", lang))),
              kv(tr("participant_weight", lang), paste(p@weight_kg, "kg"))
            )
          ),
          shiny::div(
            class = "col-6",
            shiny::tags$strong(tr("section_test", lang)),
            shiny::div(
              class = "mt-2",
              kv(tr("test_date", lang), format(m@test_date, "%Y-%m-%d")),
              kv(tr("protocol", lang), m@protocol),
              kv(tr("device", lang), m@device),
              kv(tr("modality_label", lang),
                 switch(settings()$modality %||% "cycling",
                        treadmill = tr("modality_treadmill", lang),
                        other = tr("modality_other", lang),
                        tr("modality_cycling", lang)))
            )
          )
        ),

        # Peak values as a metric row
        shiny::div(
          class = "preview-panel",
          shiny::tags$strong(tr("peak_values", lang)),
          if (is.null(peaks)) {
            shiny::p(class = "small text-muted mb-0 mt-2", tr("message_no_data", lang))
          } else {
            shiny::div(
              class = "preview-metric-row",
              metric_cell("VO2", sprintf("%.1f", peaks@vo2_kg_peak),
                          tr("unit_ml_kg_min", lang)),
              metric_cell(tr("hr", lang),
                          if (!is.null(peaks@hr_peak)) round(peaks@hr_peak) else "--",
                          tr("unit_bpm", lang)),
              metric_cell(tr("power", lang),
                          if (!is.null(peaks@power_peak)) round(peaks@power_peak) else "--",
                          tr("unit_watts", lang)),
              metric_cell(tr("rer", lang), sprintf("%.2f", peaks@rer_peak), "")
            )
          }
        ),

        # The two figures the report leads with.
        shiny::div(
          class = "preview-plot-row",
          shiny::div(
            class = "preview-plot",
            shiny::div(class = "preview-plot-title", tr("plot_gas", lang)),
            shiny::plotOutput(ns("preview_plot_gas"), height = "150px")
          ),
          shiny::div(
            class = "preview-plot",
            shiny::div(class = "preview-plot-title", tr("plot_vent_eq", lang)),
            shiny::plotOutput(ns("preview_plot_vent"), height = "150px")
          )
        ),

        # Thresholds if available
        if (!is.null(a@thresholds) && !is.null(a@thresholds@vt1_vo2) && !is.na(a@thresholds@vt1_vo2)) {
          shiny::div(
            class = "mt-3 pt-3 border-top",
            shiny::tags$strong(tr("threshold_results", lang)),
            shiny::p(
              class = "small mt-2 mb-0",
              sprintf("SV1 : %.0f mL/min", a@thresholds@vt1_vo2),
              if (!is.null(a@thresholds@vt2_vo2) && !is.na(a@thresholds@vt2_vo2)) {
                sprintf(" \u00b7 SV2 : %.0f mL/min", a@thresholds@vt2_vo2)
              }
            )
          )
        },

        # Clinical notes preview
        if (nzchar(input$clinical_notes %||% "")) {
          shiny::div(
            class = "mt-3 pt-3 border-top",
            shiny::tags$strong(tr("clinical_notes", lang)),
            shiny::p(class = "small mt-2 mb-0 fst-italic", input$clinical_notes)
          )
        },

        # Footer: how the numbers were produced, and who signs.
        shiny::div(
          class = "preview-footer",
          shiny::div(
            class = "preview-footer-note",
            sprintf(
              "cardiometR %s \u00b7 %s",
              utils::packageVersion("cardiometR"),
              sprintf(tr("preview_averaging_note", lang),
                      settings()$averaging_window %||% 30)
            ),
            shiny::tags$br(),
            sprintf(tr("preview_threshold_note", lang),
                    format(input$signature_date %||% Sys.Date(), "%Y-%m-%d"))
          ),
          shiny::div(
            class = "preview-signature",
            shiny::div(class = "preview-signature-line"),
            shiny::div(
              class = "preview-signature-name",
              if (nzchar(input$technician %||% "")) input$technician else tr("technician", lang)
            )
          )
        )
      )  # end report-preview-paper div
    })

    # The preview figures. They use the same functions the PDF uses, so
    # what the operator sees is what gets printed.
    output$preview_plot_gas <- shiny::renderPlot({
      a <- analysis()
      shiny::req(a)
      plot_gas_exchange(a, language = language())
    }, res = 96)

    output$preview_plot_vent <- shiny::renderPlot({
      a <- analysis()
      shiny::req(a)
      plot_ventilatory_equivalents(a, language = language())
    }, res = 96)

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

#' Settings Module UI
#'
#' @param id Module namespace ID.
#' @param language Language code ("en" or "fr").
#'
#' @return A Shiny UI element.
#'
#' @keywords internal
mod_settings_ui <- function(id, language = "en") {
  ns <- shiny::NS(id)

  bslib::card(
    bslib::card_header(
      shiny::icon("sliders"),
      tr("nav_configure", language)
    ),
    bslib::card_body(
      bslib::accordion(
        id = ns("settings_accordion"),
        open = "averaging",

        # Averaging settings
        bslib::accordion_panel(
          title = tr("averaging_section", language),
          value = "averaging",
          icon = shiny::icon("chart-area"),

          shiny::selectInput(
            ns("avg_method"),
            label = tr("averaging_method", language),
            choices = stats::setNames(
              c("time", "breath", "rolling"),
              c(tr("method_time", language),
                tr("method_breath", language),
                tr("method_rolling", language))
            ),
            selected = "rolling"
          ),
          shiny::sliderInput(
            ns("avg_window"),
            label = tr("averaging_window", language),
            min = 5,
            max = 60,
            value = 30,
            step = 5,
            post = "s"
          )
        ),

        # Threshold detection settings
        bslib::accordion_panel(
          title = tr("threshold_section", language),
          value = "thresholds",
          icon = shiny::icon("crosshairs"),

          shiny::checkboxGroupInput(
            ns("threshold_methods"),
            label = tr("threshold_methods", language),
            choices = c(
              "V-slope" = "v_slope",
              "VE/VO2" = "ve_vo2",
              "VE/VCO2" = "ve_vco2"
            ),
            selected = c("v_slope", "ve_vo2")
          )
        ),

        # Protocol settings
        bslib::accordion_panel(
          title = tr("protocol_section", language),
          value = "protocol",
          icon = shiny::icon("person-running"),

          shiny::radioButtons(
            ns("protocol_type"),
            label = tr("protocol", language),
            choices = stats::setNames(
              c("step", "ramp", "auto"),
              c(tr("protocol_step", language),
                tr("protocol_ramp", language),
                "Auto-detect")
            ),
            selected = "auto"
          ),
          shiny::selectInput(
            ns("modality"),
            label = tr("modality_label", language),
            choices = stats::setNames(
              c("cycling", "treadmill", "other"),
              c(tr("modality_cycling", language),
                tr("modality_treadmill", language),
                tr("modality_other", language))
            ),
            selected = "cycling"
          ),
          shiny::numericInput(
            ns("starting_intensity"),
            label = tr("starting_intensity", language),
            value = NA,
            min = 0,
            max = 500,
            step = 5
          ),
          shiny::numericInput(
            ns("increment_size"),
            label = tr("intensity_increment", language),
            value = NA,
            min = 0,
            max = 100,
            step = 5
          ),
          shiny::numericInput(
            ns("stage_duration"),
            label = tr("stage_duration", language),
            value = 180,
            min = 30,
            max = 600,
            step = 30
          )
        ),

        # Athlete comparison settings
        bslib::accordion_panel(
          title = tr("athlete_comparison", language),
          value = "athlete",
          icon = shiny::icon("medal"),

          shiny::selectInput(
            ns("athlete_sport"),
            label = tr("athlete_sport", language),
            choices = stats::setNames(
              c("", "cycling", "running", "triathlon", "general"),
              c("--", tr("cycling", language), tr("running", language),
                tr("triathlon", language), tr("general", language))
            ),
            selected = ""
          ),
          shiny::conditionalPanel(
            condition = "input.athlete_sport != ''",
            ns = ns,
            shiny::selectInput(
              ns("athlete_level"),
              label = tr("athlete_level", language),
              choices = stats::setNames(
                c("elite", "competitive", "recreational", "sedentary"),
                c(tr("elite", language), tr("competitive", language),
                  tr("recreational", language), tr("sedentary", language))
              ),
              selected = "recreational"
            )
          )
        ),

        # Report sections toggle
        bslib::accordion_panel(
          title = tr("report_sections_label", language),
          value = "report_sections",
          icon = shiny::icon("list-check"),

          shiny::checkboxGroupInput(
            ns("report_sections"),
            label = NULL,
            choices = stats::setNames(
              c("pretest", "protocol_details", "stage_table", "economy", "thresholds", "graphs", "clinical_notes"),
              c(tr("section_pretest_toggle", language),
                tr("section_protocol_toggle", language),
                tr("section_stage_table_toggle", language),
                tr("section_economy_toggle", language),
                tr("section_thresholds_toggle", language),
                tr("section_graphs_toggle", language),
                tr("section_clinical_notes_toggle", language))
            ),
            selected = c("protocol_details", "stage_table")
          )
        )
      )
    )
  )
}

#' Settings Module Server
#'
#' @param id Module namespace ID.
#' @param language Reactive language value.
#'
#' @return A list with reactive values:
#'   - `settings`: Reactive list of analysis parameters.
#'
#' @keywords internal
mod_settings_server <- function(id, language, cpet_data = shiny::reactive(NULL)) {
  shiny::moduleServer(id, function(input, output, session) {

    # Auto-detect protocol when data changes
    shiny::observeEvent(cpet_data(), {
      data <- cpet_data()
      if (is.null(data)) return()

      config <- tryCatch(
        detect_protocol_config(data),
        error = function(e) NULL
      )
      if (is.null(config)) return()

      lang <- language()

      shiny::updateSelectInput(session, "modality",
        selected = config@modality
      )

      # Update labels based on detected modality
      intensity_label <- modality_label(config@modality, "start", lang)
      increment_label <- modality_label(config@modality, "increment", lang)
      shiny::updateNumericInput(session, "starting_intensity", label = intensity_label)
      shiny::updateNumericInput(session, "increment_size", label = increment_label)

      if (!is.null(config@starting_intensity)) {
        shiny::updateNumericInput(session, "starting_intensity",
          value = config@starting_intensity
        )
      }

      if (!is.null(config@increment_size)) {
        shiny::updateNumericInput(session, "increment_size",
          value = config@increment_size
        )
      }

      if (!is.null(config@stage_duration_s)) {
        shiny::updateNumericInput(session, "stage_duration",
          value = config@stage_duration_s
        )
      }

      shiny::showNotification(
        tr("protocol_detected", lang),
        type = "message",
        duration = 3
      )
    })

    # Update intensity/increment labels when modality changes
    shiny::observeEvent(input$modality, {
      lang <- language()
      mod <- input$modality %||% "cycling"
      shiny::updateNumericInput(session, "starting_intensity",
        label = modality_label(mod, "start", lang)
      )
      shiny::updateNumericInput(session, "increment_size",
        label = modality_label(mod, "increment", lang)
      )
    })

    # Reactive settings list
    settings <- shiny::reactive({
      list(
        averaging_method = input$avg_method %||% "rolling",
        averaging_window = input$avg_window %||% 30,
        threshold_methods = input$threshold_methods %||% c("v_slope"),
        protocol = input$protocol_type %||% "auto",
        stage_duration = input$stage_duration %||% 180,
        modality = input$modality %||% "cycling",
        starting_intensity = input$starting_intensity,
        increment_size = input$increment_size,
        athlete_sport = if (is.null(input$athlete_sport) || input$athlete_sport == "") {
          NULL
        } else {
          input$athlete_sport
        },
        athlete_level = input$athlete_level %||% "recreational",
        report_sections = input$report_sections %||% c("protocol_details", "stage_table")
      )
    })

    # Track non-default settings and show badge indicators
    shiny::observe({
      s <- settings()
      defaults <- list(
        averaging_method = "rolling", averaging_window = 30,
        protocol = "auto", athlete_sport = NULL
      )
      changed <- c(
        averaging = !identical(s$averaging_method, defaults$averaging_method) ||
          !identical(s$averaging_window, defaults$averaging_window),
        protocol = !identical(s$protocol, defaults$protocol),
        athlete = !is.null(s$athlete_sport)
      )
      session$sendCustomMessage("update_settings_badges", as.list(changed))
    })

    # Update input labels on language change
    shiny::observeEvent(language(), {
      lang <- language()

      # Averaging method dropdown
      shiny::updateSelectInput(session, "avg_method",
        label = tr("averaging_method", lang),
        choices = stats::setNames(
          c("time", "breath", "rolling"),
          c(tr("method_time", lang), tr("method_breath", lang),
            tr("method_rolling", lang))
        )
      )

      # Protocol radio buttons
      shiny::updateRadioButtons(session, "protocol_type",
        label = tr("protocol", lang),
        choices = stats::setNames(
          c("step", "ramp", "auto"),
          c(tr("protocol_step", lang), tr("protocol_ramp", lang),
            "Auto-detect")
        )
      )

      # Modality dropdown
      shiny::updateSelectInput(session, "modality",
        label = tr("modality_label", lang),
        choices = stats::setNames(
          c("cycling", "treadmill", "other"),
          c(tr("modality_cycling", lang), tr("modality_treadmill", lang),
            tr("modality_other", lang))
        )
      )

      # Numeric input labels (modality-aware)
      mod <- input$modality %||% "cycling"
      shiny::updateNumericInput(session, "starting_intensity",
        label = modality_label(mod, "start", lang)
      )
      shiny::updateNumericInput(session, "increment_size",
        label = modality_label(mod, "increment", lang)
      )
      shiny::updateNumericInput(session, "stage_duration",
        label = tr("stage_duration", lang)
      )

      # Athlete sport dropdown
      shiny::updateSelectInput(session, "athlete_sport",
        label = tr("athlete_sport", lang),
        choices = stats::setNames(
          c("", "cycling", "running", "triathlon", "general"),
          c("--", tr("cycling", lang), tr("running", lang),
            tr("triathlon", lang), tr("general", lang))
        )
      )

      # Athlete level dropdown
      shiny::updateSelectInput(session, "athlete_level",
        label = tr("athlete_level", lang),
        choices = stats::setNames(
          c("elite", "competitive", "recreational", "sedentary"),
          c(tr("elite", lang), tr("competitive", lang),
            tr("recreational", lang), tr("sedentary", lang))
        )
      )

      # Report sections checkboxes
      shiny::updateCheckboxGroupInput(session, "report_sections",
        choices = stats::setNames(
          c("pretest", "protocol_details", "stage_table", "economy", "thresholds", "graphs", "clinical_notes"),
          c(tr("section_pretest_toggle", lang),
            tr("section_protocol_toggle", lang),
            tr("section_stage_table_toggle", lang),
            tr("section_economy_toggle", lang),
            tr("section_thresholds_toggle", lang),
            tr("section_graphs_toggle", lang),
            tr("section_clinical_notes_toggle", lang))
        )
      )
    })

    # Return settings
    list(
      settings = settings
    )
  })
}

# Modality-aware label for starting intensity / increment
modality_label <- function(modality, type = c("start", "increment"), language) {
  type <- match.arg(type)
  if (type == "start") {
    switch(modality,
      treadmill = tr("starting_speed", language),
      cycling = tr("starting_power", language),
      tr("starting_intensity", language)
    )
  } else {
    switch(modality,
      treadmill = tr("speed_increment", language),
      cycling = tr("power_increment", language),
      tr("intensity_increment", language)
    )
  }
}

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

  # An accordion header carries its own title plus a one-line summary of
  # what is set inside it, so the operator can read the whole
  # configuration without opening five panels.
  panel_title <- function(ns, title_id, label) {
    shiny::tagList(
      shiny::span(id = ns(title_id), label),
      shiny::span(
        class = "accordion-hint",
        shiny::textOutput(ns(paste0(title_id, "_summary")), inline = TRUE)
      )
    )
  }

  bslib::card(
    bslib::card_header(
      class = "d-flex align-items-center",
      shiny::span(
        shiny::icon("sliders"),
        shiny::span(id = ns("settings_header"), tr("settings_title", language))
      ),
      shiny::span(
        id = ns("settings_scope"),
        class = "ms-auto small text-muted fw-normal",
        tr("settings_scope", language)
      )
    ),
    bslib::card_body(
      bslib::accordion(
        id = ns("settings_accordion"),
        open = "averaging",

        # Averaging settings
        bslib::accordion_panel(
          title = panel_title(ns, "avg_title", tr("averaging_section", language)),
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

        # Protocol settings
        bslib::accordion_panel(
          title = panel_title(ns, "protocol_title", tr("protocol_section", language)),
          value = "protocol",
          icon = shiny::icon("person-running"),

          shiny::radioButtons(
            ns("protocol_type"),
            label = tr("protocol", language),
            choices = stats::setNames(
              c("auto", "step", "ramp"),
              c(tr("protocol_auto", language),
                tr("protocol_step", language),
                tr("protocol_ramp", language))
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
          ),
          shiny::conditionalPanel(
            condition = sprintf("$('#%s').val() == 'cycling'", ns("modality")),
            ns = ns,
            shiny::numericInput(
              ns("gross_efficiency"),
              label = tr("gross_efficiency_setting", language),
              value = default_gross_efficiency_pct,
              min = 10,
              max = 35,
              step = 1
            )
          )
        ),

        # Threshold detection settings
        bslib::accordion_panel(
          title = panel_title(ns, "threshold_title", tr("threshold_section", language)),
          value = "thresholds",
          icon = shiny::icon("crosshairs"),

          shiny::checkboxGroupInput(
            ns("threshold_methods"),
            label = tr("threshold_methods", language),
            choices = c(
              "V-slope" = "v_slope",
              "VE/VO2" = "ve_vo2",
              "PetO2 / PetCO2" = "end_tidal",
              "VE/VCO2" = "ve_vco2"
            ),
            selected = c("v_slope", "ve_vo2", "end_tidal")
          )
        ),

        # Athlete comparison settings
        bslib::accordion_panel(
          title = panel_title(ns, "athlete_title", tr("athlete_comparison", language)),
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
                c("recreational", "competitive", "elite", "sedentary"),
                c(tr("recreational", language), tr("competitive", language),
                  tr("elite", language), tr("sedentary", language))
              ),
              selected = "recreational"
            )
          )
        ),

        # Report sections toggle
        bslib::accordion_panel(
          title = panel_title(ns, "report_sections_title", tr("report_sections_label", language)),
          value = "report_sections",
          icon = shiny::icon("list-check"),

          shiny::checkboxGroupInput(
            ns("report_sections"),
            label = NULL,
            choices = stats::setNames(
              c("pretest", "protocol_details", "stage_table", "economy", "thresholds", "graphs", "clinical_notes",
                "athlete_profile", "population_norms", "longitudinal", "estimates_caveats"),
              c(tr("section_pretest_toggle", language),
                tr("section_protocol_toggle", language),
                tr("section_stage_table_toggle", language),
                tr("section_economy_toggle", language),
                tr("section_thresholds_toggle", language),
                tr("section_graphs_toggle", language),
                tr("section_clinical_notes_toggle", language),
                tr("report_section_athlete_profile", language),
                tr("section_population_norms", language),
                tr("report_section_longitudinal", language),
                tr("report_section_estimates_caveats", language))
            ),
            selected = c("protocol_details", "stage_table", "athlete_profile",
                         "population_norms", "estimates_caveats")
          )
        )
      ),
      shiny::div(
        class = "settings-footer",
        shiny::span(
          id = ns("settings_footer_hint"),
          class = "small text-muted flex-grow-1",
          tr("settings_changed_hint", language)
        ),
        shiny::actionButton(
          ns("reset_settings"),
          label = tr("reset", language),
          class = "btn-outline-secondary btn-sm"
        ),
        shiny::actionButton(
          ns("go_results"),
          label = tr("analyze", language),
          icon = shiny::icon("arrow-right"),
          class = "btn-primary btn-sm"
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
        report_sections = input$report_sections %||% c("protocol_details", "stage_table",
                                                        "athlete_profile", "population_norms",
                                                        "estimates_caveats"),
        gross_efficiency = input$gross_efficiency %||% default_gross_efficiency_pct
      )
    })

    # One-line summaries on the accordion headers.
    output$avg_title_summary <- shiny::renderText({
      s <- settings()
      lang <- language()
      method_label <- switch(s$averaging_method,
        time = tr("method_time", lang),
        breath = tr("method_breath", lang),
        tr("method_rolling", lang)
      )
      paste(method_label, paste0(s$averaging_window, " s"), sep = " · ")
    })

    output$protocol_title_summary <- shiny::renderText({
      s <- settings()
      lang <- language()
      protocol_label <- switch(s$protocol,
        step = tr("protocol_step", lang),
        ramp = tr("protocol_ramp", lang),
        tr("protocol_auto", lang)
      )
      modality_label <- switch(s$modality,
        treadmill = tr("modality_treadmill", lang),
        other = tr("modality_other", lang),
        tr("modality_cycling", lang)
      )
      paste(protocol_label, modality_label, sep = " · ")
    })

    output$threshold_title_summary <- shiny::renderText({
      n <- length(settings()$threshold_methods)
      sprintf(tr("threshold_method_count", language()), n)
    })

    output$athlete_title_summary <- shiny::renderText({
      s <- settings()
      lang <- language()
      if (is.null(s$athlete_sport)) return(tr("athlete_none", lang))
      sport_label <- switch(s$athlete_sport,
        running = tr("running", lang),
        triathlon = tr("triathlon", lang),
        general = tr("general", lang),
        tr("cycling", lang)
      )
      level_label <- switch(s$athlete_level,
        competitive = tr("competitive", lang),
        elite = tr("elite", lang),
        sedentary = tr("sedentary", lang),
        tr("recreational", lang)
      )
      paste(sport_label, level_label, sep = " · ")
    })

    output$report_sections_title_summary <- shiny::renderText({
      sprintf(
        tr("report_section_count", language()),
        length(settings()$report_sections),
        11L
      )
    })

    # Put the laboratory defaults back.
    shiny::observeEvent(input$reset_settings, {
      shiny::updateSelectInput(session, "avg_method", selected = "rolling")
      shiny::updateSliderInput(session, "avg_window", value = 30)
      shiny::updateCheckboxGroupInput(session, "threshold_methods",
                                      selected = c("v_slope", "ve_vo2"))
      shiny::updateRadioButtons(session, "protocol_type", selected = "auto")
      shiny::updateSelectInput(session, "athlete_sport", selected = "")
      shiny::updateSelectInput(session, "athlete_level", selected = "recreational")
      shiny::updateNumericInput(session, "gross_efficiency",
                                value = default_gross_efficiency_pct)
      shiny::updateCheckboxGroupInput(
        session, "report_sections",
        selected = c("protocol_details", "stage_table", "athlete_profile",
                     "population_norms", "estimates_caveats")
      )
    })

    # Track non-default settings and show badge indicators
    shiny::observeEvent(settings(), {
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
      ns <- session$ns

      # Update card header and accordion panel titles via JS
      session$sendCustomMessage("update_text", as.list(stats::setNames(
        c(tr("settings_title", lang), tr("averaging_section", lang), tr("threshold_section", lang),
          tr("protocol_section", lang), tr("athlete_comparison", lang), tr("report_sections_label", lang),
          tr("settings_scope", lang), tr("settings_changed_hint", lang)),
        c(ns("settings_header"), ns("avg_title"), ns("threshold_title"),
          ns("protocol_title"), ns("athlete_title"), ns("report_sections_title"),
          ns("settings_scope"), ns("settings_footer_hint"))
      )))
      shiny::updateActionButton(session, "reset_settings", label = tr("reset", lang))
      shiny::updateActionButton(session, "go_results", label = tr("analyze", lang))

      # Averaging method dropdown
      # Passing `choices` without `selected` makes Shiny fall back to the
      # first choice, so re-labelling a dropdown for a new language would
      # silently discard what the user had picked.
      shiny::updateSelectInput(session, "avg_method",
        label = tr("averaging_method", lang),
        choices = stats::setNames(
          c("time", "breath", "rolling"),
          c(tr("method_time", lang), tr("method_breath", lang),
            tr("method_rolling", lang))
        ),
        selected = input$avg_method
      )

      # Averaging window slider
      shiny::updateSliderInput(session, "avg_window",
        label = tr("averaging_window", lang)
      )

      # Threshold methods checkbox group
      shiny::updateCheckboxGroupInput(session, "threshold_methods",
        label = tr("threshold_methods", lang)
      )

      # Protocol radio buttons
      shiny::updateRadioButtons(session, "protocol_type",
        label = tr("protocol", lang),
        choices = stats::setNames(
          c("auto", "step", "ramp"),
          c(tr("protocol_auto", lang),
            tr("protocol_step", lang),
            tr("protocol_ramp", lang))
        ),
        selected = input$protocol_type %||% "auto"
      )

      # Modality dropdown
      shiny::updateSelectInput(session, "modality",
        label = tr("modality_label", lang),
        choices = stats::setNames(
          c("cycling", "treadmill", "other"),
          c(tr("modality_cycling", lang), tr("modality_treadmill", lang),
            tr("modality_other", lang))
        ),
        selected = input$modality
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

      # Gross efficiency label
      shiny::updateNumericInput(session, "gross_efficiency",
        label = tr("gross_efficiency_setting", lang)
      )

      # Athlete sport dropdown
      shiny::updateSelectInput(session, "athlete_sport",
        label = tr("athlete_sport", lang),
        choices = stats::setNames(
          c("", "cycling", "running", "triathlon", "general"),
          c("--", tr("cycling", lang), tr("running", lang),
            tr("triathlon", lang), tr("general", lang))
        ),
        selected = input$athlete_sport
      )

      # Athlete level dropdown. The order has to match the one declared in the
      # UI: it used to start with "elite" here, so the reset above landed on
      # elite and every participant was silently compared against elite norms.
      shiny::updateSelectInput(session, "athlete_level",
        label = tr("athlete_level", lang),
        choices = stats::setNames(
          c("recreational", "competitive", "elite", "sedentary"),
          c(tr("recreational", lang), tr("competitive", lang),
            tr("elite", lang), tr("sedentary", lang))
        ),
        selected = input$athlete_level
      )

      # Report sections checkboxes
      shiny::updateCheckboxGroupInput(session, "report_sections",
        choices = stats::setNames(
          c("pretest", "protocol_details", "stage_table", "economy", "thresholds", "graphs", "clinical_notes",
            "athlete_profile", "population_norms", "longitudinal", "estimates_caveats"),
          c(tr("section_pretest_toggle", lang),
            tr("section_protocol_toggle", lang),
            tr("section_stage_table_toggle", lang),
            tr("section_economy_toggle", lang),
            tr("section_thresholds_toggle", lang),
            tr("section_graphs_toggle", lang),
            tr("section_clinical_notes_toggle", lang),
            tr("report_section_athlete_profile", lang),
            tr("section_population_norms", lang),
            tr("report_section_longitudinal", lang),
            tr("report_section_estimates_caveats", lang))
        ),
        selected = input$report_sections
      )
    })

    # Return settings
    list(
      settings = settings,
      go_results = shiny::reactive(input$go_results)
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

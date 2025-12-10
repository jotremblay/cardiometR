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
          shiny::conditionalPanel(
            condition = "input.protocol_type == 'step'",
            ns = ns,
            shiny::numericInput(
              ns("stage_duration"),
              label = tr("stage_duration", language),
              value = 180,
              min = 30,
              max = 600,
              step = 30
            )
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
mod_settings_server <- function(id, language) {
  shiny::moduleServer(id, function(input, output, session) {

    # Reactive settings list
    settings <- shiny::reactive({
      list(
        averaging_method = input$avg_method %||% "rolling",
        averaging_window = input$avg_window %||% 30,
        threshold_methods = input$threshold_methods %||% c("v_slope"),
        protocol = input$protocol_type %||% "auto",
        stage_duration = input$stage_duration %||% 180,
        athlete_sport = if (is.null(input$athlete_sport) || input$athlete_sport == "") {
          NULL
        } else {
          input$athlete_sport
        },
        athlete_level = input$athlete_level %||% "recreational"
      )
    })

    # Return settings
    list(
      settings = settings
    )
  })
}

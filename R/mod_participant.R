#' Participant Module UI
#'
#' @param id Module namespace ID.
#' @param language Language code ("en" or "fr").
#'
#' @return A Shiny UI element.
#'
#' @keywords internal
mod_participant_ui <- function(id, language = "en") {
  ns <- shiny::NS(id)

  bslib::card(
    bslib::card_header(
      shiny::span(
        shiny::icon("user"),
        shiny::span(id = ns("participant_header"), tr("section_patient", language))
      )
    ),
    bslib::card_body(
      # The header fields are always editable: a correction here changes the
      # relative values and the normative z-scores, so it must be one click
      # away, not behind an edit mode.
      bslib::layout_columns(
        col_widths = c(6, 6),
        shiny::textInput(ns("participant_id"), tr("participant_id", language)),
        shiny::textInput(ns("name"), tr("participant_name", language)),
        shiny::numericInput(ns("age"), tr("participant_age", language),
                            value = 30, min = 1, max = 120),
        shiny::selectInput(ns("sex"), tr("participant_sex", language),
                           choices = stats::setNames(
                             c("M", "F", "O"),
                             c(tr("male", language), tr("female", language), tr("other", language))
                           )),
        shiny::numericInput(ns("height_cm"), tr("participant_height", language),
                            value = 170, min = 50, max = 250),
        shiny::numericInput(ns("weight_kg"), tr("participant_weight", language),
                            value = 70, min = 10, max = 300),
        shiny::textInput(ns("sport"), tr("participant_sport", language)),
        shiny::uiOutput(ns("bmi_display"))
      ),
      shiny::div(
        id = ns("participant_note"),
        class = "participant-note",
        tr("participant_prefill_note", language)
      ),
      # Prediction source selector
      shiny::tags$hr(class = "my-2"),
      shiny::selectInput(ns("prediction_source"),
                         label = shiny::span(
                           id = ns("prediction_source_label"),
                           tr("prediction_source", language)
                         ),
                         choices = c("Jones et al. (1997)" = "jones",
                                     "Pr\u00e9faut et al." = "prefaut"),
                         width = "100%"),
      shiny::tags$small(
        class = "text-muted d-block mt-1",
        id = ns("prediction_source_help"),
        tr("prediction_source_help", language)
      )
    )
  )
}

#' Participant Module Server
#'
#' @param id Module namespace ID.
#' @param language Reactive language value.
#' @param cpet_data Reactive CpetData object from upload module.
#'
#' @return A list with reactive values:
#'   - `participant`: Reactive Participant object with user edits.
#'   - `prediction_source`: Reactive prediction equation source ("jones" or "prefaut").
#'
#' @keywords internal
mod_participant_server <- function(id, language, cpet_data) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns


    # Update labels when language changes
    shiny::observeEvent(language(), {
      lang <- language()

      # Update card header via JS
      session$sendCustomMessage("update_text", as.list(stats::setNames(
        tr("section_patient", lang),
        ns("participant_header")
      )))

      # Update form input labels
      shiny::updateTextInput(session, "participant_id", label = tr("participant_id", lang))
      shiny::updateTextInput(session, "name", label = tr("participant_name", lang))
      shiny::updateNumericInput(session, "age", label = tr("participant_age", lang))
      shiny::updateSelectInput(session, "sex",
        label = tr("participant_sex", lang),
        choices = stats::setNames(
          c("M", "F", "O"),
          c(tr("male", lang), tr("female", lang), tr("other", lang))
        ),
        selected = input$sex
      )
      shiny::updateNumericInput(session, "height_cm", label = tr("participant_height", lang))
      shiny::updateNumericInput(session, "weight_kg", label = tr("participant_weight", lang))
      shiny::updateTextInput(session, "sport", label = tr("participant_sport", lang))

      # Update prediction source label + helper via JS
      session$sendCustomMessage("update_text", as.list(stats::setNames(
        c(tr("prediction_source", lang), tr("prediction_source_help", lang),
          tr("participant_prefill_note", lang)),
        c(ns("prediction_source_label"), ns("prediction_source_help"),
          ns("participant_note"))
      )))
    })

    # Pre-fill the form from the COSMED header when a file loads.
    shiny::observeEvent(cpet_data(), {
      data <- cpet_data()
      if (is.null(data)) return()
      p <- data@participant
      shiny::updateTextInput(session, "participant_id", value = p@id)
      shiny::updateTextInput(session, "name", value = p@name)
      shiny::updateNumericInput(session, "age", value = p@age)
      shiny::updateSelectInput(session, "sex", selected = p@sex)
      shiny::updateNumericInput(session, "height_cm", value = p@height_cm)
      shiny::updateNumericInput(session, "weight_kg", value = p@weight_kg)
      shiny::updateTextInput(session, "sport", value = p@sport %||% "")
    })

    # BMI is derived, so it sits in the grid as a read-only cell.
    output$bmi_display <- shiny::renderUI({
      h <- input$height_cm
      w <- input$weight_kg
      value <- if (is.null(h) || is.null(w) || !is.finite(h) || !is.finite(w) || h <= 0) {
        "--"
      } else {
        format(round(w / (h / 100)^2, 1), nsmall = 1)
      }
      shiny::div(
        class = "derived-field",
        shiny::div(class = "derived-label", "IMC"),
        shiny::div(class = "derived-value", value, shiny::tags$small(" kg/m\u00B2"))
      )
    })

    # The participant object follows the form. A correction reaches the
    # relative values and the z-scores without a save step.
    participant <- shiny::reactive({
      data <- cpet_data()
      if (is.null(data)) return(NULL)
      base <- data@participant
      tryCatch(
        Participant(
          id = if (isTRUE(nzchar(input$participant_id))) input$participant_id else base@id,
          name = if (isTRUE(nzchar(input$name))) input$name else base@name,
          age = as.integer(input$age %||% base@age),
          sex = input$sex %||% base@sex,
          height_cm = input$height_cm %||% base@height_cm,
          weight_kg = input$weight_kg %||% base@weight_kg,
          sport = if (isTRUE(nzchar(input$sport))) input$sport else NULL
        ),
        error = function(e) base
      )
    })

    # Return the participant and prediction_source
    list(
      participant = participant,
      prediction_source = shiny::reactive({
        input$prediction_source %||% "jones"
      })
    )
  })
}

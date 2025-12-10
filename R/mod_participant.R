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
      class = "d-flex justify-content-between align-items-center",
      shiny::span(
        shiny::icon("user"),
        tr("section_patient", language)
      ),
      shiny::actionButton(
        ns("edit_toggle"),
        label = NULL,
        icon = shiny::icon("edit"),
        class = "btn-sm btn-outline-secondary"
      )
    ),
    bslib::card_body(
      # Display mode
      shiny::conditionalPanel(
        condition = "!input.edit_mode",
        ns = ns,
        shiny::uiOutput(ns("participant_display"))
      ),
      # Edit mode
      shiny::conditionalPanel(
        condition = "input.edit_mode",
        ns = ns,
        shiny::textInput(ns("name"), tr("participant_name", language)),
        bslib::layout_columns(
          col_widths = c(6, 6),
          shiny::numericInput(ns("age"), tr("participant_age", language),
                              value = 30, min = 1, max = 120),
          shiny::selectInput(ns("sex"), tr("participant_sex", language),
                             choices = stats::setNames(
                               c("M", "F", "O"),
                               c(tr("male", language), tr("female", language), tr("other", language))
                             ))
        ),
        bslib::layout_columns(
          col_widths = c(6, 6),
          shiny::numericInput(ns("height_cm"), tr("participant_height", language),
                              value = 170, min = 50, max = 250),
          shiny::numericInput(ns("weight_kg"), tr("participant_weight", language),
                              value = 70, min = 10, max = 300)
        ),
        shiny::textInput(ns("sport"), tr("participant_sport", language)),
        shiny::actionButton(ns("save_edits"), tr("submit", language),
                            class = "btn-primary mt-2")
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
#'
#' @keywords internal
mod_participant_server <- function(id, language, cpet_data) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Track edit mode
    edit_mode <- shiny::reactiveVal(FALSE)

    # Store edited participant
    edited_participant <- shiny::reactiveVal(NULL)

    # Toggle edit mode
    shiny::observeEvent(input$edit_toggle, {
      edit_mode(!edit_mode())

      # Update conditional panel
      shiny::updateCheckboxInput(session, "edit_mode", value = edit_mode())
    })

    # Hidden input to control conditional panels
    output$edit_mode_js <- shiny::renderUI({
      shiny::checkboxInput(ns("edit_mode"), label = NULL, value = edit_mode())
    })

    # Populate form when data loads
    shiny::observeEvent(cpet_data(), {
      data <- cpet_data()
      if (!is.null(data)) {
        p <- data@participant
        shiny::updateTextInput(session, "name", value = p@name)
        shiny::updateNumericInput(session, "age", value = p@age)
        shiny::updateSelectInput(session, "sex", selected = p@sex)
        shiny::updateNumericInput(session, "height_cm", value = p@height_cm)
        shiny::updateNumericInput(session, "weight_kg", value = p@weight_kg)
        shiny::updateTextInput(session, "sport", value = p@sport %||% "")
      }
    })

    # Save edits
    shiny::observeEvent(input$save_edits, {
      tryCatch({
        new_participant <- Participant(
          id = cpet_data()@participant@id,
          name = input$name,
          age = as.integer(input$age),
          sex = input$sex,
          height_cm = input$height_cm,
          weight_kg = input$weight_kg,
          sport = if (nchar(input$sport) > 0) input$sport else NULL
        )
        edited_participant(new_participant)
        edit_mode(FALSE)
        shiny::updateCheckboxInput(session, "edit_mode", value = FALSE)

        shiny::showNotification(
          tr("changes_saved", language()),
          type = "message",
          duration = 2
        )
      }, error = function(e) {
        shiny::showNotification(
          paste("Error:", e$message),
          type = "error",
          duration = 5
        )
      })
    })

    # Render participant display
    output$participant_display <- shiny::renderUI({
      data <- cpet_data()
      lang <- language()

      if (is.null(data)) {
        return(
          shiny::div(
            class = "text-muted text-center py-4",
            shiny::icon("user-slash", class = "fa-2x mb-2"),
            shiny::p(tr("upload_prompt", lang))
          )
        )
      }

      # Use edited participant if available
      p <- edited_participant() %||% data@participant

      # Calculate BMI
      bmi <- round(p@weight_kg / (p@height_cm / 100)^2, 1)

      shiny::tagList(
        # Name prominently displayed
        shiny::h5(class = "mb-3", p@name),

        # Key metrics as small value boxes
        bslib::layout_columns(
          col_widths = c(6, 6),
          fill = FALSE,

          bslib::value_box(
            title = tr("participant_age", lang),
            value = paste(p@age, tr("unit_years", lang)),
            showcase = shiny::icon("calendar"),
            theme = "light",
            height = "100px"
          ),
          bslib::value_box(
            title = tr("participant_sex", lang),
            value = switch(p@sex,
                           "M" = tr("male", lang),
                           "F" = tr("female", lang),
                           "O" = tr("other", lang)),
            showcase = shiny::icon("venus-mars"),
            theme = "light",
            height = "100px"
          )
        ),

        bslib::layout_columns(
          col_widths = c(6, 6),
          fill = FALSE,

          bslib::value_box(
            title = tr("participant_height", lang),
            value = paste(p@height_cm, "cm"),
            showcase = shiny::icon("ruler-vertical"),
            theme = "light",
            height = "100px"
          ),
          bslib::value_box(
            title = tr("participant_weight", lang),
            value = paste(p@weight_kg, "kg"),
            showcase = shiny::icon("weight-scale"),
            theme = "light",
            height = "100px"
          )
        ),

        # BMI and sport
        shiny::tags$dl(
          class = "row mt-3 mb-0",
          shiny::tags$dt(class = "col-4", "BMI"),
          shiny::tags$dd(class = "col-8", paste(bmi, "kg/m\u00B2")),
          if (!is.null(p@sport) && nchar(p@sport) > 0) {
            shiny::tagList(
              shiny::tags$dt(class = "col-4", tr("participant_sport", lang)),
              shiny::tags$dd(class = "col-8", p@sport)
            )
          }
        )
      )
    })

    # Return the participant (edited or original)
    list(
      participant = shiny::reactive({
        edited_participant() %||% cpet_data()@participant
      })
    )
  })
}

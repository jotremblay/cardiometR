#' Main Application Server
#'
#' @description
#' Main server function that coordinates all modules and manages
#' the reactive language state.
#'
#' @param input Shiny input object.
#' @param output Shiny output object.
#' @param session Shiny session object.
#'
#' @keywords internal
app_server <- function(input, output, session) {

  # Reactive language state
language <- shiny::reactiveVal(getOption("cardiometR.language", "fr"))

  # Language toggle handler
  # Nav tabs updated via JS custom message handler; dropdowns updated
  # via updateSelectInput/updateRadioButtons in each module server.
  shiny::observeEvent(input$lang_switch, {
    current <- language()
    new_lang <- if (current == "en") "fr" else "en"
    language(new_lang)

    # Update the toggle button label
    shiny::updateActionButton(
      session,
      "lang_switch",
      label = if (new_lang == "en") "FR" else "EN"
    )

    # Update nav tab labels dynamically via JS
    tab_labels <- list(
      upload = tr("nav_upload", new_lang),
      configure = tr("nav_configure", new_lang),
      results = tr("nav_results", new_lang),
      quality = tr("nav_quality", new_lang),
      report = tr("nav_report", new_lang)
    )
    session$sendCustomMessage("update_nav_labels", tab_labels)

    # Show a notification about the language change
    shiny::showNotification(
      tr("language_changed", new_lang),
      type = "message",
      duration = 3
    )
  })

  # ---- Module: Upload ----
  # Returns: list(cpet_data = reactive(), validation = reactive())
  upload_result <- mod_upload_server("upload", language)

  # ---- Module: Participant ----
  # Returns: list(participant = reactive())
  participant_result <- mod_participant_server(
    "participant",
    language,
    cpet_data = upload_result$cpet_data
  )

  # ---- Module: Settings ----
  # Returns: list(settings = reactive())
  settings_result <- mod_settings_server("settings", language,
                                         cpet_data = upload_result$cpet_data)

  # ---- Module: Results ----
  # Returns: list(analysis = reactive())
  results_result <- mod_results_server(
    "results",
    language,
    cpet_data = upload_result$cpet_data,
    participant = participant_result$participant,
    settings = settings_result$settings
  )

  # ---- Module: Plots ----
  # Display only, no return value
  mod_plots_server(
    "plots",
    language,
    analysis = results_result$analysis,
    settings = settings_result$settings
  )

  # ---- Module: Quality ----
  # Returns: list(quality = reactive())
  quality_result <- mod_quality_server(
    "quality",
    language,
    cpet_data = upload_result$cpet_data,
    analysis = results_result$analysis
  )

  # ---- Module: Report ----
  # Handles PDF generation
  mod_report_server(
    "report",
    language,
    analysis = results_result$analysis,
    settings = settings_result$settings
  )

  # Back to Upload button handler
  shiny::observeEvent(input$back_to_upload, {
    bslib::nav_select("main_navbar", "upload")
  })

  # Auto-navigate to Configure tab after successful upload
  shiny::observeEvent(upload_result$cpet_data(), {
    if (!is.null(upload_result$cpet_data())) {
      bslib::nav_select("main_navbar", "configure")
    }
  })
}

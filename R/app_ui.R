#' Main Application UI
#'
#' @description
#' Builds the main Shiny UI using bslib page_navbar with four tabs:
#' Upload, Configure, Results, and Report.
#'
#' @return A Shiny UI definition.
#'
#' @keywords internal
app_ui <- function() {
  lang <- getOption("cardiometR.language", "en")

  bslib::page_navbar(
    id = "main_navbar",
    title = tr("app_title", lang),
    theme = bslib::bs_theme(
      version = 5,
      bootswatch = "flatly",
      primary = "#2E86AB",
      secondary = "#6c757d"
    ),
    fillable = TRUE,

    # Header with language toggle
    header = shiny::div(
      class = "container-fluid d-flex justify-content-end py-2",
      shiny::actionButton(
        "lang_switch",
        label = if (lang == "en") "FR" else "EN",
        class = "btn-outline-secondary btn-sm"
      )
    ),

    # Tab: Upload
    bslib::nav_panel(
      title = tr("nav_upload", lang),
      value = "upload",
      icon = shiny::icon("upload"),
      mod_upload_ui("upload", lang)
    ),

    # Tab: Configure
    bslib::nav_panel(
      title = tr("nav_configure", lang),
      value = "configure",
      icon = shiny::icon("sliders"),
      bslib::layout_columns(
        col_widths = c(5, 7),
        mod_participant_ui("participant", lang),
        mod_settings_ui("settings", lang)
      )
    ),

    # Tab: Results
    bslib::nav_panel(
      title = tr("nav_results", lang),
      value = "results",
      icon = shiny::icon("chart-line"),
      bslib::layout_columns(
        col_widths = c(4, 8),
        mod_results_ui("results", lang),
        mod_plots_ui("plots", lang)
      )
    ),

    # Tab: Quality
    bslib::nav_panel(
      title = tr("nav_quality", lang),
      value = "quality",
      icon = shiny::icon("clipboard-check"),
      mod_quality_ui("quality", lang)
    ),

    # Tab: Report
    bslib::nav_panel(
      title = tr("nav_report", lang),
      value = "report",
      icon = shiny::icon("file-pdf"),
      mod_report_ui("report", lang)
    )
  )
}

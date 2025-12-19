#' Launch CPET Analysis Shiny Application
#'
#' @description
#' Starts the interactive Shiny application for CPET data analysis.
#' The app provides bilingual support (English/French), file upload,
#' analysis configuration, visualization, and PDF report generation.
#'
#' @param language Initial language for the app ("en" or "fr"). Default is "en".
#' @param ... Additional arguments passed to [shiny::shinyApp()].
#'
#' @return A Shiny app object.
#'
#' @examples
#' if (interactive()) {
#'   run_app()
#'   run_app(language = "fr")
#' }
#'
#' @export
run_app <- function(language = c("en", "fr"), ...) {
  language <- match.arg(language)

  # Store initial language as option for app_ui to access
  options(cardiometR.language = language)

  # Add resource path for package assets (logos, etc.)
  assets_dir <- system.file("assets", package = "cardiometR")
  if (nzchar(assets_dir)) {
    shiny::addResourcePath("cardiometR", assets_dir)
  }

  shiny::shinyApp(
    ui = app_ui(),
    server = app_server,
    ...
  )
}

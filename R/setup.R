#' Check That This Machine Can Run the App
#'
#' @description
#' Reports whether everything the Shiny app needs is present: the R
#' packages, the Typst binary that renders the PDF report, and the
#' example data file. Run it after installing the package, or whenever
#' the app misbehaves on a new machine.
#'
#' @return Invisibly, `TRUE` when nothing is missing, `FALSE` otherwise.
#'
#' @examples
#' check_setup()
#'
#' @export
check_setup <- function() {
  cli::cli_h1("cardiometR setup")

  ok <- TRUE

  # R packages. Everything the app needs sits in Imports, so a normal
  # install brings them; a broken install shows up here.
  required <- c("shiny", "bslib", "DT", "plotly", "ggplot2", "patchwork",
                "gt", "S7", "readxl", "typr")
  missing <- required[!vapply(required, requireNamespace, logical(1),
                              quietly = TRUE)]
  if (length(missing) == 0) {
    cli::cli_alert_success("R packages: all present")
  } else {
    ok <- FALSE
    cli::cli_alert_danger("R packages missing: {.pkg {missing}}")
    cli::cli_alert_info(
      'Fix: {.run install.packages(c({paste0(sQuote(missing, FALSE), collapse = ", ")}))}'
    )
  }

  # Typst. The app runs without it, but the PDF button stays disabled.
  if (is_typst_available()) {
    cli::cli_alert_success("Typst: found, PDF reports will render")
  } else {
    ok <- FALSE
    cli::cli_alert_warning("Typst: not found, PDF reports are disabled")
    cli::cli_alert_info(
      "Fix: install Quarto from {.url https://quarto.org/docs/get-started/}, which bundles Typst."
    )
  }

  # Example file, so a student has something to open on day one.
  example <- system.file("extdata", "example_cosmed.xlsx",
                         package = "cardiometR")
  if (nzchar(example)) {
    cli::cli_alert_success("Example file: {.path {basename(example)}}")
  } else {
    ok <- FALSE
    cli::cli_alert_danger("Example file missing; the install is incomplete")
  }

  cli::cli_rule()
  if (ok) {
    cli::cli_alert_success("Ready. Start the app with {.run cardiometR::run_app()}")
  } else {
    cli::cli_alert_warning("Fix the items above, then run {.run cardiometR::check_setup()} again")
  }

  invisible(ok)
}

# Inspecting how a file will be interpreted, before or after importing it.


#' See how a data file's columns will be interpreted
#'
#' Reads only the top of a file, works out which format it is and where its
#' column headers are, and reports what each column would become. Nothing is
#' imported and no data is read, so this works even on a file that
#' [read_cpet()] refuses.
#'
#' Use it when a file half-imports, or when a column you expected is missing.
#' The `source` column gives the header exactly as the file spells it, which is
#' what you pass to `read_cpet(mapping = ...)`.
#'
#' @inheritParams read_cpet
#'
#' @return A data frame with one row per column found in the file:
#'   \describe{
#'     \item{source}{The header as written in the file.}
#'     \item{canonical}{The internal name it maps to, or `NA`.}
#'     \item{unit_file}{The unit the file states, if any.}
#'     \item{unit_target}{The unit cardiometR will convert it to.}
#'     \item{status}{`"mapped"`, `"ignored"` for columns this format is known
#'       to write but cardiometR does not use, or `"unrecognised"`.}
#'     \item{suggestion}{A close alias, when there is one.}
#'   }
#'   The detected format and layout are attached as attributes.
#'
#' @param quiet Suppress the summary printed to the console. Defaults to `TRUE`
#'   outside an interactive session.
#'
#' @examples
#' file <- system.file("extdata", "example_cosmed.xlsx", package = "cardiometR")
#' preview <- preview_cpet_columns(file, quiet = TRUE)
#' head(preview)
#'
#' @seealso [read_cpet()], [list_cpet_dialects()]
#'
#' @export
preview_cpet_columns <- function(file, format = NULL, sheet = NULL,
                                 quiet = !rlang::is_interactive()) {
  if (!file.exists(file)) {
    cli::cli_abort("File not found: {.file {file}}")
  }

  detected <- detect_cpet_format(file, sheet = sheet, format = format)
  dialect <- detected$dialect
  grid <- detected$raw$grid

  location <- locate_data_block(
    grid, dialect$lookup, dialect$required,
    max_scan = dialect$layout$data$max_scan_rows %||% 30L
  )

  headers <- grid[location$header_row, location$data_cols]
  resolved <- resolve_columns(headers, dialect$lookup,
                              ignore = dialect$ignore,
                              ignore_patterns = dialect$ignore_patterns)

  declared <- if (is.na(location$units_row)) {
    rep(NA_character_, length(headers))
  } else {
    parse_unit(grid[location$units_row, location$data_cols])
  }
  inline <- unname(resolved$inline_units)
  canonical <- unname(resolved$mapping)

  out <- data.frame(
    source = headers,
    canonical = canonical,
    unit_file = ifelse(is.na(declared), inline, declared),
    unit_target = vapply(canonical, function(name) {
      if (is.na(name) || !name %in% names(.canonical_units)) {
        return(NA_character_)
      }
      unname(.canonical_units[[name]])
    }, character(1), USE.NAMES = FALSE),
    status = ifelse(
      !is.na(canonical), "mapped",
      ifelse(headers %in% resolved$ignored, "ignored", "unrecognised")
    ),
    suggestion = vapply(headers, function(header) {
      hint <- resolved$suggestions[[header]]
      if (is.null(hint)) NA_character_ else paste(hint, collapse = "; ")
    }, character(1), USE.NAMES = FALSE),
    stringsAsFactors = FALSE
  )

  attr(out, "cpet_format") <- dialect$label
  attr(out, "cpet_sheet") <- detected$raw$sheet
  attr(out, "cpet_layout") <- location

  if (!quiet) {
    describe_column_preview(out)
  }
  out
}


#' Describe what a column preview found
#'
#' Written out when [preview_cpet_columns()] runs interactively. The returned
#' data frame stays an ordinary data frame, so it subsets, filters, and prints
#' the way any other one does.
#'
#' @param preview The result of [preview_cpet_columns()].
#'
#' @return Invisibly, `preview`.
#'
#' @keywords internal
describe_column_preview <- function(preview) {
  layout <- attr(preview, "cpet_layout")
  sheet <- attr(preview, "cpet_sheet")

  cli::cli_h2("Column preview")
  cli::cli_dl(c(
    "Format" = attr(preview, "cpet_format"),
    "Sheet" = if (is.null(sheet) || is.na(sheet)) "-" else as.character(sheet),
    "Layout" = "headers on row {layout$header_row}, units on row \\
                {if (is.na(layout$units_row)) 'none' else layout$units_row}, \\
                data from row {layout$data_row}"
  ))

  counts <- table(factor(preview$status,
                         levels = c("mapped", "ignored", "unrecognised")))
  cli::cli_alert_info("{counts[['mapped']]} recognised, \\
                       {counts[['ignored']]} known but unused, \\
                       {counts[['unrecognised']]} unrecognised")

  puzzling <- preview[preview$status == "unrecognised", , drop = FALSE]
  for (i in seq_len(nrow(puzzling))) {
    if (is.na(puzzling$suggestion[[i]])) {
      cli::cli_alert_warning("{.val {puzzling$source[[i]]}} was not recognised.")
    } else {
      cli::cli_alert_warning("{.val {puzzling$source[[i]]}} was not recognised. \\
                              Did you mean {.val {puzzling$suggestion[[i]]}}?")
    }
  }
  if (nrow(puzzling) > 0) {
    cli::cli_alert_info("Name them yourself with \\
                         {.code read_cpet(file, mapping = c(\"Header\" = \"vo2_ml\"))}.")
  }

  invisible(preview)
}

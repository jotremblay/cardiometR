# Reading the participant and test details out of a file's header block.
#
# The old importer read fixed cells: participant name from B2, age from B5,
# test date from E1. That works until a cart writes its block in a different
# order or a different language. Here the LABEL cells are read and matched
# against the dialect's vocabulary, so the value is found wherever it sits.
# The fixed positions remain as a fallback, declared in the dialect file, and
# anything found that way is marked as a guess in the import report.


#' Scan a header block for labelled values
#'
#' @param grid A character matrix of the whole sheet.
#' @param typed The typed read of the same sheet, used to recover real dates.
#' @param dialect A compiled dialect.
#'
#' @return A list with `values` (named list of raw values) and `provenance`
#'   (a data frame recording where each one came from).
#'
#' @keywords internal
scan_label_block <- function(grid, typed, dialect) {
  config <- dialect$layout$header_block %||% list()
  label_cols <- unlist(config$label_cols %||% c(1, 4, 7))
  max_rows <- min(config$max_rows %||% 25L, nrow(grid))
  lookup <- dialect$metadata_lookup

  values <- list()
  provenance <- list()

  for (label_col in label_cols) {
    value_col <- label_col + 1L
    if (value_col > ncol(grid)) next

    for (row in seq_len(max_rows)) {
      label <- grid[row, label_col]
      if (is.na(label) || !nzchar(trimws(label))) next

      key <- norm_key(split_header(label)$base)
      if (!nzchar(key)) next

      field <- unname(lookup[key])
      if (length(field) == 0 || is.na(field)) next
      if (!is.null(values[[field]])) next          # first hit wins

      value <- cell_value(grid, typed, row, value_col)
      if (is.null(value)) next

      values[[field]] <- value
      provenance[[field]] <- data.frame(
        field = field,
        label = trimws(label),
        row = row,
        col = value_col,
        source = "label",
        stringsAsFactors = FALSE
      )
      # A label carrying its own unit, "Height (cm)", tells us the unit of the
      # value beside it.
      inline <- split_header(label)$unit
      if (!is.na(inline)) {
        attr(values[[field]], "unit") <- inline
      }
    }
  }

  list(
    values = values,
    provenance = if (length(provenance)) do.call(rbind, provenance) else empty_provenance()
  )
}


#' @keywords internal
#' @noRd
empty_provenance <- function() {
  data.frame(field = character(), label = character(), row = integer(),
             col = integer(), source = character(), stringsAsFactors = FALSE)
}


#' Read one cell, preferring the typed value
#'
#' The typed read is what turns an Excel date serial back into a date. It only
#' covers the first rows of the sheet, so the text grid is the fallback.
#'
#' @keywords internal
#' @noRd
cell_value <- function(grid, typed, row, col) {
  if (!is.null(typed) && row <= nrow(typed) && col <= ncol(typed)) {
    value <- typed[[col]][[row]]
    if (!is.null(value) && length(value) == 1L && !is.na(value)) {
      return(value)
    }
  }
  text <- grid[row, col]
  if (is.na(text) || !nzchar(trimws(text)) || trimws(text) %in% c("-", "--", "---")) {
    return(NULL)
  }
  trimws(text)
}


#' Fill gaps from the dialect's declared fixed positions
#'
#' @param found The result of [scan_label_block()].
#' @param grid A character matrix.
#' @param typed The typed read.
#' @param dialect A compiled dialect.
#' @param location The data block location from [locate_data_block()]. Cells
#'   inside the data block are never read as metadata: a bare sheet with no
#'   header block would otherwise have its first rows of measurements read as a
#'   participant's age and weight.
#'
#' @return `found`, with any missing fields filled in and marked as positional.
#'
#' @keywords internal
fill_from_positions <- function(found, grid, typed, dialect, location = NULL) {
  positions <- dialect$layout$header_block$positional
  if (is.null(positions)) {
    return(found)
  }

  in_data_block <- function(row, col) {
    if (is.null(location)) {
      return(FALSE)
    }
    row >= location$header_row && col %in% location$data_cols
  }

  for (field in names(positions)) {
    if (!is.null(found$values[[field]])) next
    where <- unlist(positions[[field]])
    if (length(where) != 2L) next
    row <- where[[1L]]
    col <- where[[2L]]
    if (row > nrow(grid) || col > ncol(grid)) next
    if (in_data_block(row, col)) next

    value <- cell_value(grid, typed, row, col)
    if (is.null(value)) next

    found$values[[field]] <- value
    found$provenance <- rbind(found$provenance, data.frame(
      field = field, label = NA_character_, row = row, col = col,
      source = "position", stringsAsFactors = FALSE
    ))
  }
  found
}


#' Coerce a header-block cell to a date
#'
#' Returns `NULL` rather than today's date when the value cannot be read. The
#' old importer substituted `Sys.Date()` inside a `tryCatch`, so every file
#' whose date arrived as an Excel serial silently reported the day it was
#' imported instead of the day the test was done.
#'
#' @param x A cell value: a date, a `POSIXct`, an Excel serial, or text.
#'
#' @return A `Date`, or `NULL`.
#'
#' @keywords internal
as_test_date <- function(x) {
  if (is.null(x) || length(x) == 0) {
    return(NULL)
  }
  if (inherits(x, "Date")) {
    return(x[[1L]])
  }
  if (inherits(x, "POSIXct")) {
    return(as.Date(x[[1L]], tz = "UTC"))
  }
  if (is.numeric(x)) {
    return(excel_serial_to_date(x[[1L]]))
  }

  text <- trimws(as.character(x)[[1L]])
  if (!nzchar(text)) {
    return(NULL)
  }

  # A bare Excel serial arriving as text. This is exactly what the bundled
  # example file does, and what the old code choked on.
  if (grepl("^[0-9]{5}([.][0-9]+)?$", text)) {
    return(excel_serial_to_date(as.numeric(text)))
  }

  # Day first, because that is the French and international convention. A date
  # such as 01/06/2025 is genuinely ambiguous and is read as 1 June.
  formats <- c("%Y-%m-%d", "%d/%m/%Y", "%m/%d/%Y", "%d-%m-%Y", "%d.%m.%Y",
               "%Y/%m/%d", "%d %B %Y", "%B %d, %Y", "%d %b %Y")
  for (format in formats) {
    parsed <- suppressWarnings(as.Date(text, format = format))
    if (!is.na(parsed) && parsed > as.Date("1900-01-01") &&
        parsed < Sys.Date() + 3650) {
      return(parsed)
    }
  }
  NULL
}


#' @keywords internal
#' @noRd
excel_serial_to_date <- function(x) {
  if (!is.finite(x) || x <= 0 || x > 80000) {
    return(NULL)
  }
  # Excel's day zero is 1899-12-30, which absorbs its 1900 leap-year bug.
  as.Date(floor(x), origin = "1899-12-30")
}


#' Read a numeric header-block value, honouring an inline unit
#'
#' @param x A cell value, possibly carrying a `unit` attribute.
#' @param canonical Canonical name used to pick the target unit.
#' @param range Optional plausible range; a value outside it becomes `NA`.
#'
#' @return A number, or `NA_real_`.
#'
#' @keywords internal
as_measurement <- function(x, canonical = NULL, range = NULL) {
  if (is.null(x) || length(x) == 0) {
    return(NA_real_)
  }
  unit_text <- attr(x, "unit")
  value <- as_numeric_loose(as.character(x)[[1L]])
  if (!is.finite(value)) {
    return(NA_real_)
  }

  # A height written as "1.82 (m)" is converted rather than rejected.
  if (!is.null(canonical) && !is.null(unit_text)) {
    from <- parse_unit(unit_text)
    target <- switch(canonical, height_cm = "cm", weight_kg = "kg", NA_character_)
    if (!is.na(from) && !is.na(target) && from != target) {
      key <- paste(from, target, sep = "|")
      if (key %in% names(.unit_factors)) {
        value <- value * unname(.unit_factors[[key]])
      }
    }
  }

  if (!is.null(range) && (value < range[[1L]] || value > range[[2L]])) {
    return(NA_real_)
  }
  value
}

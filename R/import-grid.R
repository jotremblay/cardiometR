# Reading a file into a plain cell grid, and finding the data block inside it.
#
# The importer deliberately does not trust the position of anything. An
# operator chooses which columns to export, so two files from the same machine
# can differ in width and in where the data starts. The header row is found by
# looking for the row that contains the most recognisable variable names.


#' Read a spreadsheet or delimited file into a character grid
#'
#' Every cell comes back as text. A second, typed read is kept alongside it so
#' that dates in the header block survive as dates instead of Excel serial
#' numbers.
#'
#' @param file Path to the file.
#' @param sheet Sheet name or index for spreadsheets. `NULL` reads the first.
#' @param n_max Maximum rows to read.
#'
#' @return A list with `grid` (a character matrix), `typed` (a list of columns,
#'   or `NULL` for delimited files) and `sheet` (the sheet actually read).
#'
#' @keywords internal
read_raw_grid <- function(file, sheet = NULL, n_max = Inf) {
  extension <- tolower(tools::file_ext(file))

  if (extension %in% c("xlsx", "xls")) {
    return(read_raw_grid_excel(file, sheet = sheet, n_max = n_max))
  }
  read_raw_grid_delimited(file, n_max = n_max)
}


#' @keywords internal
#' @noRd
read_raw_grid_excel <- function(file, sheet = NULL, n_max = Inf) {
  available <- readxl::excel_sheets(file)

  target <- if (is.null(sheet)) {
    available[[1L]]
  } else if (is.numeric(sheet)) {
    available[[as.integer(sheet)]]
  } else {
    sheet
  }
  if (is.character(target) && !target %in% available) {
    cli::cli_abort(c(
      "Sheet {.val {target}} not found in {.file {basename(file)}}.",
      "i" = "This file has: {.val {available}}."
    ))
  }

  text <- readxl::read_excel(
    file, sheet = target, col_names = FALSE, col_types = "text",
    n_max = n_max, .name_repair = "minimal"
  )
  typed <- readxl::read_excel(
    file, sheet = target, col_names = FALSE, col_types = "list",
    n_max = min(n_max, 40), .name_repair = "minimal"
  )

  grid <- as.matrix(as.data.frame(text, stringsAsFactors = FALSE))
  dimnames(grid) <- NULL
  storage.mode(grid) <- "character"

  list(grid = grid, typed = typed, sheet = target)
}


#' @keywords internal
#' @noRd
read_raw_grid_delimited <- function(file, n_max = Inf) {
  lines <- readLines(file, warn = FALSE, encoding = "UTF-8")
  lines <- lines[nzchar(trimws(lines))]
  if (length(lines) == 0) {
    cli::cli_abort("{.file {basename(file)}} is empty.")
  }
  if (is.finite(n_max)) {
    lines <- utils::head(lines, n_max)
  }

  # Pick the separator that splits the first few lines into the most, and the
  # most consistent, number of fields.
  sample <- utils::head(lines, 20)
  separators <- c(";", "\t", ",", "|")
  counts <- vapply(separators, function(sep) {
    fields <- lengths(strsplit(sample, sep, fixed = TRUE))
    if (stats::var(fields) > 0 || fields[[1L]] < 2) 0L else fields[[1L]]
  }, integer(1))
  separator <- separators[[which.max(counts)]]
  if (max(counts) == 0L) {
    separator <- ","
  }

  cells <- strsplit(lines, separator, fixed = TRUE)
  width <- max(lengths(cells))
  grid <- t(vapply(cells, function(row) {
    row <- trimws(gsub('^"|"$', "", row))
    length(row) <- width
    row
  }, character(width)))

  list(grid = grid, typed = NULL, sheet = NA_character_)
}


#' Locate the header, units, and first data row inside a grid
#'
#' Every candidate row is scored by how many distinct canonical variables its
#' cells resolve to. The count of REQUIRED variables dominates the total count,
#' so a header block full of participant labels can never outscore the real
#' header row, however many words it contains.
#'
#' @param grid A character matrix.
#' @param lookup Named character vector, normalised key to canonical name.
#' @param required Canonical names that must be present.
#' @param max_scan How many rows from the top to consider.
#'
#' @return A list with `header_row`, `units_row` (`NA` when there is none),
#'   `data_row`, `data_cols`, and `n_required` found.
#'
#' @keywords internal
locate_data_block <- function(grid, lookup, required, max_scan = 30L) {
  n_scan <- min(max_scan, nrow(grid))
  if (n_scan == 0) {
    cli::cli_abort("The sheet holds no rows.")
  }

  score_row <- function(i) {
    keys <- norm_key(split_header(grid[i, ])$base)
    canonical <- unname(lookup[keys])
    matched_cols <- which(!is.na(canonical))
    list(
      n_canonical = length(unique(stats::na.omit(canonical))),
      n_required = sum(required %in% canonical),
      cols = matched_cols
    )
  }
  scores <- lapply(seq_len(n_scan), score_row)

  ranking <- order(
    -vapply(scores, `[[`, integer(1), "n_required"),
    -vapply(scores, `[[`, integer(1), "n_canonical"),
    seq_len(n_scan)
  )
  header_row <- ranking[[1L]]
  best <- scores[[header_row]]

  if (best$n_required < 3L) {
    cli::cli_abort(c(
      "Could not find a row of column headers in the first {n_scan} rows.",
      "i" = "The closest was row {header_row}, with {best$n_canonical} known \\
             variable{?s} and {best$n_required} of {length(required)} required \\
             ones.",
      "i" = "Run {.fn preview_cpet_columns} to see what the file contains, or \\
             pass {.arg mapping} to name the columns yourself."
    ), class = "cardiometr_header_not_found")
  }

  # Span from the first recognised column to the last column that carries any
  # header at all. Stopping at the last RECOGNISED column would work just as
  # well for reading the data, but it would hide the trailing columns from the
  # import report, and being able to see what was left behind is the point of
  # the report.
  header_cells <- trimws(grid[header_row, ])
  has_header <- !is.na(header_cells) & nzchar(header_cells)
  last_col <- max(which(has_header), max(best$cols))
  data_cols <- seq(min(best$cols), last_col)

  # The first row below the header that is mostly numbers. Scoring a blank row
  # as -1 rather than 0 means a spacer row is skipped instead of ending the
  # search, and it lets "no units row" and "data starts immediately" fall out
  # without being special cases.
  numeric_fraction <- function(i) {
    values <- trimws(grid[i, data_cols])
    values <- values[!is.na(values) & nzchar(values)]
    if (length(values) == 0) {
      return(-1)
    }
    mean(!is.na(suppressWarnings(as_numeric_loose(values))))
  }

  if (header_row >= nrow(grid)) {
    cli::cli_abort("No data rows below the header row.")
  }
  candidates <- seq.int(header_row + 1L, nrow(grid))
  fractions <- vapply(candidates, numeric_fraction, numeric(1))
  first_numeric <- which(fractions >= 0.5)
  if (length(first_numeric) == 0) {
    cli::cli_abort(c(
      "Found a header row but no numeric data below it.",
      "i" = "Header row was {header_row}."
    ))
  }
  data_row <- candidates[[first_numeric[[1L]]]]

  # A units row, if there is one, sits between the header and the data.
  units_row <- NA_integer_
  gap <- seq_len(0)
  if (data_row - header_row > 1L) {
    gap <- seq.int(header_row + 1L, data_row - 1L)
  }
  for (i in gap) {
    values <- trimws(grid[i, data_cols])
    values <- values[!is.na(values) & nzchar(values)]
    if (length(values) > 0 && mean(is_unit_token(values)) >= 0.5) {
      units_row <- i
      break
    }
  }

  list(
    header_row = header_row,
    units_row = units_row,
    data_row = data_row,
    data_cols = data_cols,
    n_required = best$n_required
  )
}

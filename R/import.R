# Reading CPET data files.
#
# One engine serves every supported metabolic cart. What differs between carts
# lives in the YAML files under inst/dialects, not here. The engine finds the
# data block by looking at the content rather than at fixed positions, so a
# different export configuration of the same cart still reads correctly.
#
# The eight steps are: read the grid, choose a dialect, locate the data block,
# resolve the column names, resolve and convert the units, normalise the value
# vocabularies, read the header block by label, and build the object.


#' Read CPET data from a metabolic cart export
#'
#' Reads breath-by-breath or averaged data from a metabolic cart file and
#' returns a [CpetData] object. The file's layout, column names, and units are
#' worked out from its contents, so exports in different languages, and exports
#' configured with different columns, are all handled.
#'
#' @param file Path to the data file.
#' @param format Format name, such as `"cosmed"`. `NULL` detects it from the
#'   file. See [list_cpet_dialects()].
#' @param sheet Sheet name or number, for spreadsheets. `NULL` uses the sheet
#'   the format expects, falling back to the first one.
#' @param mapping Optional named character vector naming columns explicitly, as
#'   `c("Some Header" = "vo2_ml")`. Overrides the format's own vocabulary. Use
#'   [preview_cpet_columns()] to see the headers a file actually contains.
#' @param units Optional named character vector overriding units, as
#'   `c(vo2_ml = "L/min")`, keyed on the internal column names.
#' @param quiet Suppress the summary printed to the console. Defaults to `TRUE`
#'   outside an interactive session.
#' @param ... Reserved for format-specific arguments.
#'
#' @return A [CpetData] object.
#'
#' @details
#' Values are converted into the units the rest of the package expects: VO2 and
#' VCO2 in mL/min, VE in L/min, time in seconds, speed in km/h, power in watts.
#' Where a file states its units, those are used. Where it does not, the
#' magnitude of the data is used instead, and the choice is reported.
#'
#' Phase labels are translated to `rest`, `warmup`, `exercise` and `recovery`
#' whatever language the file is written in. A label that cannot be recognised
#' becomes `NA` and is reported, rather than being guessed at.
#'
#' @examples
#' file <- system.file("extdata", "example_cosmed.xlsx", package = "cardiometR")
#' data <- read_cpet(file, quiet = TRUE)
#' data@participant@name
#'
#' @seealso [preview_cpet_columns()] to inspect a file without importing it,
#'   [list_cpet_dialects()] for the formats available.
#'
#' @export
read_cpet <- function(file,
                      format = NULL,
                      sheet = NULL,
                      mapping = NULL,
                      units = NULL,
                      quiet = !rlang::is_interactive(),
                      ...) {
  if (!file.exists(file)) {
    cli::cli_abort("File not found: {.file {file}}")
  }

  imported <- import_cpet_file(
    file = file, format = format, sheet = sheet,
    mapping = mapping, units = units
  )

  if (!quiet) {
    report_import(imported$report)
  }

  imported$data
}


#' Read a COSMED Omnia or Quark CPET export
#'
#' A convenience wrapper around [read_cpet()] for COSMED files.
#'
#' @inheritParams read_cpet
#' @param sheet Sheet name or number. `NULL` finds the data sheet whatever the
#'   export language calls it, so both `Data` and `Donnees` are handled.
#'
#' @return A [CpetData] object.
#'
#' @examples
#' file <- system.file("extdata", "example_cosmed.xlsx", package = "cardiometR")
#' read_cosmed(file)
#'
#' @export
read_cosmed <- function(file, sheet = NULL, ...) {
  read_cpet(file, format = "cosmed", sheet = sheet, ...)
}


#' Run the import pipeline
#'
#' @inheritParams read_cpet
#'
#' @return A list with `data` (a [CpetData]) and `report` (a list describing
#'   what the import did).
#'
#' @keywords internal
import_cpet_file <- function(file, format = NULL, sheet = NULL,
                             mapping = NULL, units = NULL) {
  detected <- detect_cpet_format(file, sheet = sheet, format = format)
  dialect <- detected$dialect
  raw <- detected$raw

  location <- locate_data_block(
    raw$grid, dialect$lookup, dialect$required,
    max_scan = dialect$layout$data$max_scan_rows %||% 30L
  )

  headers <- raw$grid[location$header_row, location$data_cols]
  resolved <- resolve_columns(headers, dialect$lookup,
                              ignore = dialect$ignore,
                              ignore_patterns = dialect$ignore_patterns,
                              user_mapping = mapping)

  declared <- if (is.na(location$units_row)) {
    rep(NA_character_, length(headers))
  } else {
    parse_unit(raw$grid[location$units_row, location$data_cols])
  }

  extracted <- extract_breaths(
    grid = raw$grid, location = location, resolved = resolved,
    declared_units = declared, dialect = dialect, user_units = units
  )

  vocab <- normalize_breath_vocabularies(extracted$breaths, dialect)
  breaths <- vocab$breaths

  header_block <- fill_from_positions(
    scan_label_block(raw$grid, raw$typed, dialect),
    raw$grid, raw$typed, dialect, location = location
  )

  participant <- build_participant(header_block, dialect)
  metadata <- build_metadata(header_block, dialect, device_label = dialect$label)

  breath_type <- detect_data_type(breaths)

  data <- CpetData(
    participant = participant$value,
    metadata = metadata$value,
    breaths = breaths,
    stages = NULL,
    is_averaged = breath_type$is_averaged,
    averaging_window = breath_type$averaging_window
  )

  report <- list(
    file = file,
    dialect = dialect$name,
    dialect_label = dialect$label,
    dialect_score = detected$score,
    dialect_why = detected$why,
    sheet = raw$sheet,
    layout = location,
    columns = extracted$columns,
    unknown = resolved$unknown,
    ignored = resolved$ignored,
    conflicts = resolved$conflicts,
    suggestions = resolved$suggestions,
    vocab = vocab$table,
    metadata_provenance = header_block$provenance,
    warnings = c(extracted$warnings, vocab$warnings,
                 participant$warnings, metadata$warnings)
  )

  list(data = data, report = report)
}


#' Choose the dialect for a file
#'
#' Reads a preview of the file, scores every available dialect against it, and
#' returns the best match together with the grid it already read, so the file
#' is not opened twice.
#'
#' @inheritParams read_cpet
#'
#' @return A list with `dialect`, `raw`, `score` and `why`.
#'
#' @keywords internal
detect_cpet_format <- function(file, sheet = NULL, format = NULL) {
  extension <- tolower(tools::file_ext(file))

  read_with <- function(dialect) {
    target <- sheet
    if (is.null(target) && extension %in% c("xlsx", "xls") &&
        length(dialect$sheet) > 0) {
      available <- readxl::excel_sheets(file)
      preferred <- unlist(dialect$sheet)
      hit <- preferred[norm_key(preferred) %in% norm_key(available)]
      if (length(hit) > 0) {
        target <- available[[which(norm_key(available) == norm_key(hit[[1L]]))[[1L]]]]
      }
    }
    read_raw_grid(file, sheet = target)
  }

  if (!is.null(format)) {
    dialect <- load_dialect(format)
    return(list(dialect = dialect, raw = read_with(dialect),
                score = NA_real_, why = "format given by the caller"))
  }

  names_available <- names(find_dialect_files())
  if (length(names_available) == 0) {
    cli::cli_abort("No import formats are installed.")
  }

  sheet_names <- if (extension %in% c("xlsx", "xls")) {
    tryCatch(readxl::excel_sheets(file), error = function(e) character())
  } else {
    character()
  }

  best <- NULL
  for (name in names_available) {
    dialect <- load_dialect(name)
    raw <- tryCatch(read_with(dialect), error = function(e) NULL)
    if (is.null(raw)) next

    header_candidates <- as.character(
      raw$grid[seq_len(min(5L, nrow(raw$grid))), , drop = FALSE]
    )
    scored <- score_dialect(dialect, list(
      extension = extension,
      sheet_names = sheet_names,
      grid = raw$grid,
      header_candidates = header_candidates
    ))
    if (is.null(best) || scored$score > best$score) {
      best <- list(dialect = dialect, raw = raw,
                   score = scored$score, why = scored$why)
    }
  }

  if (is.null(best)) {
    cli::cli_abort(c(
      "Could not read {.file {basename(file)}} with any known format.",
      "i" = "Formats available: {.val {names_available}}."
    ))
  }
  if (length(best$why) == 0) {
    best$why <- "no format matched strongly; used the best available"
  }
  best$why <- paste(best$why, collapse = "; ")
  best
}


#' Pull the data block out of the grid and put it in canonical units
#'
#' @keywords internal
#' @noRd
extract_breaths <- function(grid, location, resolved, declared_units,
                            dialect, user_units = NULL) {
  rows <- seq.int(location$data_row, nrow(grid))
  block <- grid[rows, location$data_cols, drop = FALSE]

  canonical <- unname(resolved$mapping)
  inline <- unname(resolved$inline_units)
  source_names <- names(resolved$mapping)

  columns <- list()
  values <- list()
  warnings <- character()

  for (j in seq_along(canonical)) {
    target <- canonical[[j]]
    if (is.na(target)) {
      columns[[length(columns) + 1L]] <- data.frame(
        source = source_names[[j]], canonical = NA_character_,
        unit_from = NA_character_, unit_to = NA_character_,
        factor = NA_real_, unit_source = NA_character_,
        status = if (source_names[[j]] %in% resolved$ignored) "ignored" else "unrecognised",
        stringsAsFactors = FALSE
      )
      next
    }

    raw_values <- block[, j]

    if (target %in% dialect$text_columns) {
      values[[target]] <- trimws(as.character(raw_values))
      columns[[length(columns) + 1L]] <- data.frame(
        source = source_names[[j]], canonical = target,
        unit_from = NA_character_, unit_to = NA_character_,
        factor = NA_real_, unit_source = "text", status = "mapped",
        stringsAsFactors = FALSE
      )
      next
    }

    numeric_values <- if (identical(target, "time_s")) {
      as_seconds(raw_values)
    } else {
      as_numeric_loose(raw_values)
    }

    resolution <- resolve_unit(
      values = numeric_values, canonical = target,
      declared = declared_units[[j]], inline = inline[[j]],
      dialect = dialect, user_units = user_units
    )
    converted <- convert_unit(numeric_values, target,
                              from = resolution$unit, source = resolution$source)

    values[[target]] <- converted$values
    if (!is.na(resolution$note)) {
      warnings <- c(warnings, resolution$note)
    }
    columns[[length(columns) + 1L]] <- data.frame(
      source = source_names[[j]], canonical = target,
      unit_from = converted$from %||% NA_character_,
      unit_to = converted$to %||% NA_character_,
      factor = converted$factor, unit_source = converted$source,
      status = "mapped", stringsAsFactors = FALSE
    )
  }

  breaths <- tibble::as_tibble(values)

  missing_required <- setdiff(dialect$required, names(breaths))
  if (length(missing_required) > 0) {
    cli::cli_abort(c(
      "This file is missing data cardiometR needs: {.field {missing_required}}.",
      "i" = "Recognised: {.field {intersect(dialect$required, names(breaths))}}.",
      "i" = "Run {.fn preview_cpet_columns} to see the file's own column names, \\
             then pass {.arg mapping} to match them up."
    ), class = "cardiometr_missing_required")
  }

  present <- intersect(dialect$required, names(breaths))
  keep <- Reduce(`&`, lapply(present, function(col) !is.na(breaths[[col]])))
  breaths <- breaths[keep, , drop = FALSE]

  if (nrow(breaths) == 0) {
    cli::cli_abort(c(
      "Every data row was discarded because a required value was missing.",
      "i" = "Check the units row and the first data row of the file."
    ), class = "cardiometr_no_data")
  }

  # Required columns first, then the rest in a stable order. Anything not in
  # the units table, such as the text columns, keeps its place at the end
  # rather than being dropped.
  preferred <- c(dialect$required,
                 setdiff(names(.canonical_units), dialect$required))
  ordered <- c(intersect(preferred, names(breaths)),
               setdiff(names(breaths), preferred))
  breaths <- breaths[, ordered, drop = FALSE]

  # Phase is kept even when partly unrecognised, because it carries meaning
  # the analysis needs. An empty event-marker column carries none.
  breaths <- drop_empty_columns(breaths, protect = c(dialect$required, "phase"))

  list(
    breaths = breaths,
    columns = do.call(rbind, columns),
    warnings = warnings
  )
}


#' Decide which unit a column's values are in
#'
#' Priority, highest first: an explicit override, the file's units row, a unit
#' written into the header itself, the magnitude of the data, and finally
#' whatever the format declares.
#'
#' Time is the exception. COSMED labels its time column as seconds while
#' storing Excel day fractions, so for time the magnitude wins: a whole test
#' spanning less than two units cannot be seconds.
#'
#' @keywords internal
#' @noRd
resolve_unit <- function(values, canonical, declared, inline, dialect,
                         user_units = NULL) {
  none <- NA_character_
  note <- NA_character_

  if (!is.null(user_units) && canonical %in% names(user_units)) {
    return(list(unit = parse_unit(user_units[[canonical]]),
                source = "user", note = none))
  }

  inline_unit <- if (is.na(inline)) NA_character_ else parse_unit(inline)
  stated <- if (!is.na(declared)) declared else inline_unit
  stated_source <- if (!is.na(declared)) "declared" else "inline"

  guess <- infer_unit_from_magnitude(values, canonical)

  if (identical(canonical, "time_s") && identical(guess$unit, "day")) {
    return(list(unit = "day", source = "heuristic", note = none))
  }

  if (!is.na(stated)) {
    if (identical(guess$confidence, "high") && !is.na(guess$unit) &&
        guess$unit != stated) {
      note <- sprintf(
        "%s: the file says %s but the values look like %s (%s). Kept %s.",
        canonical, stated, guess$unit, guess$note, stated
      )
    }
    return(list(unit = stated, source = stated_source, note = note))
  }

  if (!is.na(guess$unit) && guess$confidence %in% c("high", "low")) {
    if (identical(guess$confidence, "low")) {
      note <- sprintf("%s: no unit given; assumed %s (%s).",
                      canonical, guess$unit,
                      guess$note %||% "from the size of the values")
    }
    return(list(unit = guess$unit, source = "heuristic", note = note))
  }

  fallback <- if (canonical %in% names(dialect$declared_units)) {
    unname(dialect$declared_units[[canonical]])
  } else {
    NA_character_
  }
  list(unit = if (is.na(fallback)) none else parse_unit(fallback),
       source = "assumed", note = none)
}


#' Translate phase labels into the canonical vocabulary
#'
#' @keywords internal
#' @noRd
normalize_breath_vocabularies <- function(breaths, dialect) {
  table <- NULL
  warnings <- character()

  if ("phase" %in% names(breaths)) {
    mapped <- normalize_phase(breaths$phase, vocab = dialect$phase_vocab)
    breaths$phase <- mapped$values
    table <- mapped$table
    if (length(mapped$unmapped) > 0) {
      warnings <- c(warnings, sprintf(
        "Phase label%s %s could not be recognised, so those rows are unclassified.",
        if (length(mapped$unmapped) > 1) "s" else "",
        paste(sQuote(mapped$unmapped), collapse = ", ")
      ))
    }
  }

  list(breaths = breaths, table = table, warnings = warnings)
}


#' @keywords internal
#' @noRd
build_participant <- function(header_block, dialect) {
  found <- header_block$values
  warnings <- character()

  first_name <- as.character(found$first_name %||% "")
  last_name <- as.character(found$last_name %||% "")
  name <- trimws(paste(first_name, last_name))
  if (!nzchar(name)) {
    name <- "Unknown"
  }

  age <- as_measurement(found$age, range = c(0, 120))
  height <- as_measurement(found$height_cm, "height_cm", range = c(50, 250))
  weight <- as_measurement(found$weight_kg, "weight_kg", range = c(10, 300))

  if (is.na(age)) {
    warnings <- c(warnings, "Age was not found or is out of range; used 30.")
    age <- 30
  }
  if (is.na(height)) {
    warnings <- c(warnings, "Height was not found or is out of range; used 170 cm.")
    height <- 170
  }
  if (is.na(weight)) {
    warnings <- c(warnings, "Weight was not found or is out of range; used 70 kg.")
    weight <- 70
  }

  value <- Participant(
    id = as.character(found$participant_id %||% "Unknown"),
    name = name,
    age = age,
    sex = normalize_sex(found$sex, vocab = dialect$sex_vocab),
    height_cm = height,
    weight_kg = weight,
    sport = NULL,
    date_of_birth = as_test_date(found$date_of_birth)
  )
  list(value = value, warnings = warnings)
}


#' @keywords internal
#' @noRd
build_metadata <- function(header_block, dialect, device_label) {
  found <- header_block$values
  warnings <- character()

  test_date <- as_test_date(found$test_date)
  if (is.null(test_date)) {
    warnings <- c(warnings,
                  "The test date could not be read, so today's date was used.")
    test_date <- Sys.Date()
  }

  ergometer <- as.character(found$ergometer %||% "")
  protocol <- as.character(found$protocol %||% "Unknown")
  device <- if (nzchar(ergometer)) {
    paste(device_label, "-", ergometer)
  } else {
    device_label
  }

  modality <- detect_modality_from_text(
    c(protocol, ergometer), patterns = dialect$modality_patterns
  )

  value <- CpetMetadata(
    test_date = test_date,
    device = device,
    protocol = protocol,
    calibration_date = NULL,
    temperature_c = or_null(as_measurement(found$temperature_c, range = c(10, 40))),
    pressure_mmhg = or_null(as_measurement(found$pressure_mmhg, range = c(600, 900))),
    humidity_pct = or_null(as_measurement(found$humidity_pct, range = c(0, 100))),
    technician = NULL,
    modality = if (is.na(modality)) NULL else modality
  )
  list(value = value, warnings = warnings)
}


#' Print a summary of what an import did
#'
#' @param report The report list returned by [import_cpet_file()].
#'
#' @return Invisibly, the report.
#'
#' @keywords internal
report_import <- function(report) {
  cli::cli_h1("CPET import")
  cli::cli_dl(c(
    "File" = basename(report$file),
    "Format" = "{report$dialect_label}",
    "Sheet" = as.character(report$sheet %||% "-"),
    "Layout" = "header row {report$layout$header_row}, data from row \\
                {report$layout$data_row}"
  ))

  columns <- report$columns
  mapped <- columns[columns$status == "mapped", , drop = FALSE]
  cli::cli_alert_success("{nrow(mapped)} column{?s} recognised")

  converted <- mapped[!is.na(mapped$factor) & mapped$factor != 1, , drop = FALSE]
  if (nrow(converted) > 0) {
    cli::cli_h3("Units converted")
    for (i in seq_len(nrow(converted))) {
      row <- converted[i, ]
      cli::cli_li("{.val {row$source}} to {.field {row$canonical}}: \\
                   {row$unit_from} to {row$unit_to}, \\
                   x{signif(row$factor, 6)}")
    }
  }

  if (!is.null(report$vocab) && nrow(report$vocab) > 0) {
    cli::cli_h3("Phase labels")
    for (i in seq_len(nrow(report$vocab))) {
      row <- report$vocab[i, ]
      cli::cli_li("{.val {row$raw}} to {.val {row$canonical %||% 'unrecognised'}} \\
                   ({row$n} row{?s})")
    }
  }

  if (length(report$unknown) > 0) {
    cli::cli_h3("Not recognised, and left out")
    cli::cli_ul(report$unknown)
    for (name in names(report$suggestions)) {
      cli::cli_alert_info("{.val {name}}: did you mean \\
                           {.val {report$suggestions[[name]]}}?")
    }
  }

  if (!is.null(report$conflicts) && nrow(report$conflicts) > 0) {
    for (i in seq_len(nrow(report$conflicts))) {
      row <- report$conflicts[i, ]
      cli::cli_alert_warning("{.field {row$canonical}}: kept {.val {row$kept}}, \\
                              left out {.val {row$dropped}}")
    }
  }

  for (warning in report$warnings) {
    cli::cli_alert_warning(warning)
  }

  invisible(report)
}


#' Detect whether data is breath-by-breath or already averaged
#'
#' @param breaths A data frame of breath data.
#'
#' @return A list with `is_averaged` and `averaging_window`.
#'
#' @keywords internal
detect_data_type <- function(breaths) {
  if (nrow(breaths) < 10 || !"time_s" %in% names(breaths)) {
    return(list(is_averaged = FALSE, averaging_window = NULL))
  }

  intervals <- diff(breaths$time_s)
  intervals <- intervals[intervals > 0 & intervals < 60]

  if (length(intervals) < 5) {
    return(list(is_averaged = FALSE, averaging_window = NULL))
  }

  median_interval <- stats::median(intervals)
  cv <- stats::sd(intervals) / mean(intervals)

  common_windows <- c(5, 10, 15, 20, 30)
  matched_window <- common_windows[which.min(abs(common_windows - median_interval))]

  if (cv < 0.05 && abs(median_interval - matched_window) < 1) {
    list(is_averaged = TRUE, averaging_window = matched_window)
  } else {
    list(is_averaged = FALSE, averaging_window = NULL)
  }
}

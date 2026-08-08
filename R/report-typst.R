# Typst rendering helpers for cardiometR reports

process_conditionals <- function(content, data) {
  # Find the first {{#if ...}} tag
  if_start <- regexpr("\\{\\{#if ([^}]+)\\}\\}", content, perl = TRUE)
  if (if_start == -1) return(content)

  # Extract variable name
  match_len <- attr(if_start, "match.length")
  tag <- substr(content, if_start, if_start + match_len - 1)
  var_name <- trimws(sub("\\{\\{#if ([^}]+)\\}\\}", "\\1", tag))


  # Find the matching {{/if}} by counting nesting levels
  pos <- if_start + match_len
  depth <- 1
  else_pos <- NA
  end_pos <- NA

  while (pos <= nchar(content) && depth > 0) {
    # Check for {{#if at current position
    if (substr(content, pos, pos + 5) == "{{#if ") {
      depth <- depth + 1
      pos <- pos + 6
    # Check for {{/if}} at current position
    } else if (substr(content, pos, pos + 6) == "{{/if}}") {
      depth <- depth - 1
      if (depth == 0) {
        end_pos <- pos
      }
      pos <- pos + 7
    # Check for {{else}} at current position (only at depth 1)
    } else if (depth == 1 && substr(content, pos, pos + 7) == "{{else}}") {
      else_pos <- pos
      pos <- pos + 8
    } else {
      pos <- pos + 1
    }
  }

  # If no matching end found, return content unchanged

  if (is.na(end_pos)) return(content)

  # Extract if-content and else-content
  if_content_start <- if_start + match_len
  if (!is.na(else_pos)) {
    if_content <- substr(content, if_content_start, else_pos - 1)
    else_content <- substr(content, else_pos + 8, end_pos - 1)
  } else {
    if_content <- substr(content, if_content_start, end_pos - 1)
    else_content <- ""
  }

  # Check if variable is truthy
  var_value <- data[[var_name]]
  is_truthy <- !is.null(var_value) && length(var_value) > 0 &&
    !identical(var_value, FALSE) && !identical(var_value, "")

  # Choose replacement and recursively process it
  replacement <- if (is_truthy) if_content else else_content
  replacement <- process_conditionals(replacement, data)

  # Build result
  before <- if (if_start > 1) substr(content, 1, if_start - 1) else ""
  after <- if (end_pos + 7 <= nchar(content)) substr(content, end_pos + 7, nchar(content)) else ""

  # Recursively process the rest
  result <- paste0(before, replacement, after)
  process_conditionals(result, data)
}


#' Render Typst Report
#'
#' @description
#' Renders the Typst template with data interpolation.
#'
#' @param template_path Path to Typst template
#' @param data Named list of template data
#' @param output_file Output PDF path
#' @keywords internal
render_typst_report <- function(template_path, data, output_file) {
  # Read template
  template_content <- paste(readLines(template_path, warn = FALSE, encoding = "UTF-8"), collapse = "\n")

  # Process conditionals recursively to handle nesting properly
  template_content <- process_conditionals(template_content, data)

  # Simple mustache-style variable interpolation
  for (name in names(data)) {
    value <- data[[name]]
    if (is.null(value) || length(value) == 0) {
      value <- ""
    } else if (is.logical(value)) {
      # Skip logical values (used for conditionals)
      next
    } else {
      value <- as.character(value[[1]])
      # Replace NA strings with empty or dash
      if (is.na(value) || identical(value, "NA")) {
        value <- "-"
      }
      value <- enc2utf8(value)
    }
    pattern <- paste0("{{", name, "}}")
    template_content <- gsub(pattern, value, template_content, fixed = TRUE)
  }

  # Remove any remaining unresolved mustache patterns to prevent Typst errors
  template_content <- gsub("\\{\\{#if [^}]+\\}\\}", "", template_content)
  template_content <- gsub("\\{\\{/if\\}\\}", "", template_content)
  template_content <- gsub("\\{\\{else\\}\\}", "", template_content)
  template_content <- gsub("\\{\\{[a-zA-Z_][a-zA-Z0-9_]*\\}\\}", "", template_content)

  # Create temp directory for template and images
  temp_dir <- tempfile(pattern = "typst_")
  dir.create(temp_dir)
  on.exit(unlink(temp_dir, recursive = TRUE), add = TRUE)

  # Copy any image files to the temp directory and update paths
  image_vars <- c("graph_panel", "graph_vslope", "graph_predicted",
                  "graph_slope", "graph_zstrip", "graph_longitudinal",
                  "logo_path", "lab_logo_path", "epic_logo_path")
  for (var in image_vars) {
    val <- data[[var]]
    if (!is.null(val) && length(val) > 0 && !anyNA(val) && nchar(val) > 0 && file.exists(val)) {
      old_path <- data[[var]]
      new_name <- basename(old_path)
      new_path <- file.path(temp_dir, new_name)
      file.copy(old_path, new_path)
      # Update the path in template_content to use just the filename
      template_content <- gsub(old_path, new_name, template_content, fixed = TRUE)
    }
  }

  # Write interpolated template
  temp_typ <- file.path(temp_dir, "report.typ")
  writeLines(enc2utf8(template_content), temp_typ, useBytes = TRUE)

  # Typst requires the output file to have a .pdf extension.
  # Shiny downloadHandler passes a temp path without one, so compile to a

  # properly-named temp file and copy back when needed.
  needs_rename <- !grepl("[.]pdf$", output_file, ignore.case = TRUE)
  compile_target <- if (needs_rename) {
    file.path(temp_dir, "report.pdf")
  } else {
    output_file
  }

  # Render with typr
  if (requireNamespace("typr", quietly = TRUE)) {
    tryCatch(
      typr::typr_compile(input = temp_typ, output_file = compile_target, output_format = "pdf"),
      error = function(e) {
        # Try system typst as fallback for better error messages
        result <- system2("typst", args = c("compile", temp_typ, compile_target),
                          stdout = TRUE, stderr = TRUE)
        if (!file.exists(compile_target)) {
          cli::cli_abort(c(
            "Typst compilation failed",
            "x" = paste(result, collapse = "\n")
          ))
        }
      }
    )
  } else {
    # Fallback: try system typst
    result <- system2("typst", args = c("compile", temp_typ, compile_target),
                      stdout = TRUE, stderr = TRUE)
    if (!file.exists(compile_target)) {
      cli::cli_abort(c(
        "Failed to render Typst template",
        "i" = "Install the {.pkg typr} package or ensure Typst is installed",
        "x" = paste(result, collapse = "\n")
      ))
    }
  }

  if (needs_rename) {
    file.copy(compile_target, output_file, overwrite = TRUE)
  }

  validate_pdf_output(output_file)

  # temp_dir cleaned up via on.exit
}

validate_pdf_output <- function(path) {
  if (!file.exists(path)) {
    cli::cli_abort("Report output file was not created: {.file {path}}")
  }

  size <- file.info(path)$size
  if (is.na(size) || size == 0) {
    cli::cli_abort("Report output file is empty: {.file {path}}")
  }

  header <- tryCatch(
    rawToChar(readBin(path, what = "raw", n = 4)),
    error = function(e) ""
  )

  if (!identical(header, "%PDF")) {
    cli::cli_abort(c(
      "Report output is not a valid PDF",
      "i" = "Ensure Typst/typr can render PDFs on this system"
    ))
  }
}


#' Escape Typst Special Characters
#'
#' @param x Character string to escape
#' @return Escaped string safe for Typst content blocks
#' @keywords internal
escape_typst <- function(x) {
  if (is.null(x) || length(x) == 0 || is.na(x[1])) return(x)
  x <- enc2utf8(as.character(x))
  x <- gsub("\\", "\\\\", x, fixed = TRUE)
  x <- gsub("#", "\\#", x, fixed = TRUE)
  x <- gsub("[", "\\[", x, fixed = TRUE)
  x <- gsub("]", "\\]", x, fixed = TRUE)
  x <- gsub("_", "\\_", x, fixed = TRUE)
  x <- gsub("*", "\\*", x, fixed = TRUE)
  x <- gsub("$", "\\$", x, fixed = TRUE)
  x <- gsub("@", "\\@", x, fixed = TRUE)
  x <- gsub("<", "\\<", x, fixed = TRUE)
  x <- gsub(">", "\\>", x, fixed = TRUE)
  x <- gsub("~", "\\~", x, fixed = TRUE)
  x <- gsub("`", "\\`", x, fixed = TRUE)
  x
}


#' Format Duration
#'
#' @param seconds Duration in seconds
#' @return Formatted string (MM:SS)
#' @keywords internal
format_duration <- function(seconds) {
  mins <- floor(seconds / 60)
  secs <- round(seconds %% 60)
  sprintf("%d:%02d", mins, secs)
}


#' Format Nutritional State
#'
#' @param state Character: "fed" or "fasted"
#' @param language Language code ("en" or "fr")
#' @return Formatted string
#' @keywords internal
format_nutritional_state <- function(state, language = "en") {
  if (is.null(state) || length(state) == 0) return("-")
  if (language == "fr") {
    switch(state,
      fed = "Nourri",
      fasted = "\u00c0 jeun",
      state
    )
  } else {
    switch(state,
      fed = "Fed",
      fasted = "Fasted",
      state
    )
  }
}


#' Format Fatigue State
#'
#' @param state Character: "rested" or "fatigued"
#' @param language Language code ("en" or "fr")
#' @return Formatted string
#' @keywords internal
format_fatigue_state <- function(state, language = "en") {
  if (is.null(state) || length(state) == 0) return("-")
  if (language == "fr") {
    switch(state,
      rested = "Repos\u00e9",
      fatigued = "Fatigu\u00e9",
      state
    )
  } else {
    switch(state,
      rested = "Rested",
      fatigued = "Fatigued",
      state
    )
  }
}


#' Format Modality
#'
#' @param modality Character: "cycling" or "treadmill"
#' @param language Language code ("en" or "fr")
#' @return Formatted string
#' @keywords internal
format_modality <- function(modality, language = "en") {
  if (is.null(modality) || length(modality) == 0) return("-")
  if (language == "fr") {
    switch(modality,
      cycling = "Ergocycle",
      treadmill = "Tapis roulant",
      other = "Autre",
      modality
    )
  } else {
    switch(modality,
      cycling = "Cycle Ergometer",
      treadmill = "Treadmill",
      other = "Other",
      modality
    )
  }
}


#' Format Stage Table for Typst
#'
#' @description
#' Converts stage summary data frame to Typst table row syntax.
#'
#' @param stage_summary Data frame with stage summary data
#' @param language Language code ("en" or "fr")
#' @return Character string with complete Typst table
#' @keywords internal
format_stage_table_typst <- function(stage_summary, language = "en", modality = NULL) {
  n_stages <- nrow(stage_summary)


  # Check if lactate data exists

has_lactate <- "lactate_mmol" %in% names(stage_summary) &&
    any(!is.na(stage_summary$lactate_mmol))

  # Define headers based on language (with proper subscripts for Typst)
  # Modality-aware intensity column header
  intensity_header <- if (identical(modality, "treadmill")) {
    if (language == "fr") "Vitesse (km/h)" else "Speed (km/h)"
  } else if (identical(modality, "cycling")) {
    if (language == "fr") "Puissance (W)" else "Power (W)"
  } else {
    if (language == "fr") "Intensit\u00e9" else "Intensity"
  }

  if (language == "fr") {
    headers <- c("Palier", "Dur\u00e9e", intensity_header, "FC", "VE", 'VO#sub[2]', "QR")
    if (has_lactate) headers <- c(headers, "Lactate")
  } else {
    headers <- c("Stage", "Duration", intensity_header, "HR", "VE", 'VO#sub[2]', "RER")
    if (has_lactate) headers <- c(headers, "Lactate")
  }

  # Build header row (use text(weight: "bold") for headers with Typst markup)
  header_cells <- purrr::map_chr(headers, function(h) {
    if (grepl("#", h, fixed = TRUE)) {
      sprintf("[#text(weight: \"bold\")[%s]]", h)
    } else {
      sprintf("[*%s*]", h)
    }
  })
  header_row <- paste(header_cells, collapse = ", ")

  # Build data rows
  rows <- purrr::map_chr(seq_len(n_stages), function(i) {
    row <- stage_summary[i, ]

    # Get stage number (use row index if not in data)
    stage_num <- if ("stage" %in% names(row)) row$stage else i

    # Get duration
    duration_str <- if ("duration_s" %in% names(row) && !is.na(row$duration_s)) {
      format_duration(row$duration_s)
    } else {
      "-"
    }

    # Get intensity (power for cycling, speed for treadmill)
    intensity <- if ("power_w" %in% names(row) && !is.na(row$power_w) && row$power_w > 0) {
      sprintf("%.0f W", row$power_w)
    } else if ("speed_kmh" %in% names(row) && !is.na(row$speed_kmh)) {
      sprintf("%.1f km/h", row$speed_kmh)
    } else {
      "-"
    }

    # Get HR
    hr <- if ("hr_bpm" %in% names(row) && !is.na(row$hr_bpm)) {
      sprintf("%.0f", row$hr_bpm)
    } else {
      "-"
    }

    # Get VE
    ve <- if ("ve_l" %in% names(row) && !is.na(row$ve_l)) {
      sprintf("%.1f", row$ve_l)
    } else {
      "-"
    }

    # Get VO2
    vo2 <- if ("vo2_ml" %in% names(row) && !is.na(row$vo2_ml)) {
      sprintf("%.0f", row$vo2_ml)
    } else {
      "-"
    }

    # Get VCO2
    vco2 <- if ("vco2_ml" %in% names(row) && !is.na(row$vco2_ml)) {
      sprintf("%.0f", row$vco2_ml)
    } else {
      "-"
    }

    # Get RER
    rer <- if ("rer" %in% names(row) && !is.na(row$rer)) {
      sprintf("%.2f", row$rer)
    } else {
      "-"
    }

    # Build row - 7 columns standard, 8 with lactate
    if (has_lactate) {
      lactate <- if ("lactate_mmol" %in% names(row) && !is.na(row$lactate_mmol)) {
        sprintf("%.1f", row$lactate_mmol)
      } else {
        "-"
      }
      sprintf(
        "  [%s], [%s], [%s], [%s], [%s], [%s], [%s], [%s]",
        stage_num, duration_str, intensity, hr, ve, vo2, rer, lactate
      )
    } else {
      sprintf(
        "  [%s], [%s], [%s], [%s], [%s], [%s], [%s]",
        stage_num, duration_str, intensity, hr, ve, vo2, rer
      )
    }
  })

  # Build complete Typst table
  n_cols <- if (has_lactate) 8 else 7
  col_spec <- paste(rep("1fr", n_cols), collapse = ", ")

  sprintf(
    '#table(
  columns: (%s),
  align: center,
  inset: (x: 2mm, y: 1.4mm),
  stroke: (x, y) => (
    top: if y == 0 { 1pt + rgb("#16181c") } else if y == 1 { 0.5pt + rgb("#16181c") } else { 0.4pt + rgb("#e6e9ee") },
    bottom: 0.4pt + rgb("#e6e9ee")
  ),
  %s,
%s
)',
    col_spec,
    header_row,
    paste(rows, collapse = ",\n")
  )
}


#' Get Institution Logo Path
#'
#' @description
#' Returns the path to a built-in institution logo for use in reports.
#' Available logos: "udem" (UdeM - Ecole de kinesiologie),
#' "epic" (Centre EPIC - Institut de Cardiologie de Montreal).
#'
#' @param institution Institution identifier: "udem" or "epic"
#'
#' @return Character string with the full path to the logo file
#'
#' @examples
#' # Get UdeM logo
#' logo <- get_logo("udem")
#'
#' # Use in report config
#' \dontrun{
#' config <- ReportConfig(
#'   institution = "Ecole de kinesiologie, UdeM",
#'   logo_path = get_logo("udem")
#' )
#' }
#'
#' @export
get_logo <- function(institution = c("udem", "epic")) {
  institution <- match.arg(institution)

  logo_file <- switch(institution,
    "udem" = "Ec-_kinesiologie_-act_-phy_officiel-RVB.png",
    "epic" = "Centre_EPIC_ICM.jpg"
  )

  logo_path <- system.file("assets", logo_file, package = "cardiometR")

  if (!file.exists(logo_path)) {
    cli::cli_warn("Logo file not found: {.file {logo_file}}")
    return(NULL)
  }

  logo_path
}


#' Create Summary Table
#'
#' @description
#' Creates a gt summary table for CPET results.
#'
#' @param analysis CpetAnalysis object
#' @param language Language code
#' @param prediction_source Prediction equation source: "jones" or "prefaut"
#'
#' @return A gt table object
#'
#' @export
create_summary_table <- function(analysis, language = "en", prediction_source = "jones") {
  if (!requireNamespace("gt", quietly = TRUE)) {
    cli::cli_abort("Package {.pkg gt} is required for tables")
  }

  peaks <- analysis@peaks
  participant <- analysis@data@participant
  predicted <- calculate_predicted_values(participant, prediction_source = prediction_source)

  if (is.null(peaks) || length(peaks@vo2_peak) == 0) {
    cli::cli_abort("No peak values available in analysis")
  }

  # Build summary data
  summary_data <- tibble::tibble(
    Parameter = c(
      "VO2 peak (mL/min)",
      "VO2 peak (mL/kg/min)",
      "VE peak (L/min)",
      "HR peak (bpm)",
      "RER peak",
      "Power peak (W)"
    ),
    Value = c(
      round(peaks@vo2_peak, 0),
      round(peaks@vo2_kg_peak, 1),
      round(peaks@ve_peak, 1),
      if (!is.null(peaks@hr_peak)) round(peaks@hr_peak, 0) else NA,
      round(peaks@rer_peak, 2),
      if (!is.null(peaks@power_peak)) round(peaks@power_peak, 0) else NA
    ),
    Predicted = c(
      round(predicted$vo2_max, 0),
      round(predicted$vo2_max / participant@weight_kg, 1),
      round(predicted$ve_max, 0),
      round(predicted$hr_max, 0),
      NA,
      round(predicted$power_max, 0)
    )
  )

  summary_data <- summary_data |>
    dplyr::mutate(
      `% Predicted` = dplyr::if_else(
        !is.na(Predicted) & !is.na(Value),
        round(100 * Value / Predicted, 0),
        NA_real_
      )
    )

  # Create gt table
  tbl <- gt::gt(summary_data) |>
    gt::tab_header(
      title = if (language == "fr") "R\u00e9sultats Maximaux" else "Peak Results"
    ) |>
    gt::fmt_number(columns = c(Value, Predicted), decimals = 0, use_seps = TRUE) |>
    gt::fmt_number(columns = `% Predicted`, decimals = 0, pattern = "{x}%") |>
    gt::sub_missing(missing_text = "-") |>
    gt::tab_style(
      style = gt::cell_fill(color = "#2E86AB20"),
      locations = gt::cells_body(rows = 1:2)
    ) |>
    gt::tab_options(
      table.font.size = gt::px(12),
      heading.title.font.size = gt::px(14),
      heading.title.font.weight = "bold"
    )

  tbl
}


#' Build the Test-Validity Block for the Report
#'
#' @description
#' Runs the ACSM maximal-effort criteria and turns the verdict into the
#' one-line banner the report opens with.
#'
#' @param analysis A [CpetAnalysis] object.
#' @param labels The label list for the report language.
#' @param language Language code.
#'
#' @return A named list of template values.
#'
#' @keywords internal
build_validity_block <- function(analysis, labels, language = "en") {
  criteria <- tryCatch(
    assess_maximal_criteria(analysis@data),
    error = function(e) NULL
  )
  if (is.null(criteria)) {
    return(list(has_validity = FALSE))
  }

  confirmed <- identical(criteria@determination, "maximal")
  peaks <- analysis@peaks

  detail_parts <- character()
  if (!is.null(peaks)) {
    if (length(peaks@rer_peak) > 0 && is.finite(peaks@rer_peak)) {
      detail_parts <- c(detail_parts,
                        sprintf("%s %.2f", labels$label_rer_peak, peaks@rer_peak))
    }
    if (!is.null(peaks@hr_peak) && length(peaks@hr_peak) > 0 &&
        is.finite(peaks@hr_peak)) {
      detail_parts <- c(detail_parts,
                        sprintf("%s %.0f bpm", tr("hr", language), peaks@hr_peak))
    }
  }

  list(
    has_validity = TRUE,
    validity_color = if (confirmed) "ok" else "vt1_col",
    validity_title = sprintf(
      "%s \u2014 %d/%d %s",
      if (confirmed) labels$validity_confirmed else labels$validity_not_confirmed,
      criteria@criteria_met,
      criteria@criteria_available,
      labels$label_criteria_of
    ),
    validity_detail = escape_typst(paste(detail_parts, collapse = " \u00b7 "))
  )
}


#' Build the Analysis-Parameters Block for the Report
#'
#' @description
#' Lists the settings that produced the numbers, as Typst grid cells.
#'
#' @param analysis A [CpetAnalysis] object.
#' @param config A [ReportConfig] object.
#' @param labels The label list for the report language.
#' @param language Language code.
#'
#' @return A named list of template values.
#'
#' @keywords internal
build_analysis_params_block <- function(analysis, config, labels,
                                        language = "en") {
  window_s <- analysis@data@averaging_window %||% 30

  averaging <- if (isTRUE(analysis@data@is_averaged)) {
    sprintf("%s %d s",
            if (identical(language, "fr")) "moyenne mobile" else "rolling average",
            as.integer(round(window_s)))
  } else {
    if (identical(language, "fr")) "aucun" else "none"
  }

  thresholds <- analysis@thresholds
  threshold_desc <- if (is.null(thresholds) || is.null(thresholds@vt1_method)) {
    "\u2014"
  } else if (identical(thresholds@vt1_method, "manual")) {
    tr("threshold_method_manual", language)
  } else {
    thresholds@vt1_method
  }

  predicted_source <- switch(config@prediction_source %||% "jones",
    prefaut = "Pr\u00e9faut",
    "Jones et al. (1997)"
  )

  rows <- list(
    list(labels$param_software,
         paste("cardiometR", utils::packageVersion("cardiometR"))),
    list(labels$param_averaging, averaging),
    list(labels$param_peak_rule,
         sprintf(labels$param_peak_rule_value %||% "%d s",
                 as.integer(round(window_s)))),
    list(labels$param_thresholds, threshold_desc),
    list(labels$param_predicted, predicted_source),
    list(labels$param_exclusion, labels$param_exclusion_value),
    list(labels$param_render, labels$param_render_value)
  )

  # A missing label or value must not collapse the pair, or the whole
  # block loses a row and the grid falls out of step.
  as_cell <- function(x) {
    if (is.null(x) || length(x) == 0 || is.na(x[[1]])) "\u2014" else as.character(x[[1]])
  }
  rows <- lapply(rows, function(row) c(as_cell(row[[1]]), as_cell(row[[2]])))

  cells <- vapply(rows, function(row) {
    sprintf(
      paste0("grid(columns: (42mm, 1fr), inset: (bottom: 1.2mm), ",
             "stroke: (bottom: 0.4pt + rgb(\"#eceff3\")), ",
             "text(size: 8.5pt, fill: muted)[%s], ",
             "text(size: 8.5pt, weight: 600)[%s])"),
      escape_typst(row[[1]]),
      escape_typst(row[[2]])
    )
  }, character(1))

  list(
    has_analysis_params = TRUE,
    analysis_params_content = paste(cells, collapse = ",\n    ")
  )
}

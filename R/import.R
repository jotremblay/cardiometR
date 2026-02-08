# COSMED CPET Data Import Functions

#' Read COSMED Quark CPET Excel Export
#'
#' Import breath-by-breath data from a COSMED Quark CPET xlsx export file.
#'
#' @param file Path to the xlsx file exported from COSMED Omnia software
#' @param sheet Sheet name or index to read (default "Data")
#'
#' @return A CpetData S7 object containing participant info, metadata, and breaths
#'
#' @details
#' The COSMED xlsx format has a specific structure:
#' - Columns 1-2: Participant information (ID, name, age, sex, height, weight, DOB)
#' - Columns 4-5: Test metadata (date, duration, protocol, etc.)
#' - Columns 7-8: Environmental conditions (temperature, pressure, humidity)
#' - Columns 10+: Breath-by-breath data starting from row 4
#'
#' Row 1 contains variable names, row 2 contains units, row 3 is empty,
#' and data starts from row 4.
#'
#' @examples
#' \dontrun{
#' data <- read_cosmed("path/to/cosmed_export.xlsx")
#' print(data)
#' }
#'
#' @export
read_cosmed <- function(file, sheet = "Data") {
  if (!file.exists(file)) {
    cli::cli_abort("File not found: {file}")
  }

  # Read raw data without column names
  raw <- readxl::read_excel(
    file,
    sheet = sheet,
    col_names = FALSE,
    .name_repair = "minimal"
  )

  # Parse participant information
  participant <- parse_cosmed_participant(raw)

  # Parse metadata
  metadata <- parse_cosmed_metadata(raw)

  # Parse breath-by-breath data
  breaths <- parse_cosmed_breaths(raw)

  # Detect if data is breath-by-breath or time-averaged
  bxb_detection <- detect_data_type(breaths)

  # Create and return CpetData object
  CpetData(
    participant = participant,
    metadata = metadata,
    breaths = breaths,
    stages = NULL,
    is_averaged = bxb_detection$is_averaged,
    averaging_window = bxb_detection$averaging_window
  )
}


#' Parse Participant Information from COSMED Export
#'
#' @param raw Raw data frame from readxl
#' @return A Participant S7 object
#' @keywords internal
parse_cosmed_participant <- function(raw) {
  # Extract participant data from columns 1-2, rows 1-8
  get_value <- function(row) {
    val <- as.character(raw[[2]][row])
    if (is.na(val) || val == "") return(NULL)
    val
  }

  # Parse fields
  id <- get_value(1) %||% "Unknown"
  last_name <- get_value(2) %||% ""
  first_name <- get_value(3) %||% ""
  name <- trimws(paste(first_name, last_name))
  if (name == "") name <- "Unknown"

  gender_raw <- tolower(get_value(4) %||% "O")
  sex <- dplyr::case_when(
    grepl("male", gender_raw) & !grepl("female", gender_raw) ~ "M",
    grepl("female", gender_raw) ~ "F",
    grepl("^m$", gender_raw) ~ "M",
    grepl("^f$", gender_raw) ~ "F",
    TRUE ~ "O"
  )

  age <- as.numeric(get_value(5) %||% 30)
  height_cm <- as.numeric(get_value(6) %||% 170)
  weight_kg <- as.numeric(get_value(7) %||% 70)

  # Parse date of birth (row 8)
  dob_raw <- get_value(8)
  date_of_birth <- if (!is.null(dob_raw)) {
    tryCatch(
      as.Date(dob_raw),
      error = function(e) NULL
    )
  } else {
    NULL
  }

  Participant(
    id = id,
    name = name,
    age = age,
    sex = sex,
    height_cm = height_cm,
    weight_kg = weight_kg,
    sport = NULL,
    date_of_birth = date_of_birth
  )
}


#' Parse Metadata from COSMED Export
#'
#' @param raw Raw data frame from readxl
#' @return A CpetMetadata S7 object
#' @keywords internal
parse_cosmed_metadata <- function(raw) {
  # Columns 4-5 contain test metadata
  # Columns 7-8 contain environmental conditions
  get_meta <- function(col, row) {
    val <- as.character(raw[[col]][row])
    if (is.na(val) || val == "" || val == "-") return(NULL)
    val
  }

  # Test date (column 5, row 1)
  test_date_raw <- get_meta(5, 1)
  test_date <- if (!is.null(test_date_raw)) {
    tryCatch(
      as.Date(test_date_raw),
      error = function(e) Sys.Date()
    )
  } else {
    Sys.Date()
  }

  # Protocol (column 5, row 8)
  protocol <- get_meta(5, 8) %||% "Unknown"

  # Ergometer/device (column 5, row 7)
  ergometer <- get_meta(5, 7) %||% "COSMED Quark CPET"
  device <- paste("COSMED Quark CPET -", ergometer)

  # Environmental conditions from columns 7-8
  pressure_mmhg <- as.numeric(get_meta(8, 1))
  temperature_c <- as.numeric(get_meta(8, 2))
  humidity_pct <- as.numeric(get_meta(8, 3))

  CpetMetadata(
    test_date = test_date,
    device = device,
    protocol = protocol,
    calibration_date = NULL,
    temperature_c = temperature_c,
    pressure_mmhg = pressure_mmhg,
    humidity_pct = humidity_pct,
    technician = NULL
  )
}


#' Parse Breath-by-Breath Data from COSMED Export
#'
#' @param raw Raw data frame from readxl
#' @return A tibble of breath-by-breath data
#' @keywords internal
parse_cosmed_breaths <- function(raw) {
  # Get column headers from row 1 (starting at column 10)
  headers <- as.character(raw[1, 10:ncol(raw)])
  units <- as.character(raw[2, 10:ncol(raw)])

  # Data starts at row 4 (index 4)
  data_start <- 4
  data_cols <- 10:ncol(raw)

  # Extract data portion
  breath_data <- raw[data_start:nrow(raw), data_cols]

  # Set column names
  names(breath_data) <- headers

  # Identify text columns that should not be converted to numeric
  text_columns <- c("Phase", "Marker")

  # Convert numeric columns only
  numeric_cols <- setdiff(names(breath_data), text_columns)
  breath_data <- breath_data |>
    dplyr::mutate(dplyr::across(dplyr::all_of(numeric_cols), as.numeric))

  # Map COSMED column names to cardiometR standard names
  column_mapping <- c(
    "t" = "time_s",
    "VO2" = "vo2_ml",
    "VCO2" = "vco2_ml",
    "VE" = "ve_l",
    "RQ" = "rer",
    "HR" = "hr_bpm",
    "Power" = "power_w",
    "Rf" = "bf",
    "VT" = "vt_l",
    "VE/VO2" = "ve_vo2",
    "VE/VCO2" = "ve_vco2",
    "VO2/Kg" = "vo2_kg",
    "FeO2" = "feo2_pct",
    "FeCO2" = "feco2_pct",
    "PeO2" = "peto2_mmhg",
    "PeCO2" = "petco2_mmhg",
    "SpO2" = "spo2_pct",
    "Speed" = "speed_kmh",
    "Revolution" = "rpm",
    "Phase" = "phase",
    "Marker" = "marker",
    "METS" = "mets",
    "VO2/HR" = "vo2_hr"
  )

  # Rename columns that exist
  for (old_name in names(column_mapping)) {
    if (old_name %in% names(breath_data)) {
      new_name <- column_mapping[old_name]
      names(breath_data)[names(breath_data) == old_name] <- new_name
    }
  }

  # Select and order standard columns (keeping only what exists)
  standard_cols <- c(
    "time_s", "vo2_ml", "vco2_ml", "ve_l", "rer",
    "hr_bpm", "power_w", "speed_kmh", "bf", "vt_l",
    "ve_vo2", "ve_vco2", "vo2_kg",
    "peto2_mmhg", "petco2_mmhg", "spo2_pct",
    "phase", "marker", "mets", "vo2_hr", "rpm"
  )

  available_cols <- standard_cols[standard_cols %in% names(breath_data)]
  breaths <- breath_data |>
    dplyr::select(dplyr::any_of(available_cols))

  # Validate required columns
  required_cols <- c("time_s", "vo2_ml", "vco2_ml", "ve_l", "rer")
  missing_required <- setdiff(required_cols, names(breaths))
  if (length(missing_required) > 0) {
    cli::cli_abort("COSMED file missing required columns: {paste(missing_required, collapse = ', ')}")
  }

  # Remove rows with NA in required columns
  breaths <- breaths |>
    dplyr::filter(
      !is.na(.data[["time_s"]]),
      !is.na(.data[["vo2_ml"]]),
      !is.na(.data[["vco2_ml"]]),
      !is.na(.data[["ve_l"]]),
      !is.na(.data[["rer"]])
    )

  # Convert time from Excel day fraction to seconds when needed
  # Excel stores time as fraction of a day (1 = 24 hours = 86400 seconds)
  if ("time_s" %in% names(breaths)) {
    max_time <- suppressWarnings(max(breaths$time_s, na.rm = TRUE))
    if (is.finite(max_time) && max_time <= 2) {
      breaths <- breaths |>
        dplyr::mutate(time_s = time_s * 86400)
    }
  }

  # Ensure required columns have non-missing data
  for (col in required_cols) {
    if (all(is.na(breaths[[col]]))) {
      cli::cli_abort("Required column {col} contains only missing values")
    }
  }

  # Convert phase to character if present
  if ("phase" %in% names(breaths)) {
    breaths$phase <- as.character(breaths$phase)
  }

  tibble::as_tibble(breaths)
}


#' Read CPET Data (Generic Interface)
#'
#' Generic function to read CPET data from various metabolic cart formats.
#' Currently supports COSMED Quark CPET xlsx exports.
#'
#' @param file Path to the CPET data file
#' @param format Optional format specification. If NULL, auto-detected from file.
#'   Options: "cosmed", "cortex", "vyntus"
#' @param ... Additional arguments passed to format-specific readers
#'
#' @return A CpetData S7 object
#'
#' @examples
#' \dontrun{
#' # Auto-detect format
#' data <- read_cpet("path/to/file.xlsx")
#'
#' # Explicit format
#' data <- read_cpet("path/to/file.xlsx", format = "cosmed")
#' }
#'
#' @export
read_cpet <- function(file, format = NULL, ...) {
  if (!file.exists(file)) {
    cli::cli_abort("File not found: {file}")
  }

  # Auto-detect format if not specified
  if (is.null(format)) {
    format <- detect_cpet_format(file)
  }

  # Dispatch to appropriate reader
  switch(tolower(format),
    cosmed = read_cosmed(file, ...),
    cli::cli_abort("Unsupported format: {format}. Currently supported: cosmed")
  )
}


#' Detect CPET File Format
#'
#' Attempts to detect the metabolic cart format from file contents.
#'
#' @param file Path to the CPET data file
#' @return Character string indicating the detected format
#' @keywords internal
detect_cpet_format <- function(file) {
  ext <- tolower(tools::file_ext(file))

  if (ext %in% c("xlsx", "xls")) {
    # Try to detect from content
    tryCatch({
      # Read first few rows
      preview <- readxl::read_excel(
        file,
        n_max = 5,
        col_names = FALSE,
        .name_repair = "minimal"
      )

      # Check for COSMED-specific patterns
      # COSMED has "ID1" in first cell, metadata in specific layout
      first_cell <- as.character(preview[[1]][1])
      if (!is.na(first_cell) && grepl("^ID", first_cell)) {
        return("cosmed")
      }

      # Default to cosmed for xlsx files
      return("cosmed")
    }, error = function(e) {
      cli::cli_warn("Could not auto-detect format, defaulting to cosmed")
      return("cosmed")
    })
  }

  cli::cli_abort("Cannot detect format for file: {file}")
}


#' Detect if CPET data is breath-by-breath or time-averaged
#'
#' @param breaths Data frame with breath-by-breath data
#' @return List with is_averaged (logical) and averaging_window (numeric or NULL)
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

  # Regular intervals (CV < 5%) with window >= 3s suggest time-averaged
  common_windows <- c(5, 10, 15, 20, 30)
  matched_window <- common_windows[which.min(abs(common_windows - median_interval))]

  if (cv < 0.05 && abs(median_interval - matched_window) < 1) {
    list(is_averaged = TRUE, averaging_window = matched_window)
  } else {
    list(is_averaged = FALSE, averaging_window = NULL)
  }
}

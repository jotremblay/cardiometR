# Unit parsing, unit inference, and numeric coercion for imported data.
#
# Pure helpers. Nothing here reads a file or knows about a dialect.
#
# The package's analysis layer has hard-coded unit assumptions throughout: VO2
# and VCO2 in mL/min, VE in L/min, time in seconds, speed in km/h, power in
# watts. Ventilatory equivalents are computed as `ve_l * 1000 / vo2_ml`, so a
# cart that reports VO2 in L/min yields equivalents a thousand times too large
# while the threshold detectors, which look at relative change, still find a
# breakpoint. The result looks plausible and is wrong. Normalising units at
# import is what prevents that.


# ---- unit vocabulary -------------------------------------------------------

# Maps a folded unit string to a canonical token. Built as a named vector
# rather than a switch so it can be inspected and extended.
.unit_aliases <- c(
  # volume per time
  mlmin = "mL/min", mlmin1 = "mL/min", mlpermin = "mL/min", ccmin = "mL/min",
  lmin = "L/min", lmin1 = "L/min", lpermin = "L/min",
  lminbtps = "L/min", lbtpsmin = "L/min",
  mlminkg = "mL/min/kg", mlkgmin = "mL/min/kg", mlminkg1 = "mL/min/kg",
  mlmin1kg1 = "mL/min/kg", mlkgmin1 = "mL/min/kg",

  # volume
  l = "L", lbtps = "L", lbtp = "L", lstpd = "L", litre = "L", liter = "L",
  ml = "mL", mlbtps = "mL",

  # time
  s = "s", sec = "s", secs = "s", second = "s", seconds = "s",
  secondes = "s", sn = "s",
  min = "min", mins = "min", minute = "min", minutes = "min",
  hhmmss = "hh:mm:ss", hmmss = "hh:mm:ss", mmss = "hh:mm:ss",

  # speed
  kmh = "km/h", kmh1 = "km/h", kmhr = "km/h", kmheure = "km/h",
  kmph = "km/h", kph = "km/h",
  ms = "m/s", ms1 = "m/s", msec = "m/s",
  mph = "mph", mih = "mph", milesh = "mph",

  # pressure
  mmhg = "mmHg", torr = "mmHg",
  kpa = "kPa", hpa = "hPa", mbar = "hPa", pa = "Pa", atm = "atm",

  # temperature
  c = "degC", degc = "degC", celsius = "degC",
  f = "degF", degf = "degF", fahrenheit = "degF",

  # rates
  bpm = "bpm", battmin = "bpm", batmin = "bpm", battiti = "bpm",
  min1 = "1/min", `1min` = "1/min",
  rpm = "rpm", trmin = "rpm", tourmin = "rpm", tmin = "rpm",

  # misc
  w = "W", watt = "W", watts = "W",
  mlbeat = "mL/beat", mlbatt = "mL/beat",
  kg = "kg", g = "g", cm = "cm", m = "m", mm = "mm", km = "km",
  pct = "%", percent = "%", pourcent = "%", perc = "%"
)

# Canonical unit each internal column is expected to be in. NA means the column
# is dimensionless and never converted.
.canonical_units <- c(
  time_s = "s",
  vo2_ml = "mL/min",
  vco2_ml = "mL/min",
  ve_l = "L/min",
  rer = NA_character_,
  hr_bpm = "bpm",
  power_w = "W",
  speed_kmh = "km/h",
  vt_l = "L",
  vo2_kg = "mL/min/kg",
  peto2_mmhg = "mmHg",
  petco2_mmhg = "mmHg",
  bf = "1/min",
  spo2_pct = "%",
  feo2_pct = "%",
  feco2_pct = "%",
  rpm = "rpm",
  vo2_hr = "mL/beat",
  mets = NA_character_,
  ve_vo2 = NA_character_,
  ve_vco2 = NA_character_
)

# Multiplicative conversions, keyed "from|to". Anything absent is refused
# rather than guessed.
.unit_factors <- c(
  "L/min|mL/min" = 1000, "mL/min|L/min" = 1 / 1000,
  "L|mL" = 1000, "mL|L" = 1 / 1000,
  "m/s|km/h" = 3.6, "km/h|m/s" = 1 / 3.6,
  "mph|km/h" = 1.609344, "km/h|mph" = 1 / 1.609344,
  "min|s" = 60, "s|min" = 1 / 60,
  "day|s" = 86400, "s|day" = 1 / 86400,
  "kPa|mmHg" = 7.500617, "mmHg|kPa" = 1 / 7.500617,
  "hPa|mmHg" = 0.7500617, "Pa|mmHg" = 0.007500617,
  "atm|mmHg" = 760,
  "bpm|1/min" = 1, "1/min|bpm" = 1,
  "m|cm" = 100, "cm|m" = 1 / 100
)


#' Reduce a unit string to a canonical token
#'
#' @param x Character vector of unit strings, as written in a file.
#'
#' @return Character vector of canonical tokens, `NA` where unrecognised or
#'   where the unit is a dimensionless placeholder such as `"---"`.
#'
#' @keywords internal
parse_unit <- function(x) {
  x <- as.character(x)
  x[is.na(x)] <- ""
  x <- trimws(x)

  # Percent and per-mille survive norm_key only if handled first: the
  # punctuation strip would otherwise erase them entirely.
  out <- rep(NA_character_, length(x))
  out[x %in% c("%", "pct")] <- "%"

  # Middle dot is a multiplication sign in unit notation; a trailing inverse
  # marker becomes a plain 1 so "km.h-1" and "km/h" fold together.
  y <- gsub("\u00b7", "/", x, fixed = TRUE)
  y <- gsub("\u207b\u00b9", "-1", y, fixed = TRUE)
  y <- gsub("\u00b0", "deg", y, fixed = TRUE)

  keys <- norm_key(y)
  hits <- unname(.unit_aliases[keys])
  out[is.na(out)] <- hits[is.na(out)]
  out
}


#' Does this cell look like a unit rather than data?
#'
#' Used to tell a units row apart from a data row. COSMED writes `"---"` for
#' dimensionless columns, so a units row can be mostly placeholders; refusing
#' those would make the row undetectable.
#'
#' @param x Character vector of cell contents.
#'
#' @return Logical vector.
#'
#' @keywords internal
is_unit_token <- function(x) {
  x <- trimws(as.character(x))
  x[is.na(x)] <- ""
  placeholder <- grepl("^-{1,}$", x) | x %in% c("", "%", "n/a", "na")
  !is.na(parse_unit(x)) | placeholder
}


#' Convert values into a column's canonical unit
#'
#' @param values Numeric vector.
#' @param canonical Canonical column name, used to look up the target unit.
#' @param from Canonical token the values are currently in, or `NA`.
#' @param source How `from` was established: `"declared"`, `"inline"`,
#'   `"heuristic"`, `"user"`, or `"assumed"`. Carried through for reporting.
#'
#' @return A list with `values`, `from`, `to`, `factor` and `source`.
#'
#' @keywords internal
convert_unit <- function(values, canonical, from, source = "declared") {
  to <- if (canonical %in% names(.canonical_units)) {
    unname(.canonical_units[[canonical]])
  } else {
    NA_character_
  }

  unchanged <- list(values = values, from = from, to = to,
                    factor = 1, source = source)
  if (is.na(to) || is.na(from) || identical(from, to)) {
    return(unchanged)
  }

  # Temperature is affine, so it cannot be expressed as a factor.
  if (identical(from, "degF") && identical(to, "degC")) {
    return(list(values = (values - 32) * 5 / 9, from = from, to = to,
                factor = NA_real_, source = source))
  }

  key <- paste(from, to, sep = "|")
  if (!key %in% names(.unit_factors)) {
    cli::cli_warn(c(
      "No conversion from {.val {from}} to {.val {to}} for {.field {canonical}}.",
      "i" = "Values were left unchanged."
    ))
    return(list(values = values, from = from, to = to,
                factor = 1, source = "unconverted"))
  }

  factor <- unname(.unit_factors[[key]])
  list(values = values * factor, from = from, to = to,
       factor = factor, source = source)
}


#' Infer a unit from the magnitude of the data
#'
#' The last resort when a file declares no unit, and the tie-breaker for time,
#' where the declared unit cannot be trusted: COSMED labels its time column
#' seconds while storing Excel day fractions.
#'
#' @param values Numeric vector.
#' @param canonical Canonical column name.
#'
#' @return A list with `unit`, `confidence` (`"high"`, `"low"` or `"none"`) and
#'   a `note` explaining the call.
#'
#' @keywords internal
infer_unit_from_magnitude <- function(values, canonical) {
  none <- list(unit = NA_character_, confidence = "none", note = NA_character_)
  finite <- values[is.finite(values)]
  if (length(finite) == 0) {
    return(none)
  }
  mid <- stats::median(abs(finite))
  top <- max(finite)

  switch(canonical,
    time_s = if (top <= 2) {
      list(unit = "day", confidence = "high",
           note = "whole test spans under 2 units, so these are Excel day fractions")
    } else if (top <= 240 && length(finite) > 30) {
      list(unit = "min", confidence = "low",
           note = "max under 240 across many rows, so possibly minutes")
    } else {
      list(unit = "s", confidence = "high", note = NA_character_)
    },
    vo2_ml = ,
    vco2_ml = if (mid < 20) {
      list(unit = "L/min", confidence = "high",
           note = sprintf("median %.2f is far below a mL/min reading", mid))
    } else {
      list(unit = "mL/min", confidence = "high", note = NA_character_)
    },
    ve_l = if (mid > 500) {
      list(unit = "mL/min", confidence = "high",
           note = sprintf("median %.0f is far above an L/min reading", mid))
    } else {
      list(unit = "L/min", confidence = "high", note = NA_character_)
    },
    vt_l = if (mid > 100) {
      list(unit = "mL", confidence = "high", note = NA_character_)
    } else {
      list(unit = "L", confidence = "high", note = NA_character_)
    },
    # 10 km/h and 10 m/s are both real speeds, a jog and a sprint, so this can
    # never be more than a hint.
    speed_kmh = if (top > 0 && top < 12) {
      list(unit = "m/s", confidence = "low",
           note = sprintf("max %.1f is low for km/h, so possibly m/s", top))
    } else {
      list(unit = "km/h", confidence = "low", note = NA_character_)
    },
    none
  )
}


#' Coerce a time column to seconds
#'
#' Handles plain numeric seconds, Excel day fractions, clock notation, and the
#' `difftime` and `POSIXct` values `readxl` can hand back. `as.numeric()` alone
#' returns `NA` for `"00:10:30"`, which silently empties the whole table.
#'
#' @param x A vector of times in any of the supported forms.
#'
#' @return Numeric vector of seconds.
#'
#' @keywords internal
as_seconds <- function(x) {
  if (inherits(x, "difftime")) {
    return(as.numeric(x, units = "secs"))
  }
  if (inherits(x, "POSIXct")) {
    return(as.numeric(x) - as.numeric(x[[1L]]))
  }
  if (is.numeric(x)) {
    return(as.numeric(x))
  }

  text <- trimws(as.character(x))
  text[!nzchar(text) | text %in% c("-", "--", "---", "NA", "N/A", "n/a")] <- NA_character_
  text <- sub(",", ".", text, fixed = TRUE)

  is_clock <- !is.na(text) &
    grepl("^[0-9]{1,3}:[0-9]{1,2}(:[0-9]{1,2}([.][0-9]+)?)?$", text)

  out <- rep(NA_real_, length(text))
  out[!is_clock] <- suppressWarnings(as.numeric(text[!is_clock]))

  if (any(is_clock)) {
    parts <- strsplit(text[is_clock], ":", fixed = TRUE)
    out[is_clock] <- vapply(parts, function(p) {
      p <- as.numeric(p)
      # Two parts is ambiguous. mm:ss is the right default for a CPET
      # stopwatch; hh:mm would make every test 60 times too long.
      if (length(p) == 2L) p[[1L]] * 60 + p[[2L]] else p[[1L]] * 3600 + p[[2L]] * 60 + p[[3L]]
    }, numeric(1))
  }
  out
}


#' Coerce text to numeric, tolerating locale conventions
#'
#' For delimited files, where a French export writes `"1 234,5"`. Spreadsheets
#' store real doubles regardless of how they are displayed, so this is not
#' needed on the xlsx path.
#'
#' @param x A character vector.
#'
#' @return Numeric vector.
#'
#' @keywords internal
as_numeric_loose <- function(x) {
  if (is.numeric(x)) {
    return(as.numeric(x))
  }
  text <- trimws(as.character(x))
  text[!nzchar(text) | grepl("^-{1,}$", text) | text %in% c("NA", "N/A", "n/a")] <- NA_character_
  text <- gsub("[\u2009\u00a0 ]", "", text)   # thin, non-breaking, plain
  # A comma is the decimal mark only when it is the sole comma and no dot is
  # present; otherwise it is a thousands separator.
  decimal_comma <- !grepl(".", text, fixed = TRUE) &
    lengths(regmatches(text, gregexpr(",", text, fixed = TRUE))) == 1L
  text[decimal_comma] <- sub(",", ".", text[decimal_comma], fixed = TRUE)
  text <- gsub(",", "", text, fixed = TRUE)
  suppressWarnings(as.numeric(text))
}

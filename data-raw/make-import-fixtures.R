# Generate the synthetic CPET import fixtures.
#
# Run this by hand when a fixture needs to change:
#
#     source("data-raw/make-import-fixtures.R")
#
# Requires the writexl package, which is NOT a package dependency. The generated
# files are committed to tests/testthat/fixtures/ on purpose, so that running
# the test suite and R CMD check need no spreadsheet writer at all.
#
# Each fixture isolates one thing the importer has to survive. Keep them small
# and keep the comment next to each one accurate; they are the specification.

if (!requireNamespace("writexl", quietly = TRUE)) {
  stop("writexl is needed to regenerate the fixtures: install.packages('writexl')")
}

fixture_dir <- file.path("tests", "testthat", "fixtures")
dir.create(fixture_dir, recursive = TRUE, showWarnings = FALSE)


# ---- physiology ------------------------------------------------------------
# A short, plausible incremental cycle test: rest, warmup, then 4 stages.
# Values are generated once and reused so every fixture describes the same
# test. That way a language or unit difference is the ONLY difference between
# two fixtures, which is what makes a failure interpretable.

make_physiology <- function(n_rest = 12, n_warm = 12, n_stage = 12, n_stages = 4,
                            interval_s = 10) {
  n <- n_rest + n_warm + n_stage * n_stages
  time_s <- seq(interval_s, by = interval_s, length.out = n)

  block <- c(
    rep("rest", n_rest),
    rep("warmup", n_warm),
    rep("exercise", n_stage * n_stages)
  )

  # Work rate: 0 at rest, 50 W warmup, then 100 W stepping by 50 W.
  power_w <- c(
    rep(0, n_rest),
    rep(50, n_warm),
    rep(100 + 50 * (seq_len(n_stages) - 1), each = n_stage)
  )

  # VO2 tracks work rate with a resting offset (roughly the ACSM cycling
  # relation: 7 mL/min/kg of body mass plus 10.8 mL/min per watt).
  weight_kg <- 75
  vo2_ml <- 7 * weight_kg + 10.8 * power_w
  vo2_ml <- vo2_ml * stats::runif(n, 0.98, 1.02)

  # RER climbs through the test and crosses 1.0 late, so threshold detection
  # has something to find.
  rer <- 0.80 + 0.30 * (power_w / max(power_w))
  rer <- pmin(rer * stats::runif(n, 0.99, 1.01), 1.25)

  vco2_ml <- vo2_ml * rer
  ve_l <- vco2_ml / 1000 * 26 + 4          # roughly VE/VCO2 of 26
  hr_bpm <- 70 + 100 * (power_w / max(power_w)) + stats::runif(n, -2, 2)
  speed_ms <- rep(0, n)                     # cycle test: no belt speed

  data.frame(
    time_s   = time_s,
    block    = block,
    power_w  = power_w,
    vo2_ml   = round(vo2_ml, 1),
    vco2_ml  = round(vco2_ml, 1),
    ve_l     = round(ve_l, 2),
    rer      = round(rer, 3),
    hr_bpm   = round(hr_bpm),
    speed_ms = speed_ms,
    stringsAsFactors = FALSE
  )
}

set.seed(20250615)
phys <- make_physiology()


# ---- vocabulary ------------------------------------------------------------

vocab <- list(
  en = list(
    header_labels = c("ID1", "Last Name", "First Name", "Gender", "Age",
                      "Height (cm)", "Weight (kg)", "D.O.B."),
    header_values = c("EN001", "Smith", "John", "Male", "38", "180", "78",
                      "1987-02-03"),
    meta_labels   = c("Test date", "Test Time", "Subject Type", "Test Type",
                      "Ergometer", "Protocol"),
    meta_values   = c("2025-06-15", "09:14:00", "Healthy", "Maximal",
                      "Lode Excalibur", "Cycle ramp 50W/stage"),
    env_labels    = c("Barometric Pressure (mmHg)", "Ambient Temperature (°C)",
                      "Ambient Relative Humidity (%)"),
    env_values    = c("754", "22", "29"),
    data_headers  = c("t", "HR", "Power", "VO2", "VCO2", "VE", "RQ",
                      "Speed", "Phase"),
    data_units    = c("s", "bpm", "Watt", "mL/min", "mL/min", "L/min", "---",
                      "km/h", "---"),
    phase         = c(rest = "REST", warmup = "WARMUP", exercise = "EXERCISE")
  ),
  # Modelled on a real COSMED Omnia French export. Two details are copied from
  # it deliberately, because both would be wrong if guessed:
  #
  #  1. The test metadata sits at DIFFERENT rows than in the English export.
  #     Protocol is row 13 and ergometer row 14, where the English file has
  #     them at rows 8 and 7. Reading fixed cells gives the wrong answer.
  #  2. Omnia translates the column headers and the header-block labels, but
  #     leaves the phase VALUES in its own English vocabulary, and writes NONE
  #     where no phase applies.
  fr = list(
    header_labels = c("ID1", "Nom de famille", "Prénom", "Sexe",
                      "Âge", "Taille (cm)", "Poids (kg)",
                      "Date de Naissance"),
    header_values = c("FR001", "Tremblay", "Marie", "Femme", "42", "165", "58",
                      "11/04/1983"),
    meta_labels   = c("Date du test", "Heure du test", "Type de sujet",
                      "Réponse ECG", "Motif du test",
                      "Motif d'arrêt du test", "Motif du test",
                      "Type de test", "Effort Maximal", "Durée du test",
                      "Durée de l'exercice", "Dispositif FC",
                      "Protocole", "Ergomètre"),
    meta_values   = c("15/06/2025", "09:14:00", "Sain", "Aucun", "Aucun",
                      "Aucun", "Aucun", "Maximal", "Non confirmé",
                      "0.0289", "0.0103", "PC ECG",
                      "Rampe vélo 50W/palier", "Excalib.Sport(Dev#1)"),
    env_labels    = c("Pression Barométrique (mmHg)",
                      "Température Ambiante (°C)",
                      "Humidité Relative Ambiante (%)"),
    env_values    = c("754", "22", "29"),
    data_headers  = c("t", "FC", "Puissance", "V'O2", "V'CO2", "VE", "QR",
                      "Marqueur Vitesse", "Phase"),
    # VO2 and VCO2 in L/min and speed in m/s, so the unit conversion path is
    # exercised rather than assumed.
    data_units    = c("s", "bpm", "W", "L/min", "L/min", "L/min", "---",
                      "m/s", "---"),
    phase         = c(rest = "REST", warmup = "WARMUP",
                      exercise = "EXERCISE")
  ),
  # The same French export, but with the phase values translated too. Some
  # carts and some Omnia versions do this, and it must not stop working just
  # because the file in hand happens not to.
  fr_phases = list(
    header_labels = c("ID1", "Nom de famille", "Prénom", "Sexe",
                      "Âge", "Taille (cm)", "Poids (kg)",
                      "Date de Naissance"),
    header_values = c("FR002", "Gagnon", "Luc", "Homme", "35", "178", "74",
                      "22/09/1990"),
    meta_labels   = c("Date du test", "Protocole", "Ergomètre"),
    meta_values   = c("15/06/2025", "Tapis roulant incrémental",
                      "Woodway PPS"),
    env_labels    = c("Pression Barométrique (mmHg)",
                      "Température Ambiante (°C)",
                      "Humidité Relative Ambiante (%)"),
    env_values    = c("754", "22", "29"),
    data_headers  = c("Temps", "F Resp", "Puissance", "VO2", "VCO2",
                      "Ventilation", "QR", "Vitesse", "Phase"),
    data_units    = c("s", "1/min", "W", "mL/min", "mL/min", "L/min", "---",
                      "km/h", "---"),
    phase         = c(rest = "REPOS", warmup = "ÉCHAUFFEMENT",
                      exercise = "EXERCICE")
  )
)


# ---- grid assembly ---------------------------------------------------------
# COSMED lays the header block out as label/value pairs in columns A/B, D/E and
# G/H, with the breath data starting at column J. Everything is written as text
# because that is how the importer reads it back (col_names = FALSE).

new_grid <- function(n_row, n_col) {
  m <- matrix(NA_character_, nrow = n_row, ncol = n_col)
  colnames(m) <- paste0("X", seq_len(n_col))
  m
}

put_pairs <- function(grid, label_col, labels, values) {
  for (i in seq_along(labels)) {
    grid[i, label_col] <- labels[[i]]
    grid[i, label_col + 1L] <- values[[i]]
  }
  grid
}

#' Assemble one fixture grid.
#'
#' @param lang A name in `vocab`: "en", "fr", or "fr_phases".
#' @param units_row Write the units row? FALSE forces the magnitude heuristic.
#' @param header_block Write the participant/metadata block? FALSE produces a
#'   bare data sheet whose header is row 1.
#' @param time_mode "seconds", "day_fraction" (Excel time storage), or "clock"
#'   (hh:mm:ss text).
#' @param drop Canonical names to leave out of the data block.
#' @param duplicate Canonical name to write twice under two different headers.
build_grid <- function(lang = "en",
                       units_row = TRUE,
                       header_block = TRUE,
                       time_mode = "seconds",
                       drop = character(),
                       duplicate = NULL) {
  v <- vocab[[lang]]
  keep <- !(c("time_s", "hr_bpm", "power_w", "vo2_ml", "vco2_ml", "ve_l",
              "rer", "speed_ms", "block") %in% drop)
  headers <- v$data_headers[keep]
  units <- v$data_units[keep]

  time_col <- switch(time_mode,
    seconds      = as.character(phys$time_s),
    day_fraction = format(phys$time_s / 86400, scientific = FALSE, digits = 15),
    clock        = sprintf("%02d:%02d:%02d",
                           phys$time_s %/% 3600,
                           (phys$time_s %% 3600) %/% 60,
                           phys$time_s %% 60),
    stop("unknown time_mode: ", time_mode)
  )

  # Values follow whatever the units row declares, so a fixture can never
  # claim one unit and hold another by accident.
  unit_of <- function(name) v$data_units[[match(name, c(
    "time_s", "bf", "power_w", "vo2_ml", "vco2_ml", "ve_l", "rer",
    "speed_kmh", "phase"
  ))]]
  vo2_scale <- if (identical(unit_of("vo2_ml"), "L/min")) 1 / 1000 else 1
  speed <- if (identical(unit_of("speed_kmh"), "m/s")) {
    phys$speed_ms
  } else {
    phys$speed_ms * 3.6
  }

  phase_values <- unname(v$phase[phys$block])
  # Omnia writes NONE on the odd row where no phase applies. It must not be
  # reported as an unrecognised label, because there is nothing to fix.
  if (identical(lang, "fr")) {
    phase_values[[1L]] <- "NONE"
  }

  cols <- list(
    time_s   = time_col,
    hr_bpm   = as.character(phys$hr_bpm),
    power_w  = as.character(phys$power_w),
    vo2_ml   = as.character(round(phys$vo2_ml * vo2_scale, 4)),
    vco2_ml  = as.character(round(phys$vco2_ml * vo2_scale, 4)),
    ve_l     = as.character(phys$ve_l),
    rer      = as.character(phys$rer),
    speed_ms = as.character(speed),
    block    = phase_values
  )
  cols <- cols[keep]

  if (!is.null(duplicate)) {
    headers <- c(headers, "Ventilation")
    units <- c(units, "L/min")
    cols <- c(cols, list(dup = as.character(phys$ve_l)))
  }

  first_data_col <- if (header_block) 10L else 1L
  header_row <- if (header_block) 1L else 1L
  units_row_i <- if (units_row) header_row + 1L else NA_integer_
  # COSMED leaves a blank row between the units row and the data.
  data_row <- if (units_row) header_row + 3L else header_row + 1L

  n_row <- data_row + nrow(phys) - 1L
  n_col <- first_data_col + length(headers) - 1L
  grid <- new_grid(max(n_row, 12L), n_col)

  if (header_block) {
    grid <- put_pairs(grid, 1L, v$header_labels, v$header_values)
    grid <- put_pairs(grid, 4L, v$meta_labels, v$meta_values)
    grid <- put_pairs(grid, 7L, v$env_labels, v$env_values)
  }

  data_cols <- seq(first_data_col, length.out = length(headers))
  grid[header_row, data_cols] <- headers
  if (!is.na(units_row_i)) grid[units_row_i, data_cols] <- units
  for (j in seq_along(cols)) {
    grid[seq(data_row, length.out = nrow(phys)), data_cols[j]] <- cols[[j]]
  }
  grid
}

write_fixture <- function(grid, name, sheet = "Data") {
  df <- as.data.frame(grid, stringsAsFactors = FALSE)
  names(df) <- paste0("X", seq_len(ncol(df)))
  sheets <- list(df)
  names(sheets) <- sheet
  path <- file.path(fixture_dir, name)
  writexl::write_xlsx(sheets, path, col_names = FALSE)
  message("wrote ", path)
  invisible(path)
}


# ---- the fixtures ----------------------------------------------------------

# Baseline English. Should behave like the bundled example file.
write_fixture(build_grid("en"), "cosmed_en_synth.xlsx")

# French headers, French phase values, French sex, VO2 in L/min, speed in m/s,
# dd/mm/yyyy dates, and the sheet named in French.
write_fixture(build_grid("fr"), "cosmed_fr_synth.xlsx", sheet = "Données")

# Same French file with the units row removed. Only the magnitude heuristic can
# work out that VO2 is in L/min.
write_fixture(build_grid("fr", units_row = FALSE), "cosmed_fr_nounits.xlsx",
              sheet = "Données")

# A French export whose phase VALUES are translated as well, on a treadmill
# protocol, with speed in km/h.
write_fixture(build_grid("fr_phases"), "cosmed_fr_phases.xlsx",
              sheet = "Données")

# The trap the real COSMED file sets: the units row claims seconds while the
# values are Excel day fractions.
write_fixture(build_grid("en", time_mode = "day_fraction"),
              "cosmed_en_daytime.xlsx")

# Time written as hh:mm:ss text. as.numeric() returns NA for these, which drops
# every row.
write_fixture(build_grid("en", time_mode = "clock"), "cosmed_en_clocktime.xlsx")

# No header block, no units row, header on row 1, data from row 2.
write_fixture(build_grid("en", units_row = FALSE, header_block = FALSE),
              "cosmed_minimal.xlsx")

# Two columns both claiming ve_l.
write_fixture(build_grid("en", duplicate = "ve_l"), "cosmed_conflict.xlsx")

# Missing a required column: import must abort with a clear message.
write_fixture(build_grid("en", drop = "vco2_ml"), "cosmed_missing.xlsx")

# Nothing recognisable at all.
junk <- new_grid(8, 4)
junk[] <- "lorem"
write_fixture(junk, "junk.xlsx")


# ---- delimited fixture -----------------------------------------------------
# French CSV conventions: semicolon separator, comma decimal mark, no header
# block. Written with base R so no extra dependency is involved.

write_csv_fixture <- function() {
  v <- vocab$fr
  num <- function(x) sub(".", ",", format(x, trim = TRUE, scientific = FALSE),
                         fixed = TRUE)
  df <- data.frame(
    Temps     = phys$time_s,
    FC        = phys$hr_bpm,
    Puissance = phys$power_w,
    `V'O2`    = num(round(phys$vo2_ml / 1000, 4)),
    `V'CO2`   = num(round(phys$vco2_ml / 1000, 4)),
    VE        = num(phys$ve_l),
    QR        = num(phys$rer),
    Phase     = unname(v$phase[phys$block]),
    check.names = FALSE,
    stringsAsFactors = FALSE
  )
  path <- file.path(fixture_dir, "generic_fr.csv")
  con <- file(path, open = "w", encoding = "UTF-8")
  on.exit(close(con))
  writeLines(paste(names(df), collapse = ";"), con)
  writeLines(apply(df, 1, paste, collapse = ";"), con)
  message("wrote ", path)
}
write_csv_fixture()

message("\nAll fixtures regenerated. Commit tests/testthat/fixtures/.")

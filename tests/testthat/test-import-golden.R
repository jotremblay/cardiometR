# Golden regression guard for the import layer.
#
# These tests pin what read_cosmed() produces from the bundled English COSMED
# export. They exist so the import rewrite (dialect files, content-based header
# discovery, unit conversion, value vocabularies) can be verified to change
# nothing it was not meant to change.
#
# Two groups:
#   1. Stable facts. These must hold before and after the rewrite.
#   2. Known defects. These assert the CURRENT wrong behaviour on purpose, so
#      that fixing them fails loudly here and forces a deliberate update.

example_cosmed_file <- function() {
  system.file("extdata", "example_cosmed.xlsx", package = "cardiometR")
}


# ---- 1. Stable facts -------------------------------------------------------

test_that("participant is parsed from the header block", {
  f <- example_cosmed_file()
  skip_if(f == "", "Example COSMED file not found")
  p <- read_cosmed(f)@participant

  expect_identical(p@id, "999001")
  expect_identical(p@name, "Jean Dupont")
  expect_identical(p@sex, "M")
  expect_equal(p@age, 64)
  expect_equal(p@height_cm, 182)
  expect_equal(p@weight_kg, 85.5)
})

test_that("test metadata is parsed from the header block", {
  f <- example_cosmed_file()
  skip_if(f == "", "Example COSMED file not found")
  m <- read_cosmed(f)@metadata

  expect_identical(m@protocol, "_Lode_INC-RE_")
  expect_match(m@device, "Excalib", fixed = TRUE)
  expect_equal(m@pressure_mmhg, 754)
  expect_equal(m@temperature_c, 22)
  expect_equal(m@humidity_pct, 29)
})

test_that("the breath table has the expected shape and values", {
  f <- example_cosmed_file()
  skip_if(f == "", "Example COSMED file not found")
  d <- read_cosmed(f)
  b <- d@breaths

  expect_equal(nrow(b), 126L)
  expect_true(all(c("time_s", "vo2_ml", "vco2_ml", "ve_l", "rer") %in% names(b)))
  expect_equal(range(b$time_s), c(30, 1281))
  expect_true(all(diff(b$time_s) > 0))

  expect_equal(b$vo2_ml[1], 713.5136, tolerance = 1e-4)
  expect_equal(b$vco2_ml[1], 606.7844, tolerance = 1e-4)
  expect_equal(b$ve_l[1], 28.6)
  expect_equal(b$rer[1], 0.85)
  expect_equal(b$hr_bpm[1], 97)
  expect_equal(max(b$power_w), 340)

  expect_true(d@is_averaged)
  expect_equal(d@averaging_window, 10)
})

test_that("phase values are recognised by the downstream matchers", {
  f <- example_cosmed_file()
  skip_if(f == "", "Example COSMED file not found")
  phases <- unique(read_cosmed(f)@breaths$phase)

  # Downstream code always folds case before matching, so the comparison that
  # matters is the lowercase one. This is the contract the value vocabulary in
  # the rewrite has to keep satisfying.
  expect_setequal(tolower(phases), c("rest", "warmup", "exercise"))
})


# ---- 2. Derived analysis outputs -------------------------------------------
# A mapping regression can survive the column checks above and still corrupt
# the results. These pin the analysis layer's view of the same file.

test_that("peak values are unchanged", {
  f <- example_cosmed_file()
  skip_if(f == "", "Example COSMED file not found")
  pk <- find_peaks(read_cosmed(f))

  expect_equal(pk@vo2_peak, 5053.720, tolerance = 1e-3)
  expect_equal(pk@vo2_kg_peak, 59.10784, tolerance = 1e-5)
  expect_equal(pk@ve_peak, 181.1667, tolerance = 1e-4)
  expect_equal(pk@rer_peak, 0.9633333, tolerance = 1e-6)
  expect_equal(pk@hr_peak, 166.3333, tolerance = 1e-4)
  expect_equal(pk@power_peak, 340)
  expect_equal(pk@averaging_s, 30)

  # This file is a cycle test. Its speed column is absent today, and after the
  # "mark Speed" alias is added it will be all zeros and therefore dropped.
  # Either way this must stay NULL, or the app flips to treadmill mode.
  expect_null(pk@speed_peak)
})

test_that("protocol configuration is unchanged", {
  f <- example_cosmed_file()
  skip_if(f == "", "Example COSMED file not found")
  cfg <- detect_protocol_config(read_cosmed(f))

  expect_identical(cfg@modality, "cycling")
  expect_equal(cfg@starting_intensity, 130)
  expect_equal(cfg@increment_size, 45)
  expect_equal(cfg@stage_duration_s, 300)
})

test_that("stage assignment is unchanged", {
  f <- example_cosmed_file()
  skip_if(f == "", "Example COSMED file not found")
  b <- extract_stages(read_cosmed(f))@breaths

  expect_true(all(c("stage", "stage_name", "power_rounded") %in% names(b)))
  expect_equal(
    as.vector(table(b$stage)),
    c(39L, 30L, 30L, 12L, 12L, 3L)
  )
  expect_identical(names(table(b$stage)), c("0", "2", "3", "4", "5", "6"))
  expect_identical(unique(b$stage_name[b$stage == 0]), "Rest")
})


# ---- 3. Known defects, asserted on purpose ---------------------------------
# Each of these pins behaviour that is WRONG today. Fixing it must break the
# test here first, so the fix is deliberate and gets recorded.

test_that("FIXED: the real test date is read, not today's date", {
  f <- example_cosmed_file()
  skip_if(f == "", "Example COSMED file not found")

  # The true test date is 2025-06-15, in cell E1 as an Excel serial. The old
  # importer called as.Date() on the serial-as-text, which errors, and a
  # tryCatch quietly substituted Sys.Date(). Every imported file therefore
  # reported the day it was opened rather than the day of the test.
  expect_identical(read_cosmed(f)@metadata@test_date, as.Date("2025-06-15"))
})

test_that("FIXED: the date of birth is read", {
  f <- example_cosmed_file()
  skip_if(f == "", "Example COSMED file not found")

  # In cell B8, also as an Excel serial.
  expect_identical(read_cosmed(f)@participant@date_of_birth,
                   as.Date("1960-03-22"))
})

test_that("FIXED: speed is recognised, then dropped because it is all zeros", {
  f <- example_cosmed_file()
  skip_if(f == "", "Example COSMED file not found")

  # The old mapping looked for a column called "Speed". No COSMED export has
  # one; the real column is "mark Speed", in m/s, which is why treadmill
  # economy never had a data source. It is now recognised and converted to
  # km/h. On this cycle test the values are all zero, so the column is then
  # dropped rather than being allowed to masquerade as treadmill data.
  d <- read_cosmed(f)
  expect_false("speed_kmh" %in% names(d@breaths))
  expect_null(find_peaks(d)@speed_peak)
  expect_identical(detect_protocol_config(d)@modality, "cycling")
})

test_that("FIXED: the test modality is read from the protocol text", {
  f <- example_cosmed_file()
  skip_if(f == "", "Example COSMED file not found")

  # "_Lode_INC-RE_" names a Lode cycle ergometer. Recording the modality at
  # import means a protocol named in French is understood too, rather than
  # falling through to whatever the data happens to suggest.
  expect_identical(read_cosmed(f)@metadata@modality, "cycling")
})

test_that("FIXED: expired gas fractions survive import", {
  f <- example_cosmed_file()
  skip_if(f == "", "Example COSMED file not found")
  b <- read_cosmed(f)@breaths

  # FeO2 and FeCO2 were renamed by the column mapping and then dropped, because
  # the standard_cols whitelist right below it omitted them.
  expect_true(all(c("feo2_pct", "feco2_pct") %in% names(b)))

  # Mixed expired fractions, as percentages.
  expect_true(all(b$feo2_pct > 13 & b$feo2_pct < 21, na.rm = TRUE))
  expect_true(all(b$feco2_pct > 1 & b$feco2_pct < 8, na.rm = TRUE))
})

test_that("optional channels that hold no data are dropped", {
  skip_if(!file.exists(test_path("fixtures", "cosmed_en_synth.xlsx")),
          "Fixtures not generated")

  # The synthetic English fixture is a cycle test whose Speed column is all
  # zeros, the same shape as a real COSMED cycle export. It must not reach the
  # analysis layer, or the app reads the test as a treadmill run.
  b <- read_cosmed(test_path("fixtures", "cosmed_en_synth.xlsx"))@breaths
  expect_false("speed_kmh" %in% names(b))

  # Required columns are never dropped, whatever they hold.
  expect_true(all(c("time_s", "vo2_ml", "vco2_ml", "ve_l", "rer") %in% names(b)))
})


# ---- the import report -----------------------------------------------------

test_that("an import report is attached and describes what happened", {
  f <- example_cosmed_file()
  skip_if(f == "", "Example COSMED file not found")
  report <- cpet_import_report(read_cpet(f, quiet = TRUE))

  expect_s7_class(report, CpetImportReport)
  expect_identical(report@dialect, "cosmed")
  expect_identical(report@sheet, "Data")
  expect_equal(report@layout$header_row, 1L)
  expect_equal(report@layout$data_row, 4L)

  # Every column in the file is accounted for, one way or another.
  expect_setequal(unique(report@columns$status),
                  c("mapped", "ignored"))
  expect_length(report@unknown, 0)

  # The units it converted are recorded with the factor it used.
  time_row <- report@columns[report@columns$canonical == "time_s" &
                               !is.na(report@columns$canonical), ]
  expect_equal(time_row$factor, 86400)
  expect_identical(time_row$unit_source, "heuristic")

  # And where each participant detail came from.
  expect_true(all(c("age", "weight_kg", "test_date") %in% report@metadata$field))
})

test_that("the report survives averaging and stage extraction", {
  f <- example_cosmed_file()
  skip_if(f == "", "Example COSMED file not found")
  data <- read_cpet(f, quiet = TRUE)

  # Both rebuild the CpetData, and both must carry the report across or it
  # vanishes the moment any analysis runs.
  expect_false(is.null(cpet_import_report(extract_stages(data))))
  expect_false(is.null(cpet_import_report(
    suppressWarnings(average(data, method = "time", window = 30))
  )))
})

test_that("an object built by hand has no import report", {
  f <- example_cosmed_file()
  skip_if(f == "", "Example COSMED file not found")
  data <- read_cpet(f, quiet = TRUE)

  # The property is declared NULL-first in its union so that omitting it
  # yields NULL rather than an empty prototype report.
  by_hand <- CpetData(
    participant = data@participant,
    metadata = data@metadata,
    breaths = data@breaths,
    is_averaged = FALSE
  )
  expect_null(cpet_import_report(by_hand))
})

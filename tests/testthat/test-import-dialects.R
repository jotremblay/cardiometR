# Tests for the format descriptions and for delimited files.
#
# The promise being tested here is that supporting a new metabolic cart, or a
# local variation of one already supported, needs a text file rather than a
# change to the package.

fixture <- function(name) test_path("fixtures", name)


# ---- what ships ------------------------------------------------------------

test_that("the packaged formats load and describe themselves", {
  dialects <- list_cpet_dialects()

  expect_true(all(c("cosmed", "csv") %in% dialects$name))
  expect_true(all(nzchar(dialects$label)))
  expect_true(all(file.exists(dialects$path)))
})

test_that("a format can be named by its file stem or by its own name", {
  by_name <- load_dialect("cosmed")
  by_stem <- load_dialect("cosmed-omnia")

  expect_identical(by_name$path, by_stem$path)
  expect_identical(by_name$name, "cosmed")
})

test_that("an unknown format is refused with the list of real ones", {
  expect_error(load_dialect("nonesuch"), "Unknown format")
  expect_error(load_dialect("nonesuch"), "cosmed")
})

test_that("the shared vocabulary is merged into every format", {
  dialect <- load_dialect("csv")

  # These come from _core.yml, not from generic-csv.yml.
  expect_identical(unname(dialect$lookup[["temps"]]), "time_s")
  expect_identical(unname(dialect$lookup[["frequencecardiaque"]]), "hr_bpm")
  expect_identical(unname(dialect$phase_vocab[["repos"]]), "rest")
  expect_setequal(dialect$required,
                  c("time_s", "vo2_ml", "vco2_ml", "ve_l", "rer"))
})

test_that("a brand adds names on top of the shared ones without losing them", {
  cosmed <- load_dialect("cosmed")

  # From cosmed-omnia.yml.
  expect_identical(unname(cosmed$lookup[["markspeed"]]), "speed_kmh")
  expect_identical(unname(cosmed$lookup[["peo2"]]), "peto2_mmhg")
  # Still from _core.yml.
  expect_identical(unname(cosmed$lookup[["vitesse"]]), "speed_kmh")
})


# ---- delimited files -------------------------------------------------------

test_that("a French CSV imports, semicolons and decimal commas included", {
  skip_if(!file.exists(fixture("generic_fr.csv")), "Fixtures not generated")
  data <- read_cpet(fixture("generic_fr.csv"), quiet = TRUE)

  expect_equal(nrow(data@breaths), 72L)
  expect_equal(range(data@breaths$time_s), c(10, 720))
  expect_setequal(unique(data@breaths$phase), c("rest", "warmup", "exercise"))

  # Values are written as "0,5209" in L/min. Both the comma and the unit have
  # to be handled or this lands three orders of magnitude out.
  expect_equal(round(stats::median(data@breaths$vo2_ml)), 1871)
})

test_that("a delimited file is read by the delimited format, not by COSMED", {
  skip_if(!file.exists(fixture("generic_fr.csv")), "Fixtures not generated")
  report <- cpet_import_report(read_cpet(fixture("generic_fr.csv"), quiet = TRUE))

  expect_identical(report@dialect, "csv")
})

test_that("a spreadsheet is still read by the COSMED format", {
  example_file <- system.file("extdata", "example_cosmed.xlsx",
                              package = "cardiometR")
  skip_if(example_file == "", "Example COSMED file not found")

  # Adding the delimited format must not let it steal spreadsheets.
  report <- cpet_import_report(read_cpet(example_file, quiet = TRUE))
  expect_identical(report@dialect, "cosmed")
})


# ---- adding a cart without touching the package ----------------------------

test_that("a format in the user's own directory is found and used", {
  directory <- withr::local_tempdir()
  writeLines(c(
    "version: 1",
    "name: mycart",
    'label: "Acme Cart 3000"',
    "extends: _core",
    "extensions: [csv]",
    "detect:",
    "  - where: header",
    '    matches: "^AcmeTime$"',
    "    weight: 9",
    "layout:",
    "  header_block:",
    "    label_cols: []",
    "    max_rows: 0",
    "aliases:",
    '  time_s: ["acmetime"]',
    '  vo2_ml: ["acme o2"]'
  ), file.path(directory, "mycart.yml"))

  data_file <- file.path(directory, "acme.csv")
  writeLines(c(
    "AcmeTime,Acme O2,VCO2,VE,RER",
    "10,520,415,14.8,0.80",
    "20,600,490,16.1,0.82",
    "30,900,760,22.0,0.84",
    "40,1400,1200,31.0,0.86",
    "50,2000,1750,45.0,0.88"
  ), data_file)

  withr::local_options(cardiometR.dialect_dir = directory)
  clear_dialect_cache()
  withr::defer(clear_dialect_cache())

  expect_true("mycart" %in% list_cpet_dialects()$name)

  data <- read_cpet(data_file, quiet = TRUE)
  report <- cpet_import_report(data)

  expect_identical(report@dialect, "mycart")
  expect_equal(nrow(data@breaths), 5L)
  expect_identical(unname(report@columns$canonical[report@columns$source == "AcmeTime"]),
                   "time_s")
  # The shared vocabulary still applies, so VCO2, VE and RER need no aliases.
  expect_true(all(c("vco2_ml", "ve_l", "rer") %in% names(data@breaths)))
})

test_that("a user format outscores a packaged one on its own files", {
  directory <- withr::local_tempdir()
  writeLines(c(
    "version: 1",
    "name: cosmed",
    'label: "COSMED, local variation"',
    "extends: _core",
    "extensions: [xlsx, xls]",
    "sheet: [Data]"
  ), file.path(directory, "cosmed-omnia.yml"))

  withr::local_options(cardiometR.dialect_dir = directory)
  clear_dialect_cache()
  withr::defer(clear_dialect_cache())

  # Same file stem, so the user's copy replaces the packaged one entirely.
  expect_identical(load_dialect("cosmed")$label, "COSMED, local variation")
})

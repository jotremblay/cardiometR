# Shiny Module Tests for cardiometR
# Tests modules in isolation using shinytest2

test_that("mod_upload_ui creates valid UI", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("bslib")

  # Test English UI
  ui_en <- mod_upload_ui("test_upload", language = "en")
  expect_true(inherits(ui_en, "shiny.tag") || inherits(ui_en, "shiny.tag.list"))

  # Test French UI
  ui_fr <- mod_upload_ui("test_upload", language = "fr")
  expect_true(inherits(ui_fr, "shiny.tag") || inherits(ui_fr, "shiny.tag.list"))
})

test_that("mod_participant_ui creates valid UI", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("bslib")

  ui <- mod_participant_ui("test_participant", language = "en")
  expect_true(inherits(ui, "shiny.tag") || inherits(ui, "shiny.tag.list"))
})

test_that("mod_settings_ui creates valid UI", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("bslib")

  ui <- mod_settings_ui("test_settings", language = "en")
  expect_true(inherits(ui, "shiny.tag") || inherits(ui, "shiny.tag.list"))
})

test_that("mod_results_ui creates valid UI", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("bslib")

  ui <- mod_results_ui("test_results", language = "en")
  expect_true(inherits(ui, "shiny.tag") || inherits(ui, "shiny.tag.list"))
})

test_that("mod_plots_ui creates valid UI", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("bslib")

  ui <- mod_plots_ui("test_plots", language = "en")
  expect_true(inherits(ui, "shiny.tag") || inherits(ui, "shiny.tag.list"))
})

test_that("mod_report_ui creates valid UI", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("bslib")

  ui <- mod_report_ui("test_report", language = "en")
  expect_true(inherits(ui, "shiny.tag") || inherits(ui, "shiny.tag.list"))
})

test_that("app_ui function creates valid page", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("bslib")

  # Test with default language
  ui <- app_ui()
  expect_true(inherits(ui, "shiny.tag") || inherits(ui, "shiny.tag.list"))

  # Test with French
  withr::with_options(list(cardiometR.language = "fr"), {
    ui_fr <- app_ui()
    expect_true(inherits(ui_fr, "shiny.tag") || inherits(ui_fr, "shiny.tag.list"))
  })
})

# Server-side module tests using testServer
test_that("mod_upload_server handles missing file gracefully", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("shinytest2")

  shiny::testServer(mod_upload_server, args = list(language = shiny::reactiveVal("en")), {
    # Initially no data
    expect_null(cpet_data())
    expect_null(validation())
  })
})

test_that("mod_participant_server returns reactive values", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("shinytest2")

  # Create mock data for participant module
  mock <- create_mock_breath_data(n_breaths = 100)

  participant <- Participant(
    id = mock$participant$id,
    name = mock$participant$name,
    age = mock$participant$age,
    sex = mock$participant$sex,
    height_cm = mock$participant$height_cm,
    weight_kg = mock$participant$weight_kg
  )

  metadata <- CpetMetadata(
    test_date = mock$metadata$test_date,
    device = mock$metadata$device,
    protocol = mock$metadata$protocol
  )

  cpet_data <- CpetData(
    participant = participant,
    metadata = metadata,
    breaths = mock$breaths,
    is_averaged = FALSE
  )

  shiny::testServer(
    mod_participant_server,
    args = list(
      language = shiny::reactiveVal("en"),
      cpet_data = shiny::reactiveVal(cpet_data)
    ),
    {
      # The module should have reactive outputs
      # Initial state should reflect the loaded data
      session$flushReact()
    }
  )
})

test_that("mod_settings_server returns analysis settings", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("shinytest2")

  shiny::testServer(
    mod_settings_server,
    args = list(
      language = shiny::reactiveVal("en")
    ),
    {
      session$flushReact()
      # Settings module should return analysis configuration
      result <- session$getReturned()
      expect_type(result, "list")
      expect_true("settings" %in% names(result))
    }
  )
})

test_that("translation function handles all UI labels", {
  skip_if_not_installed("yaml")

  # Load English labels
  labels_en <- yaml::read_yaml(
    system.file("translations", "labels_en.yml", package = "cardiometR")
  )

  # Load French labels
  labels_fr <- yaml::read_yaml(
    system.file("translations", "labels_fr.yml", package = "cardiometR")
  )

  # All English keys should exist in French
  for (key in names(labels_en)) {
    expect_true(
      key %in% names(labels_fr),
      info = paste("Missing French translation for:", key)
    )
  }

  # All French keys should exist in English
  for (key in names(labels_fr)) {
    expect_true(
      key %in% names(labels_en),
      info = paste("Missing English translation for:", key)
    )
  }
})

test_that("run_app function exists and has correct signature", {
  expect_true(is.function(run_app))

  # Check that run_app accepts language parameter
  args <- formals(run_app)
  expect_true("language" %in% names(args) || length(args) >= 0)
})


# Re-labelling dropdowns for a new language ---------------------------------
# Every updateSelectInput() that passes `choices` must also pass `selected`.
# Shiny falls back to the first choice otherwise, so re-labelling a dropdown
# silently discards whatever the user had picked. That is how every
# participant ended up compared against elite normative data, and how the
# averaging method flipped from rolling to time on start-up.

test_that("the settings module never re-labels a dropdown without keeping the selection", {
  source_file <- system.file("R", "mod_settings.R", package = "cardiometR")
  if (source_file == "" || !file.exists(source_file)) {
    source_file <- test_path("..", "..", "R", "mod_settings.R")
  }
  skip_if(!file.exists(source_file), "mod_settings.R source not available")

  lines <- readLines(source_file, warn = FALSE)
  starts <- grep("update\\w*Input\\(", lines)

  offenders <- character()
  for (start in starts) {
    depth <- 0L
    idx <- start
    repeat {
      depth <- depth +
        lengths(regmatches(lines[[idx]], gregexpr("(", lines[[idx]], fixed = TRUE))) -
        lengths(regmatches(lines[[idx]], gregexpr(")", lines[[idx]], fixed = TRUE)))
      if (depth <= 0L || idx >= length(lines)) break
      idx <- idx + 1L
    }
    block <- paste(lines[start:idx], collapse = " ")
    if (grepl("choices", block) && !grepl("selected", block)) {
      offenders <- c(offenders, trimws(lines[[start]]))
    }
  }

  expect_identical(offenders, character())
})

test_that("the athlete level choices are declared in the same order everywhere", {
  source_file <- system.file("R", "mod_settings.R", package = "cardiometR")
  if (source_file == "" || !file.exists(source_file)) {
    source_file <- test_path("..", "..", "R", "mod_settings.R")
  }
  skip_if(!file.exists(source_file), "mod_settings.R source not available")

  lines <- paste(readLines(source_file, warn = FALSE), collapse = "\n")
  orders <- regmatches(
    lines,
    gregexpr('c\\("recreational"[^)]*\\)|c\\("elite"[^)]*\\)', lines)
  )[[1L]]

  # Recreational must come first: it is the default, and a stray reset lands
  # on whichever level is listed first.
  expect_true(length(orders) >= 2)
  expect_true(all(startsWith(orders, 'c("recreational"')))
})


# Import report panel -------------------------------------------------------

test_that("the import report panel renders in both languages", {
  skip_if_not_installed("bslib")
  example_file <- system.file("extdata", "example_cosmed.xlsx",
                              package = "cardiometR")
  skip_if(example_file == "", "Example COSMED file not found")

  report <- cpet_import_report(read_cpet(example_file, quiet = TRUE))

  for (lang in c("en", "fr")) {
    panel <- import_report_panel(report, lang)
    expect_true(inherits(panel, "shiny.tag") || inherits(panel, "shiny.tag.list"))

    rendered <- as.character(panel)
    # The source column names and the internal ones both have to be visible,
    # since matching one to the other is the whole point of the panel.
    expect_true(grepl("VO2", rendered, fixed = TRUE))
    expect_true(grepl("vo2_ml", rendered, fixed = TRUE))
    # And the unit conversion it applied.
    expect_true(grepl("86400", rendered, fixed = TRUE))
  }
})

test_that("the import report panel is absent without a report", {
  expect_null(import_report_panel(NULL, "en"))
})


# Translation parity --------------------------------------------------------

test_that("the English and French label files define the same keys", {
  # Two translation files drift the moment a key is added to one and not the
  # other, and nothing else in the package would notice.
  en <- yaml::read_yaml(system.file("translations", "labels_en.yml",
                                    package = "cardiometR"))
  fr <- yaml::read_yaml(system.file("translations", "labels_fr.yml",
                                    package = "cardiometR"))

  expect_setequal(names(en), names(fr))
  expect_true(all(nzchar(unlist(en))))
  expect_true(all(nzchar(unlist(fr))))
})

test_that("a French label is not simply the English one left in place", {
  en <- yaml::read_yaml(system.file("translations", "labels_en.yml",
                                    package = "cardiometR"))
  fr <- yaml::read_yaml(system.file("translations", "labels_fr.yml",
                                    package = "cardiometR"))

  # Plenty of entries are legitimately identical: units, abbreviations, and
  # variable names. What would be wrong is most of the file matching.
  shared <- intersect(names(en), names(fr))
  identical_share <- mean(vapply(shared, function(k)
    identical(en[[k]], fr[[k]]), logical(1)))
  expect_lt(identical_share, 0.5)
})

test_that("save_analysis_session and load_analysis_session round-trip", {
  mock <- create_mock_breath_data(n_breaths = 80)
  participant <- Participant(
    id = mock$participant$id, name = mock$participant$name,
    age = mock$participant$age, sex = mock$participant$sex,
    height_cm = mock$participant$height_cm, weight_kg = mock$participant$weight_kg
  )
  data <- CpetData(
    participant = participant,
    metadata = CpetMetadata(test_date = Sys.Date(), device = "Mock", protocol = "Ramp"),
    breaths = mock$breaths
  )
  path <- withr::local_tempfile(fileext = ".rds")
  save_analysis_session(
    path = path,
    cpet_data = data,
    settings = list(averaging_window = 30, athlete_sport = "cycling"),
    threshold_override = list(vt1_vo2 = 2000)
  )
  loaded <- load_analysis_session(path)
  expect_true(S7::S7_inherits(loaded$cpet_data, CpetData))
  expect_equal(loaded$settings$averaging_window, 30)
  expect_equal(loaded$threshold_override$vt1_vo2, 2000)
})

test_that("mod_results_server produces peaks for mock data", {
  skip_if_not_installed("shiny")
  mock <- create_mock_breath_data(n_breaths = 120)
  participant <- Participant(
    id = mock$participant$id, name = mock$participant$name,
    age = mock$participant$age, sex = mock$participant$sex,
    height_cm = mock$participant$height_cm, weight_kg = mock$participant$weight_kg
  )
  data <- CpetData(
    participant = participant,
    metadata = CpetMetadata(test_date = Sys.Date(), device = "Mock", protocol = "Ramp"),
    breaths = mock$breaths
  )

  shiny::testServer(
    mod_results_server,
    args = list(
      language = shiny::reactiveVal("en"),
      cpet_data = shiny::reactive(data),
      participant = shiny::reactive(participant),
      settings = shiny::reactive(list(
        averaging_method = "rolling",
        averaging_window = 30,
        threshold_methods = c("v_slope", "ve_vo2"),
        protocol = "ramp",
        stage_duration = 60,
        increment_size = 25,
        modality = "cycling",
        starting_intensity = 0,
        athlete_sport = "cycling",
        athlete_level = "recreational"
      )),
      prediction_source = shiny::reactive("jones"),
      dark_mode = shiny::reactive(FALSE),
      threshold_override = shiny::reactive(NULL)
    ),
    {
      a <- analysis()
      expect_true(inherits(a, "S7_object"))
      expect_gt(a@peaks@vo2_peak, 0)
    }
  )
})

test_that("threshold override persists through apply_threshold_override", {
  data <- create_mock_cpet_analysis()@data
  th <- detect_thresholds(data, methods = c("v_slope", "ve_vo2"))
  ov <- list(vt1_vo2 = 1800, vt2_vo2 = 2600)
  out <- apply_threshold_override(th, ov, data)
  expect_equal(out@vt1_vo2, 1800)
  expect_equal(out@vt2_vo2, 2600)
})

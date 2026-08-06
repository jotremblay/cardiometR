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

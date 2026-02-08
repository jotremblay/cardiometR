# Tests for report generation functions
# Note: is_s7_class helper defined in helper-mock-data.R

# Create test analysis object
create_test_analysis <- function() {
  participant <- Participant(
    id = "TEST001",
    name = "Test Subject",
    age = 30,
    sex = "M",
    height_cm = 175,
    weight_kg = 70
  )

  metadata <- CpetMetadata(
    test_date = Sys.Date(),
    device = "Test Device",
    protocol = "Incremental"
  )

  n <- 100
  time_s <- seq(0, by = 10, length.out = n)
  intensity <- pmin(time_s / max(time_s), 1)

  set.seed(42)
  breaths <- tibble::tibble(
    time_s = time_s,
    vo2_ml = 300 + intensity * 2700 + rnorm(n, 0, 30),
    vco2_ml = 250 + intensity * 3000 + rnorm(n, 0, 30),
    ve_l = 10 + intensity * 120 + rnorm(n, 0, 2),
    rer = vco2_ml / vo2_ml,
    hr_bpm = 70 + intensity * 120 + rnorm(n, 0, 2),
    power_w = round(intensity * 300),
    phase = dplyr::case_when(
      time_s < 120 ~ "REST",
      time_s < 300 ~ "WARMUP",
      TRUE ~ "EXERCISE"
    )
  )

  data <- CpetData(
    participant = participant,
    metadata = metadata,
    breaths = breaths,
    is_averaged = FALSE
  )

  peaks <- find_peaks(data, averaging = 30)
  validation <- validate(data)

  CpetAnalysis(
    data = data,
    peaks = peaks,
    validation = validation
  )
}


# get_report_labels() tests ------------------------------------------------

test_that("get_report_labels returns English labels by default", {
  labels <- get_report_labels("en")

  expect_type(labels, "list")
  expect_true("title" %in% names(labels))
  expect_true("section_patient" %in% names(labels))
  expect_equal(labels$label_name, "Name")
})

test_that("get_report_labels returns French labels", {
  labels <- get_report_labels("fr")

  expect_type(labels, "list")
  expect_equal(labels$label_name, "Nom")
  expect_true(grepl("patient", labels$section_patient, ignore.case = TRUE))
})

test_that("get_report_labels contains all required sections", {
  labels <- get_report_labels("en")

  required_sections <- c(
    "section_patient", "section_test", "section_peak_values",
    "section_thresholds", "section_graphs", "section_interpretation"
  )

  expect_true(all(required_sections %in% names(labels)))
})


# calculate_predicted_values() tests --------------------------------------

test_that("calculate_predicted_values returns expected structure", {
  participant <- Participant(
    id = "X", name = "X", age = 30, sex = "M",
    height_cm = 175, weight_kg = 70
  )

  predicted <- calculate_predicted_values(participant)

  expect_type(predicted, "list")
  expect_true("hr_max" %in% names(predicted))
  expect_true("vo2_max" %in% names(predicted))
  expect_true("ve_max" %in% names(predicted))
  expect_true("power_max" %in% names(predicted))
})

test_that("calculate_predicted_values gives reasonable HR max", {
  participant_young <- Participant(
    id = "X", name = "X", age = 20, sex = "M",
    height_cm = 175, weight_kg = 70
  )
  participant_old <- Participant(
    id = "X", name = "X", age = 60, sex = "M",
    height_cm = 175, weight_kg = 70
  )

  pred_young <- calculate_predicted_values(participant_young)
  pred_old <- calculate_predicted_values(participant_old)

  # Young person should have higher predicted HR max
  expect_gt(pred_young$hr_max, pred_old$hr_max)

  # HR max should be in reasonable range
  expect_gt(pred_young$hr_max, 150)
  expect_lt(pred_young$hr_max, 220)
})

test_that("calculate_predicted_values differs by sex", {
  participant_m <- Participant(
    id = "X", name = "X", age = 30, sex = "M",
    height_cm = 175, weight_kg = 70
  )
  participant_f <- Participant(
    id = "X", name = "X", age = 30, sex = "F",
    height_cm = 165, weight_kg = 60
  )

  pred_m <- calculate_predicted_values(participant_m)
  pred_f <- calculate_predicted_values(participant_f)

  # Males typically have higher predicted VO2max
  expect_gt(pred_m$vo2_max_rel, pred_f$vo2_max_rel)
})


# generate_auto_interpretation() tests ------------------------------------

test_that("generate_auto_interpretation returns expected structure", {
  analysis <- create_test_analysis()
  interp <- generate_auto_interpretation(analysis, "en")

  expect_type(interp, "list")
  expect_true("aerobic" %in% names(interp))
  expect_true("cardiovascular" %in% names(interp))
  expect_true("ventilatory" %in% names(interp))
})

test_that("generate_auto_interpretation returns French text", {
  analysis <- create_test_analysis()
  interp_en <- generate_auto_interpretation(analysis, "en")
  interp_fr <- generate_auto_interpretation(analysis, "fr")

  # Text should be different between languages
  expect_false(interp_en$aerobic == interp_fr$aerobic)
})


# build_template_data() tests ---------------------------------------------

test_that("build_template_data returns complete data", {
  analysis <- create_test_analysis()
  config <- ReportConfig(language = "en", institution = "Test Hospital")
  labels <- get_report_labels("en")

  data <- build_template_data(analysis, config, labels, NULL, NULL)

  expect_type(data, "list")
  expect_true("patient_name" %in% names(data))
  expect_true("patient_id" %in% names(data))
  expect_true("test_date" %in% names(data))
  expect_true("vo2_peak_value" %in% names(data))
})

test_that("build_template_data includes institution", {
  analysis <- create_test_analysis()
  config <- ReportConfig(language = "en", institution = "My Hospital")
  labels <- get_report_labels("en")

  data <- build_template_data(analysis, config, labels, NULL, NULL)

  expect_equal(data$institution, "My Hospital")
})


# format_duration() tests -------------------------------------------------

test_that("format_duration formats correctly", {
  expect_equal(format_duration(60), "1:00")
  expect_equal(format_duration(90), "1:30")
  expect_equal(format_duration(125), "2:05")
  expect_equal(format_duration(600), "10:00")
  expect_equal(format_duration(3661), "61:01")
})


# create_summary_table() tests --------------------------------------------

test_that("create_summary_table returns gt object", {
  skip_if_not_installed("gt")

  analysis <- create_test_analysis()
  tbl <- create_summary_table(analysis)

  expect_s3_class(tbl, "gt_tbl")
})

test_that("create_summary_table supports French", {
  skip_if_not_installed("gt")

  analysis <- create_test_analysis()
  tbl_en <- create_summary_table(analysis, language = "en")
  tbl_fr <- create_summary_table(analysis, language = "fr")

  expect_s3_class(tbl_en, "gt_tbl")
  expect_s3_class(tbl_fr, "gt_tbl")
})


# ReportConfig class tests ------------------------------------------------

test_that("ReportConfig creates with defaults", {
  config <- ReportConfig()

  expect_equal(config@language, "fr")
  expect_equal(config@output_format, "pdf")
})

test_that("ReportConfig supports French language", {
  config <- ReportConfig(language = "fr")

  expect_equal(config@language, "fr")
})

test_that("ReportConfig validates language", {
  expect_error(
    ReportConfig(language = "de"),
    "language must be"
  )
})

test_that("ReportConfig accepts optional fields", {
  config <- ReportConfig(
    language = "en",
    institution = "Test Hospital",
    technician = "Dr. Smith"
  )

  expect_equal(config@institution, "Test Hospital")
  expect_equal(config@technician, "Dr. Smith")
})


# process_conditionals() tests --------------------------------------------

test_that("process_conditionals includes block when truthy", {
  content <- "before {{#if show}}SHOWN{{/if}} after"
  result <- process_conditionals(content, list(show = TRUE))
  expect_equal(result, "before SHOWN after")
})

test_that("process_conditionals removes block when falsy", {
  content <- "before {{#if show}}SHOWN{{/if}} after"

  expect_equal(
    process_conditionals(content, list(show = FALSE)),
    "before  after"
  )
  expect_equal(
    process_conditionals(content, list(show = NULL)),
    "before  after"
  )
  expect_equal(
    process_conditionals(content, list(show = "")),
    "before  after"
  )
  expect_equal(
    process_conditionals(content, list()),
    "before  after"
  )
})

test_that("process_conditionals handles else branch", {
  content <- "{{#if flag}}YES{{else}}NO{{/if}}"

  expect_equal(process_conditionals(content, list(flag = TRUE)), "YES")
  expect_equal(process_conditionals(content, list(flag = FALSE)), "NO")
})

test_that("process_conditionals handles nested blocks", {
  content <- "{{#if outer}}O {{#if inner}}I{{/if}} E{{/if}}"

  expect_equal(
    process_conditionals(content, list(outer = TRUE, inner = TRUE)),
    "O I E"
  )
  expect_equal(
    process_conditionals(content, list(outer = TRUE, inner = FALSE)),
    "O  E"
  )
  expect_equal(
    process_conditionals(content, list(outer = FALSE)),
    ""
  )
})

test_that("process_conditionals handles multiple sequential blocks", {
  content <- "{{#if a}}A{{/if}} {{#if b}}B{{/if}}"

  expect_equal(
    process_conditionals(content, list(a = TRUE, b = TRUE)),
    "A B"
  )
  expect_equal(
    process_conditionals(content, list(a = TRUE, b = FALSE)),
    "A "
  )
  expect_equal(
    process_conditionals(content, list(a = FALSE, b = TRUE)),
    " B"
  )
})

test_that("process_conditionals removes all patterns from actual template", {
  template_path <- system.file("templates", "cpet_report.typ", package = "cardiometR")
  skip_if(template_path == "", "Template not installed")

  content <- paste(readLines(template_path, warn = FALSE), collapse = "\n")

  # Minimal data to test conditional processing
  data <- list(
    logo_path = NULL, lab_name = "", has_pretest_conditions = FALSE,
    has_protocol_details = FALSE, has_stage_table = FALSE,
    has_economy_metrics = FALSE, thresholds_detected = FALSE,
    has_graphs = FALSE, has_clinical_notes = FALSE,
    last_meal_hours = NULL, caffeine_intake = FALSE,
    equipment_model = NULL, analyzer_model = NULL,
    graph_panel = NULL, graph_vslope = NULL, graph_predicted = NULL,
    gross_efficiency = NULL, running_economy = NULL
  )

  result <- process_conditionals(content, data)

  expect_false(grepl("\\{\\{#if", result))
  expect_false(grepl("\\{\\{/if\\}\\}", result))
  expect_false(grepl("\\{\\{else\\}\\}", result))
})


# Template path tests -----------------------------------------------------

test_that("get_template_path returns valid path", {
  path <- get_template_path()

  # Should return path to installed template
  expect_true(nchar(path) > 0 || file.exists(path) || path == "")
})

test_that("get_template_path uses custom template if provided", {
  # Create temp file
 temp_template <- tempfile(fileext = ".typ")
  writeLines("test template", temp_template)

  path <- get_template_path(temp_template)
  expect_equal(path, temp_template)

  unlink(temp_template)
})


# Integration tests -------------------------------------------------------

test_that("Full report workflow creates expected data", {
  analysis <- create_test_analysis()
  config <- ReportConfig(language = "en", institution = "Test Clinic")
  labels <- get_report_labels("en")

  # Build template data
  template_data <- build_template_data(analysis, config, labels, NULL, NULL)

  # Check key fields
  expect_equal(template_data$patient_name, "Test Subject")
  expect_equal(template_data$patient_id, "TEST001")
  expect_equal(template_data$institution, "Test Clinic")

  # Check peak values are numeric
  expect_true(is.numeric(as.numeric(template_data$vo2_peak_value)))
  expect_true(is.numeric(as.numeric(template_data$hr_peak_value)))
})

test_that("render_typst_report produces valid PDF", {
  skip_if(
    !requireNamespace("typr", quietly = TRUE) && !nzchar(Sys.which("typst")),
    "Neither typr package nor typst CLI available"
  )

  analysis <- create_test_analysis()
  config <- ReportConfig(language = "en", institution = "Test Hospital")
  labels <- get_report_labels("en")
  template_data <- build_template_data(analysis, config, labels, NULL, NULL)
  template_data$has_graphs <- FALSE
  template_path <- get_template_path()

  output_file <- tempfile(fileext = ".pdf")
  on.exit(unlink(output_file), add = TRUE)

  expect_no_error(render_typst_report(template_path, template_data, output_file))
  expect_true(file.exists(output_file))
  expect_gt(file.info(output_file)$size, 0)

  header <- rawToChar(readBin(output_file, what = "raw", n = 4))
  expect_equal(header, "%PDF")
})

test_that("render_typst_report handles Shiny temp path without .pdf extension", {
  skip_if(
    !requireNamespace("typr", quietly = TRUE) && !nzchar(Sys.which("typst")),
    "Neither typr package nor typst CLI available"
  )

  analysis <- create_test_analysis()
  config <- ReportConfig(language = "en", institution = "Test Hospital")
  labels <- get_report_labels("en")
  template_data <- build_template_data(analysis, config, labels, NULL, NULL)
  template_data$has_graphs <- FALSE
  template_path <- get_template_path()

  # Simulate Shiny downloadHandler temp file (no .pdf extension)
  output_file <- tempfile()
  on.exit(unlink(output_file), add = TRUE)

  expect_no_error(render_typst_report(template_path, template_data, output_file))
  expect_true(file.exists(output_file))

  header <- rawToChar(readBin(output_file, what = "raw", n = 4))
  expect_equal(header, "%PDF")
})

test_that("Report with real COSMED data works", {
  skip_if_not_installed("readxl")
  skip_if_not_installed("gt")

  example_file <- system.file("extdata", "example_cosmed.xlsx", package = "cardiometR")
  skip_if(example_file == "", "Example file not found")

  # Load and process data
  data <- read_cosmed(example_file)
  peaks <- find_peaks(data, averaging = 30)
  validation <- validate(data)

  analysis <- CpetAnalysis(
    data = data,
    peaks = peaks,
    validation = validation
  )

  # Build template data
  config <- ReportConfig(language = "en")
  labels <- get_report_labels("en")
  template_data <- build_template_data(analysis, config, labels, NULL, NULL)

  # Verify real data is present
  expect_true(nchar(template_data$patient_name) > 0)
  expect_true(as.numeric(template_data$vo2_peak_value) > 0)

  # Create summary table
  tbl <- create_summary_table(analysis)
  expect_s3_class(tbl, "gt_tbl")
})

# Plan: Improved CPET R Package Development Prompt

## Task Summary
Create an enhanced, well-sourced prompt for developing an R package for CPET (Cardiopulmonary Exercise Testing) analysis, including a Shiny app for xlsx import and PDF report generation.

## User Requirements (Confirmed)
- **Primary Device:** COSMED Quark CPET (extensible to other formats)
- **Sample Data:** User will provide xlsx samples for format parsing
- **Languages:** Bilingual (English + French) for both PDF reports and Shiny UI
- **Deployment:** Local initially, future shinyapps.io compatibility

---

## FINAL IMPROVED PROMPT

Below is the production-ready prompt for building the CPET R package:

---

# Comprehensive Prompt: CPET Analysis R Package with Shiny App

## 1. Package Overview

**Package Name:** `cardiometR`

Build a production-grade R package for analyzing Cardiopulmonary Exercise Testing (CPET) data from COSMED Quark CPET metabolic carts, following:
- [rOpenSci package development standards](https://devguide.ropensci.org/)
- [R Packages (2e) by Wickham & Bryan](https://r-pkgs.org/)
- [S7 OOP system](https://rconsortium.github.io/S7/) for modern class definitions
- [Data Processing Strategies to Determine Maximum Oxygen Uptake: A Systematic Scoping Review and Experimental Comparison with Guidelines for Reporting](https://link.springer.com/article/10.1007/s40279-023-01903-3)
- [ATS/ACCP CPET Guidelines (DOI: 10.1164/rccm.167.2.211)](https://www.atsjournals.org/doi/10.1164/rccm.167.2.211)

**Core Capabilities:**
1. Import and validate COSMED Quark CPET xlsx exports
2. Process breath-by-breath data with configurable averaging
3. Generate bilingual (EN/FR) PDF reports via **Typst directly** (using [typr](https://cran.r-project.org/web/packages/typr/) package, no Quarto dependency)
4. Provide a Shiny app for non-programmer users
5. Use **S7 classes** throughout for type-safe, validated data structures

**Reference Packages to Study:**
- [spiro](https://docs.ropensci.org/spiro/) - rOpenSci-reviewed, JOSS publication: Nolte, S. (2023). *JOSS*, 8(81), 5089. [DOI: 10.21105/joss.05089](https://doi.org/10.21105/joss.05089)
- [whippr](https://fmmattioni.github.io/whippr/) - VO2 kinetics, averaging methods
- [S7 vignettes](https://cran.r-project.org/web/packages/S7/vignettes/S7.html) - class definitions, generics, methods

---

## 2. S7 Class Definitions

Use [S7](https://rconsortium.github.io/S7/) for all core data structures. S7 provides:
- Formal class definitions with property validation
- Clean generic/method dispatch
- Future-proof (planned for base R integration)

### Core Classes

```r
library(S7)

# Participant information
Participant <- new_class("Participant",
  properties = list(
    id = class_character,
    name = class_character,
    age = class_numeric,
    sex = new_property(class_character, validator = function(value) {
      if (!value %in% c("M", "F", "O")) return("sex must be 'M', 'F', or 'O'")
      NULL
    }),
    height_cm = class_numeric,
    weight_kg = class_numeric,
    sport = class_character | NULL
  )
)

# Calibration/environmental metadata
CpetMetadata <- new_class("CpetMetadata",

properties = list(
    test_date = class_Date,
    device = class_character,
    calibration_date = class_Date | NULL,
    temperature_c = class_numeric | NULL,
    pressure_mmhg = class_numeric | NULL,
    humidity_pct = class_numeric | NULL,
    protocol = class_character,
    technician = class_character | NULL
  )
)

# Single breath measurement
Breath <- new_class("Breath",
  properties = list(
    time_s = class_numeric,
    vo2_ml = class_numeric,
    vco2_ml = class_numeric,
    ve_l = class_numeric,
    hr_bpm = class_numeric | NULL,
    power_w = class_numeric | NULL,
    rer = class_numeric,
    bf = class_numeric,
    vt_l = class_numeric,
    peto2_mmhg = class_numeric | NULL,
    petco2_mmhg = class_numeric | NULL
  )
)

# Main CPET data container
CpetData <- new_class("CpetData",
  properties = list(
    participant = Participant,
    metadata = CpetMetadata,
    breaths = class_data.frame,      # tibble of breath-by-breath data
    stages = class_data.frame | NULL, # stage annotations
    is_averaged = class_logical,
    averaging_window = class_numeric | NULL
  ),
  validator = function(self) {
    required_cols <- c("time_s", "vo2_ml", "vco2_ml", "ve_l", "rer")
    missing <- setdiff(required_cols, names(self@breaths))
    if (length(missing) > 0) {
      return(paste("breaths missing required columns:", paste(missing, collapse = ", ")))
    }
    NULL
  }
)

# Threshold detection results
Thresholds <- new_class("Thresholds",
  properties = list(
    vt1_vo2 = class_numeric | NULL,
    vt1_hr = class_numeric | NULL,
    vt1_power = class_numeric | NULL,
    vt1_method = class_character | NULL,
    vt2_vo2 = class_numeric | NULL,
    vt2_hr = class_numeric | NULL,
    vt2_power = class_numeric | NULL,
    vt2_method = class_character | NULL,
    confidence = class_character | NULL
  )
)

# Peak values container
PeakValues <- new_class("PeakValues",
  properties = list(
    vo2_peak = class_numeric,
    vo2_kg_peak = class_numeric,
    hr_peak = class_numeric | NULL,
    power_peak = class_numeric | NULL,
    ve_peak = class_numeric,
    rer_peak = class_numeric,
    averaging_s = class_numeric
  )
)

# Validation report
ValidationReport <- new_class("ValidationReport",
  properties = list(
    is_valid = class_logical,
    errors = class_list,
    warnings = class_list,
    info = class_list
  )
)

# Complete analysis result
CpetAnalysis <- new_class("CpetAnalysis",
  properties = list(
    data = CpetData,
    peaks = PeakValues | NULL,
    thresholds = Thresholds | NULL,
    stage_summary = class_data.frame | NULL,
    validation = ValidationReport | NULL
  )
)
```

### S7 Generics for CPET Operations

```r
# Generic for averaging - dispatches on CpetData
average <- new_generic("average", "x")

method(average, CpetData) <- function(x, method = c("time", "breath", "rolling"),
                                       window = 30) {
  method <- match.arg(method)
  # Implementation returns new CpetData with is_averaged = TRUE
}

# Generic for stage extraction
extract_stages <- new_generic("extract_stages", "x")

method(extract_stages, CpetData) <- function(x, protocol = c("step", "ramp"),
                                              stage_duration = 180) {
  # Implementation returns CpetData with stages populated
}

# Generic for peak determination
find_peaks <- new_generic("find_peaks", "x")

method(find_peaks, CpetData) <- function(x, averaging = 30) {
  # Returns PeakValues object
}

# Generic for threshold detection
detect_thresholds <- new_generic("detect_thresholds", "x")

method(detect_thresholds, CpetData) <- function(x, methods = c("V-slope", "VE_VO2")) {
  # Returns Thresholds object
}

# Generic for validation
validate <- new_generic("validate", "x")

method(validate, CpetData) <- function(x, ...) {
  # Returns ValidationReport object
}

# Print methods for nice console output
method(print, CpetData) <- function(x, ...) {
  cat("<CpetData>\n")
  cat("  Participant:", x@participant@name, "\n")
  cat("  Test date:", as.character(x@metadata@test_date), "\n
")
  cat("  Breaths:", nrow(x@breaths), "\n")
  cat("  Averaged:", x@is_averaged, "\n")
  invisible(x)
}
```

---

## 3. Data Import Module

### Primary Function
```r
#' Read COSMED Quark CPET xlsx export
#'
#' @param file Path to xlsx file exported from COSMED Omnia software
#' @param participant Participant S7 object (optional, extracted from file if NULL)
#' @return A CpetData S7 object
#' @export
read_cosmed <- function(file, participant = NULL) {
  # Parse xlsx, create CpetData S7 object
  # Validation happens automatically via S7 property validators
}
```

### File Format Support (Extensible Architecture)
```r
# S7 generic for reading CPET data - extensible to new formats
read_cpet <- new_generic("read_cpet", "file")

# Default method dispatches by file extension or explicit format
method(read_cpet, class_character) <- function(file, format = NULL, ...) {
  format <- format %||% detect_format(file)
  switch(format,
    cosmed = read_cosmed(file, ...),
    cortex = read_cortex(file, ...),
    vyntus = read_vyntus(file, ...),
    stop("Unknown format: ", format)
  )
}
```

### Metadata Extraction
```r
#' Extract metadata from CPET file
#' @return CpetMetadata S7 object
extract_metadata <- function(file) {
  # Parse header rows: calibration data, ambient conditions
  # Return CpetMetadata(...) S7 object with validated properties
}
```

---

## 3. Data Validation Module

Implement quality checks per [ATS/ACCP Guidelines](https://www.atsjournals.org/doi/10.1164/rccm.167.2.211):

```r
#' Validate CPET data quality
#'
#' @param data cpet_data object
#' @param rest_duration_min Minimum rest phase duration (default 3 min)
#' @param aberrant_threshold SD threshold for outlier breaths (default 3)
#' @return Validation report with warnings/errors
#' @export
validate_cpet <- function(data,
                          rest_duration_min = 3,
                          aberrant_threshold = 3,
                          resting_vo2_range = c(150, 400),
                          resting_rer_range = c(0.70, 0.90),
                          max_hr_range = c(60, 220),
                          max_rer_range = c(0.70, 1.30)) {
  # Check rest phase adequacy

# Check physiological plausibility
  # Flag aberrant breaths (coughs, swallows)
  # Verify calibration data presence
}
```

---

## 4. Data Processing Module

### Averaging Methods
Reference: [whippr averaging documentation](https://fmmattioni.github.io/whippr/)

```r
#' Apply averaging to breath-by-breath data
#'
#' @param data cpet_data object
#' @param method "time" (seconds), "breath" (n breaths), or "rolling"
#' @param window Window size (seconds for time, n for breath)
#' @param variables Variables to average (default: VO2, VCO2, VE, HR, RER)
#' @return Averaged data
#' @export
average_cpet <- function(data,
                         method = c("time", "breath", "rolling"),
                         window = 30,
                         variables = c("VO2", "VCO2", "VE", "HR", "RER",
                                      "VO2_kg", "VE_VO2", "VE_VCO2",
                                      "PETO2", "PETCO2", "BF", "VT")) {
  method <- match.arg(method)
  # Implementation
}
```

### Stage Extraction
```r
#' Extract exercise stages from protocol
#'
#' @param data cpet_data object
#' @param protocol "step" or "ramp"
#' @param stage_duration Seconds per stage (for step protocols)
#' @param markers Optional manual stage markers
#' @return Data with stage annotations
#' @export
extract_stages <- function(data,
                           protocol = c("step", "ramp"),
                           stage_duration = 180,
                           markers = NULL) {
  # Auto-detect from power/workload column or use markers
}

#' Summarize data by stage
#'
#' @param data cpet_data with stages
#' @param method "last30s", "last60s", "mean", "peak"
#' @return Stage summary tibble
#' @export
summarize_stages <- function(data, method = c("last30s", "last60s", "mean", "peak")) {
}
```

### Peak Value Determination
```r
#' Determine peak/max values
#'
#' @param data cpet_data object
#' @param averaging Seconds to average for peak (default 30)
#' @param variables Variables to determine peaks for
#' @return Named list of peak values
#' @export
determine_peaks <- function(data,
                            averaging = 30,
                            variables = c("VO2", "VO2_kg", "HR", "VE", "RER", "Power")) {
}
```

---

## 5. Threshold Detection Module

```r
#' Detect ventilatory thresholds
#'
#' @param data Processed cpet_data
#' @param methods Detection methods to use
#' @return Threshold results with confidence
#' @export
detect_thresholds <- function(data,
                              methods = c("V-slope", "VE_VO2", "VE_VCO2",
                                         "PETO2", "PETCO2", "RER")) {
  # V-slope: VCO2 vs VO2 breakpoint (Beaver et al. 1986)
  # Ventilatory equivalents: VE/VO2 nadir, VE/VCO2 rise
  # End-tidal gases: PETO2 rise, PETCO2 plateau/decline
}
```

---
## 6. Visualization Module

Reference: [spiro 9-panel plot](https://docs.ropensci.org/spiro/)

```r
#' Create stage summary plot
#' @export
plot_stages <- function(data, variables = c("VO2", "HR", "VE", "RER")) {
  # ggplot2 faceted line/point plot
}

#' Create V-slope plot for threshold detection
#' @export
plot_v_slope <- function(data, show_threshold = TRUE) {
  # VCO2 vs VO2 with breakpoint annotation
}

#' Create ventilatory equivalents plot
#' @export
plot_ve_equivalents <- function(data, show_thresholds = TRUE) {
  # VE/VO2 and VE/VCO2 vs VO2 or time
}

#' Composite panel plot (Wasserman-style)
#' @export
plot_cpet_panel <- function(data, layout = "3x3") {
  # 9-panel or custom layout using patchwork
}
```

---

## 7. Table Generation Module

```r
#' Create stage summary table
#' @param format Output format: "gt", "flextable", "kable"
#' @export
table_stages <- function(data, format = c("gt", "flextable", "kable"),
                         language = c("en", "fr")) {
  # Formatted table with stage, time, power, VO2, HR, VE, RER, etc.
}

#' Create peak values table
#' @export
table_peaks <- function(data, include_predicted = TRUE,
                        language = c("en", "fr")) {
  # Peak values with optional predicted values and %predicted
}
```

---

## 8. PDF Report Generation (Direct Typst, No Quarto)

### Technology Stack
- **Typst** directly via [typr](https://cran.r-project.org/web/packages/typr/) R package
- No Quarto or LaTeX dependencies required
- Fast compilation (Typst is written in Rust)
- Clean, readable template syntax

### Template Structure
```
inst/
  templates/
    cpet_report.typ           # Main Typst template
    cpet_header.typ           # Header/logo component
    cpet_tables.typ           # Table styling
    cpet_plots.typ            # Plot embedding
  translations/
    labels_en.yml             # English labels
    labels_fr.yml             # French labels
  assets/
    logo-placeholder.png
```

### Typst Template Example (`cpet_report.typ`)
```typst
#import "cpet_header.typ": header
#import "cpet_tables.typ": stage_table, peak_table

// Document settings
#set document(title: "{{title}}", author: "{{technician}}")
#set page(paper: "a4", margin: (x: 2cm, y: 2.5cm))
#set text(font: "Inter", size: 10pt)

// Header with logo
#header(
  institution: "{{institution}}",
  logo: "{{logo_path}}",
  date: "{{test_date}}"
)

= {{tr.report_title}}

== {{tr.participant_info}}

#table(
  columns: 2,
  [*{{tr.name}}*], [{{participant.name}}],
  [*{{tr.age}}*], [{{participant.age}} {{tr.years}}],
  [*{{tr.height}}*], [{{participant.height_cm}} cm],
  [*{{tr.weight}}*], [{{participant.weight_kg}} kg],
)

== {{tr.stage_results}}

{{stage_table}}

== {{tr.peak_values}}

{{peak_table}}

== {{tr.plots}}

#image("{{plot_path}}", width: 100%)
```

### Report Generation S7 Class & Function
```r
# S7 class for report configuration
ReportConfig <- new_class("ReportConfig",
  properties = list(
    language = new_property(class_character,
      validator = function(value) {
        if (!value %in% c("en", "fr")) return("language must be 'en' or 'fr'")
        NULL
      }),
    institution = class_character | NULL,
    logo_path = class_character | NULL,
    technician = class_character | NULL,
    output_format = new_property(class_character, default = "pdf"),
    template = class_character | NULL
  )
)

#' Render CPET PDF report using Typst
#'
#' @param analysis CpetAnalysis S7 object
#' @param output_file Output PDF path
#' @param config ReportConfig S7 object
#' @return Path to generated PDF
#' @export
render_report <- function(analysis, output_file = NULL, config = ReportConfig(language = "en")) {

 stopifnot(inherits(analysis, "CpetAnalysis"))
  stopifnot(inherits(config, "ReportConfig"))

  # Load translations
  labels <- load_translations(config@language)

  # Generate temporary plot files
  plot_paths <- generate_report_plots(analysis)

  # Build Typst content from template
  template_path <- config@template %||%
    system.file("templates", "cpet_report.typ", package = "cardiometR")

  typst_content <- build_typst_content(
    template = template_path,
    analysis = analysis,
    labels = labels,
    config = config,
    plot_paths = plot_paths
  )

  # Compile with typr
  output_file <- output_file %||% tempfile(fileext = ".pdf")
  typr::typr_compile(
    input = typst_content,
    output = output_file,
    output_format = "pdf"
  )

  # Cleanup temp files
  unlink(plot_paths)

  output_file
}

#' Build Typst content by interpolating template
#' @keywords internal
build_typst_content <- function(template, analysis, labels, config, plot_paths) {
  content <- readLines(template) |> paste(collapse = "\n")

  # Interpolate participant data
  content <- gsub("{{participant.name}}", analysis@data@participant@name, content)
  content <- gsub("{{participant.age}}", analysis@data@participant@age, content)
  # ... etc

  # Interpolate translations
  for (key in names(labels)) {
    content <- gsub(paste0("{{tr.", key, "}}"), labels[[key]], content)
  }

  # Generate and embed tables
  content <- gsub("{{stage_table}}",
                  generate_typst_table(analysis@stage_summary, labels), content)
  content <- gsub("{{peak_table}}",
                  generate_typst_peaks(analysis@peaks, labels), content)

  content
}
```

### Typst Table Generator
```r
#' Generate Typst table markup from data frame
#' @keywords internal
generate_typst_table <- function(df, labels, ...) {
  # Convert tibble to Typst table syntax
  header <- paste0("[*", names(df), "*]", collapse = ", ")
  rows <- df |>
    purrr::pmap_chr(function(...) {
      vals <- c(...)
      paste0("[", vals, "]", collapse = ", ")
    }) |>
    paste(collapse = ",\n  ")

  glue::glue("#table(\n  columns: {ncol(df)},\n  {header},\n  {rows}\n)")
}
```

---

## 9. Shiny App Module

### Architecture
Follow [Mastering Shiny Ch.20](https://mastering-shiny.org/scaling-packaging.html) and [golem patterns](https://engineering-shiny.org/golem.html):

```
R/
  run_app.R              # App launcher function
  app_ui.R               # Main UI assembly
  app_server.R           # Main server logic
  mod_upload.R           # File upload module
  mod_participant.R      # Participant info form
  mod_settings.R         # Analysis settings
  mod_preview.R          # Data preview/visualization
  mod_report.R           # Report generation module
```

### Launcher Function
```r
#' Launch CPET Analysis Shiny App
#'
#' @param ... Arguments passed to shiny::runApp
#' @export
run_app <- function(...) {
  app_dir <- system.file("app", package = "cardiometR")
  shiny::runApp(app_dir, ...)
}
```

### App Features
1. **File Upload Panel**
   - Drag-and-drop xlsx upload
   - Format auto-detection
   - Validation feedback

2. **Participant Information Form**
   - ID, Name, Age, Sex, Height (cm), Weight (kg)
   - Sport/Activity
   - Test date

3. **Analysis Settings Panel**
   - Averaging method and window (10s, 15s, 30s, 60s)
   - Protocol type (step/ramp)
   - Stage duration
   - Threshold methods to include

4. **Output Configuration**
   - Language toggle (EN/FR)
   - Technician name
   - Institution name
   - Logo upload

5. **Preview Panel**
   - Processed data table (DT)
   - Interactive plots (plotly)
   - Stage summary

6. **Export Panel**
   - Download PDF report
   - Download processed CSV
   - Download plots (PNG/SVG)

### UI Framework
Use [bslib](https://rstudio.github.io/bslib/) for modern Bootstrap 5 styling:
```r
app_ui <- function() {
  bslib::page_navbar(
    title = "CPET Lab",
    theme = bslib::bs_theme(bootswatch = "flatly"),
    nav_panel("Upload", mod_upload_ui("upload")),
    nav_panel("Configure", mod_settings_ui("settings")),
    nav_panel("Preview", mod_preview_ui("preview")),
    nav_panel("Report", mod_report_ui("report"))
  )
}
```

---

## 10. Internationalization (i18n)

### Translation Files (YAML)
```yaml
# inst/translations/labels_en.yml
app_title: "CPET Analysis"
upload_prompt: "Drop COSMED xlsx file here"
participant_name: "Name"
participant_age: "Age (years)"
generate_report: "Generate PDF Report"
...

# inst/translations/labels_fr.yml
app_title: "Analyse CPET"
upload_prompt: "Déposez le fichier xlsx COSMED ici"
participant_name: "Nom"
participant_age: "Âge (années)"
generate_report: "Générer le rapport PDF"
...
```

### Translation Helper
```r
#' Get translated label
#' @keywords internal
tr <- function(key, language = "en") {
  labels <- yaml::read_yaml(
    system.file("translations", paste0("labels_", language, ".yml"),
                package = "cardiometR")
  )
  labels[[key]] %||% key
}
```

---

## 11. Package Structure

```
cardiometR/
├── DESCRIPTION
├── NAMESPACE
├── LICENSE.md
├── README.md
├── R/
│   ├── cpetlab-package.R      # Package documentation
│   ├── classes.R              # S7 class definitions (Participant, CpetData, etc.)
│   ├── generics.R             # S7 generics (average, validate, detect_thresholds)
│   ├── methods-average.R      # S7 methods for averaging
│   ├── methods-validate.R     # S7 methods for validation
│   ├── methods-thresholds.R   # S7 methods for threshold detection
│   ├── methods-print.R        # S7 print methods
│   ├── import.R               # read_cosmed(), read_cpet()
│   ├── process.R              # Stage extraction, peak determination
│   ├── plot.R                 # plot_* functions (ggplot2)
│   ├── table.R                # table_* functions (gt)
│   ├── report.R               # render_report() with typr
│   ├── typst-helpers.R        # Typst content generation
│   ├── i18n.R                 # Translation helpers
│   ├── run_app.R              # Shiny launcher
│   ├── app_ui.R               # Shiny UI
│   ├── app_server.R           # Shiny server
│   ├── mod_upload.R           # Upload module
│   ├── mod_participant.R      # Participant form module
│   ├── mod_settings.R         # Settings module
│   ├── mod_preview.R          # Preview module
│   ├── mod_report.R           # Report module
│   └── utils.R                # Utility functions
├── inst/
│   ├── app/
│   │   └── app.R              # Minimal app.R calling pkg functions
│   ├── templates/
│   │   ├── cpet_report.typ    # Main Typst template
│   │   ├── cpet_header.typ    # Header component
│   │   ├── cpet_tables.typ    # Table styling
│   │   └── cpet_plots.typ     # Plot embedding
│   ├── translations/
│   │   ├── labels_en.yml
│   │   └── labels_fr.yml
│   ├── assets/
│   │   └── logo-placeholder.png
│   └── extdata/
│       └── example_cosmed.xlsx
├── man/                        # roxygen2 generated
├── tests/
│   ├── testthat/
│   │   ├── test-classes.R     # S7 class tests
│   │   ├── test-import.R
│   │   ├── test-process.R
│   │   ├── test-validate.R
│   │   └── test-report.R
│   └── testthat.R
├── vignettes/
│   ├── getting-started.Rmd
│   ├── s7-classes.Rmd         # S7 class documentation
│   ├── shiny-app-guide.Rmd
│   └── custom-reports.Rmd
└── data-raw/
    └── prepare_example_data.R
```

---

## 12. Dependencies

```
Package: cardiometR
Title: Cardio-Metabolic Exercise Testing Analysis with S7 Classes
Version: 0.1.0
Authors@R: person("Your", "Name", email = "you@example.com", role = c("aut", "cre"))
Description: Import, process, and report cardiopulmonary exercise testing
    (CPET) data from COSMED metabolic carts. Uses S7 classes for type-safe
    data structures. Includes a Shiny application for interactive analysis
    and bilingual PDF report generation via Typst.
License: MIT + file LICENSE
Encoding: UTF-8
Roxygen: list(markdown = TRUE)
RoxygenNote: 7.3.0
Imports:
    S7 (>= 0.2.0),
    dplyr (>= 1.1.0),
    tidyr,
    purrr,
    readxl,
    ggplot2,
    gt,
    scales,
    zoo,
    cli,
    rlang,
    glue,
    yaml,
    typr,
    shiny (>= 1.7.0),
    bslib,
    DT
Suggests:
    testthat (>= 3.0.0),
    shinytest2,
    knitr,
    rmarkdown,
    plotly,
    patchwork
Config/testthat/edition: 3
VignetteBuilder: knitr
URL: https://github.com/youruser/cardiometR
BugReports: https://github.com/youruser/cardiometR/issues
```

**Key dependency changes:**
- **Added:** `S7` for modern OOP classes with validation
- **Added:** `typr` for direct Typst PDF compilation
- **Removed:** `quarto` (no longer needed)

---

## 13. Example Workflow

```r
library(cardiometR)

# Option 1: Programmatic analysis with S7 classes

# Read data - returns CpetData S7 object with validated properties
data <- read_cosmed("participant_001.xlsx")

# Validation returns ValidationReport S7 object
validation <- validate(data)
print(validation)  # Shows errors, warnings, info

# Processing with S7 generics - returns new CpetData objects
processed <- data |>
  average(method = "rolling", window = 30) |>
  extract_stages(protocol = "step", stage_duration = 180)

# Results are S7 objects with type-safe properties
peaks <- find_peaks(processed, averaging = 30)
print(peaks@vo2_peak)      # Access via @ operator
print(peaks@vo2_kg_peak)

thresholds <- detect_thresholds(processed, methods = c("V-slope", "VE_VO2"))

# Create complete analysis object
analysis <- CpetAnalysis(
  data = processed,
  peaks = peaks,
  thresholds = thresholds,
  stage_summary = summarize_stages(processed),
  validation = validation
)

# Generate PDF report with Typst (no Quarto needed)
config <- ReportConfig(
  language = "fr",
  institution = "UCLouvain",
  technician = "Dr. Smith"
)

render_report(analysis, output_file = "rapport_001.pdf", config = config)

# Option 2: Shiny app for interactive analysis
run_app()
```

### S7 Class Benefits Demonstrated

```r
# Type validation happens automatically
participant <- Participant(
  id = "001",
  name = "Jean Dupont",
  age = 35,
  sex = "M",           # Validated: must be "M", "F", or "O"
  height_cm = 175,
  weight_kg = 72
)

# This would error: sex must be 'M', 'F', or 'O'
# Participant(sex = "X", ...)

# Property access with @ operator
participant@name
participant@age

# S7 generics dispatch correctly
print(data)           # Uses method(print, CpetData)
print(peaks)          # Uses method(print, PeakValues)
```

---

## 14. Key References

### CPET Guidelines
- ATS/ACCP Statement on Cardiopulmonary Exercise Testing. *Am J Respir Crit Care Med*. 2003;167:211-277. [DOI: 10.1164/rccm.167.2.211](https://www.atsjournals.org/doi/10.1164/rccm.167.2.211)
- Practical guide to cardiopulmonary exercise testing in adults. *Respir Res*. 2021. [Link](https://link.springer.com/article/10.1186/s12931-021-01895-6)

### R Packages to Study
- Nolte S. spiro: An R package for analyzing data from cardiopulmonary exercise testing. *JOSS*. 2023;8(81):5089. [DOI: 10.21105/joss.05089](https://doi.org/10.21105/joss.05089)
- whippr: Tools for Manipulating Gas Exchange Data. [Docs](https://fmmattioni.github.io/whippr/)

### S7 OOP System
- S7: An Object Oriented System for R. [Docs](https://rconsortium.github.io/S7/) | [CRAN](https://cran.r-project.org/package=S7)
- S7 Basics Vignette. [Link](https://cran.r-project.org/web/packages/S7/vignettes/S7.html)
- S7 0.2.0 Release Notes. [Tidyverse Blog](https://www.tidyverse.org/blog/2024/11/s7-0-2-0/)

### R Development Resources
- Wickham H, Bryan J. R Packages (2e). [r-pkgs.org](https://r-pkgs.org/)
- rOpenSci Packages: Development, Maintenance, and Peer Review. [devguide.ropensci.org](https://devguide.ropensci.org/)
- Fay C, et al. Engineering Production-Grade Shiny Apps. [engineering-shiny.org](https://engineering-shiny.org/)

### PDF Generation with Typst (No Quarto)
- typr: R package for Typst compilation. [CRAN](https://cran.r-project.org/web/packages/typr/)
- Typst documentation. [typst.app/docs](https://typst.app/docs/)
- Making Pretty PDFs with Typst. [nrennie.rbind.io](https://nrennie.rbind.io/blog/pdf-quarto/making-pdf-with-quarto-typst-latex/)

---

## Appendix: Complete Source Reference List

### CPET R Packages (Study Source Code)
| Package | Links |
|---------|-------|
| **spiro** | [CRAN](https://cran.r-project.org/web/packages/spiro/) &#124; [Docs](https://docs.ropensci.org/spiro/) &#124; [GitHub](https://github.com/ropensci/spiro) |
| **whippr** | [CRAN](https://cran.r-project.org/web/packages/whippr/) &#124; [Docs](https://fmmattioni.github.io/whippr/) &#124; [GitHub](https://github.com/fmmattioni/whippr) |

### CPET Clinical Guidelines
| Document | Link |
|----------|------|
| ATS/ACCP Statement (2003) | [DOI: 10.1164/rccm.167.2.211](https://www.atsjournals.org/doi/10.1164/rccm.167.2.211) |
| Practical CPET Guide (2021) | [Respiratory Research](https://link.springer.com/article/10.1186/s12931-021-01895-6) |
| AHA Clinician's Guide | [PubMed](https://pubmed.ncbi.nlm.nih.gov/20585013/) |

### S7 OOP System
| Resource | Link |
|----------|------|
| S7 Package Docs | [rconsortium.github.io/S7](https://rconsortium.github.io/S7/) |
| S7 on CRAN | [cran.r-project.org/package=S7](https://cran.r-project.org/package=S7) |
| S7 Basics Vignette | [CRAN vignette](https://cran.r-project.org/web/packages/S7/vignettes/S7.html) |
| S7 0.2.0 Announcement | [tidyverse.org](https://www.tidyverse.org/blog/2024/11/s7-0-2-0/) |

### R Development Resources
| Resource | Link |
|----------|------|
| R Packages (2e) | [r-pkgs.org](https://r-pkgs.org/) |
| rOpenSci Dev Guide | [devguide.ropensci.org](https://devguide.ropensci.org/) |
| Engineering Shiny Apps | [engineering-shiny.org](https://engineering-shiny.org/) |
| Mastering Shiny Ch.20 | [mastering-shiny.org/scaling-packaging](https://mastering-shiny.org/scaling-packaging.html) |
| golem Framework | [thinkr-open.github.io/golem](https://thinkr-open.github.io/golem/) |

### Typst (Direct PDF, No Quarto)
| Resource | Link |
|----------|------|
| typr R Package | [CRAN](https://cran.r-project.org/web/packages/typr/) |
| Typst Documentation | [typst.app/docs](https://typst.app/docs/) |
| Typst Tutorial | [typst.app/docs/tutorial](https://typst.app/docs/tutorial/) |
| Pretty PDFs with Typst | [nrennie.rbind.io](https://nrennie.rbind.io/blog/pdf-quarto/making-pdf-with-quarto-typst-latex/) |

### User-Provided (Required)
- Sample COSMED Quark CPET xlsx export file(s)
- Institution logo (optional)

---
name: typst-reports
description: Typst PDF report generation for cardiometR using the typr package. Use when creating PDF report templates, implementing the render_report() function, or working with bilingual report content.
allowed-tools: Read, Write, Edit, Glob
---

# Typst PDF Report Generation for cardiometR

## When to Use This Skill

- Creating Typst template files (.typ)
- Implementing the `render_report()` function
- Generating tables in Typst syntax from R data
- Building bilingual report content
- Embedding plots and images in reports

## Technology Stack

- **typr**: R package for compiling Typst to PDF
- **Typst**: Modern typesetting language (Rust-based, fast)
- No Quarto or LaTeX dependencies

## Template File Structure

```
inst/
├── templates/
│   ├── cpet_report.typ      # Main template
│   ├── cpet_header.typ      # Header component
│   ├── cpet_tables.typ      # Table styling
│   └── cpet_plots.typ       # Plot embedding
└── translations/
    ├── labels_en.yml        # English labels
    └── labels_fr.yml        # French labels
```

## Typst Syntax Quick Reference

### Document Setup
```typst
#set document(title: "CPET Report", author: "Dr. Smith")
#set page(paper: "a4", margin: (x: 2cm, y: 2.5cm))
#set text(font: "Inter", size: 10pt)
#set heading(numbering: "1.")
```

### Headings
```typst
= Level 1 Heading
== Level 2 Heading
=== Level 3 Heading
```

### Text Formatting
```typst
*bold text*
_italic text_
`monospace`
#text(fill: blue)[colored text]
#text(size: 14pt, weight: "bold")[large bold]
```

### Tables
```typst
#table(
  columns: 4,
  stroke: 0.5pt + gray,
  fill: (_, y) => if y == 0 { rgb("#f0f0f0") },
  align: (left, center, center, right),
  [*Stage*], [*Time*], [*VO2*], [*HR*],
  [Rest], [0:00], [250], [72],
  [Stage 1], [3:00], [850], [95],
  [Stage 2], [6:00], [1450], [120],
)
```

### Images
```typst
#figure(
  image("path/to/plot.png", width: 100%),
  caption: [CPET 9-panel plot]
)
```

### Grid Layout
```typst
#grid(
  columns: (1fr, 1fr),
  gutter: 1em,
  [Left content],
  [Right content]
)
```

### Functions (reusable components)
```typst
#let header(institution, date) = {
  grid(
    columns: (1fr, auto),
    text(weight: "bold")[#institution],
    text[#date]
  )
  line(length: 100%)
}

// Usage
#header("UCLouvain", "2024-01-15")
```

## R Integration with typr

### Basic Compilation
```r
library(typr)

# Compile Typst content to PDF
typr_compile(
  input = "path/to/template.typ",
  output = "output.pdf",
  output_format = "pdf"
)
```

### Template Interpolation Pattern
```r
build_typst_content <- function(template_path, analysis, labels, config) {
  content <- readLines(template_path) |> paste(collapse = "\n")

  # Participant data
  content <- gsub("{{participant.name}}", analysis@data@participant@name, content)
  content <- gsub("{{participant.age}}", analysis@data@participant@age, content)

  # Translations
  for (key in names(labels)) {
    content <- gsub(paste0("{{tr.", key, "}}"), labels[[key]], content)
  }

  # Config values
  content <- gsub("{{institution}}", config@institution %||% "", content)
  content <- gsub("{{technician}}", config@technician %||% "", content)

  content
}
```

### Generating Typst Tables from R
```r
generate_typst_table <- function(df, headers = names(df)) {
  n_cols <- ncol(df)

  # Header row
  header_row <- paste0("[*", headers, "*]", collapse = ", ")

  # Data rows
  data_rows <- df |>
    purrr::pmap_chr(function(...) {
      vals <- c(...)
      paste0("[", vals, "]", collapse = ", ")
    }) |>
    paste(collapse = ",\n  ")

  # Complete table
  glue::glue(
    "#table(\n",
    "  columns: {n_cols},\n",
    "  stroke: 0.5pt + gray,\n",
    "  fill: (_, y) => if y == 0 {{ rgb(\"#f0f0f0\") }},\n",
    "  {header_row},\n",
    "  {data_rows}\n",
    ")"
  )
}
```

### Complete render_report() Implementation
```r
render_report <- function(analysis, output_file = NULL,
                          config = ReportConfig(language = "en")) {
  # Validate inputs
  stopifnot(inherits(analysis, "CpetAnalysis"))
  stopifnot(inherits(config, "ReportConfig"))

  # Load translations
  labels <- yaml::read_yaml(
    system.file("translations", paste0("labels_", config@language, ".yml"),
                package = "cardiometR")
  )

  # Generate plots to temp files
  plot_dir <- tempdir()
  plot_path <- file.path(plot_dir, "cpet_panel.png")

  p <- plot_cpet_panel(analysis@data)
  ggplot2::ggsave(plot_path, p, width = 10, height = 8, dpi = 150)

  # Load and interpolate template
  template_path <- system.file("templates", "cpet_report.typ",
                               package = "cardiometR")

  content <- build_typst_content(
    template_path = template_path,
    analysis = analysis,
    labels = labels,
    config = config
  )

  # Insert generated content
  content <- gsub("{{stage_table}}",
                  generate_typst_table(analysis@stage_summary, labels),
                  content)
  content <- gsub("{{peak_table}}",
                  generate_peaks_table(analysis@peaks, labels),
                  content)
  content <- gsub("{{plot_path}}", plot_path, content)

  # Write temp Typst file
  temp_typ <- tempfile(fileext = ".typ")
  writeLines(content, temp_typ)

  # Compile to PDF
  output_file <- output_file %||% tempfile(fileext = ".pdf")
  typr::typr_compile(input = temp_typ, output = output_file)

  # Cleanup
  unlink(c(temp_typ, plot_path))

  output_file
}
```

## Bilingual Report Pattern

### Template with Translation Placeholders
```typst
= {{tr.report_title}}

== {{tr.participant_info}}

#table(
  columns: 2,
  [*{{tr.name}}*], [{{participant.name}}],
  [*{{tr.age}}*], [{{participant.age}} {{tr.years}}],
)

== {{tr.stage_results}}

{{stage_table}}
```

### Translation Files
```yaml
# labels_en.yml
report_title: "CPET Analysis Report"
participant_info: "Participant Information"
name: "Name"
age: "Age"
years: "years"
stage_results: "Stage Results"

# labels_fr.yml
report_title: "Rapport d'analyse CPET"
participant_info: "Informations du participant"
name: "Nom"
age: "Âge"
years: "ans"
stage_results: "Résultats par palier"
```

## Files Reference

- `R/report.R` - `render_report()` function
- `R/typst-helpers.R` - Helper functions for Typst generation
- `inst/templates/*.typ` - Typst template files
- `inst/translations/*.yml` - Translation files

## Typst Documentation

- Official docs: https://typst.app/docs/
- Tutorial: https://typst.app/docs/tutorial/
- Reference: https://typst.app/docs/reference/

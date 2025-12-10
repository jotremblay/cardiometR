---
description: Create or edit Typst template component for PDF reports
argument-hint: [header|table|plot|main|custom-name]
allowed-tools: Read, Write, Edit, Glob
---

# Typst Template Generator

Create or edit a Typst template component for cardiometR PDF reports.

## Component to Create/Edit
- **Type**: $1
- **Additional context**: $ARGUMENTS

## Instructions

1. Read existing templates in `inst/templates/` for patterns
2. Read `R/typst-helpers.R` for the template interpolation logic
3. Create/edit the appropriate `.typ` file

## Template Location

Based on component type:
- `header` → `inst/templates/cpet_header.typ`
- `table` → `inst/templates/cpet_tables.typ`
- `plot` → `inst/templates/cpet_plots.typ`
- `main` → `inst/templates/cpet_report.typ`
- Custom → `inst/templates/cpet_{name}.typ`

## Main Report Template Pattern

```typst
// cpet_report.typ - Main report template
#import "cpet_header.typ": header
#import "cpet_tables.typ": stage_table, peak_table

// Document settings
#set document(title: "{{title}}", author: "{{technician}}")
#set page(paper: "a4", margin: (x: 2cm, y: 2.5cm))
#set text(font: "Inter", size: 10pt)
#set heading(numbering: "1.")

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
  stroke: 0.5pt + gray,
  [*{{tr.name}}*], [{{participant.name}}],
  [*{{tr.age}}*], [{{participant.age}} {{tr.years}}],
  [*{{tr.sex}}*], [{{participant.sex}}],
  [*{{tr.height}}*], [{{participant.height_cm}} cm],
  [*{{tr.weight}}*], [{{participant.weight_kg}} kg],
)

== {{tr.stage_results}}

{{stage_table}}

== {{tr.peak_values}}

{{peak_table}}

#pagebreak()

== {{tr.plots}}

#figure(
  image("{{plot_path}}", width: 100%),
  caption: [{{tr.cpet_panel_caption}}]
)
```

## Header Component Pattern

```typst
// cpet_header.typ
#let header(institution: "", logo: none, date: "") = {
  grid(
    columns: (1fr, auto),
    align: (left, right),
    [
      #if logo != none {
        image(logo, height: 1.5cm)
      }
      #text(size: 14pt, weight: "bold")[#institution]
    ],
    [
      #text(size: 10pt)[#date]
    ]
  )
  line(length: 100%, stroke: 0.5pt)
  v(0.5cm)
}
```

## Table Component Pattern

```typst
// cpet_tables.typ
#let cpet_table(data, headers) = {
  table(
    columns: data.at(0).len(),
    fill: (_, y) => if y == 0 { rgb("#f0f0f0") },
    stroke: 0.5pt + gray,
    align: center,
    ..headers.map(h => [*#h*]),
    ..data.flatten().map(cell => [#cell])
  )
}
```

## Interpolation Placeholders

Use these placeholders in templates (replaced by R):
- `{{participant.name}}` - Participant properties
- `{{tr.label_key}}` - Translated labels
- `{{stage_table}}` - Generated Typst table markup
- `{{plot_path}}` - Path to generated plot image

## After Creating

Ensure `R/typst-helpers.R` has interpolation logic for any new placeholders.

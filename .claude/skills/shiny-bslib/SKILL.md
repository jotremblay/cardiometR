---
name: shiny-bslib
description: Shiny app development patterns with bslib for cardiometR. Use when building Shiny UI components, creating modules, implementing i18n support, or designing the interactive CPET analysis interface.
allowed-tools: Read, Write, Edit, Glob
---

# Shiny + bslib Development for cardiometR

## When to Use This Skill

- Creating Shiny UI components with bslib
- Implementing Shiny modules (UI + server pairs)
- Adding internationalization (i18n) support
- Building interactive data visualization
- Designing the CPET analysis workflow

## cardiometR Shiny Architecture

```
R/
├── run_app.R         # App launcher: run_app()
├── app_ui.R          # Main UI assembly
├── app_server.R      # Main server logic
├── mod_upload.R      # File upload module
├── mod_participant.R # Participant info form
├── mod_settings.R    # Analysis settings
├── mod_preview.R     # Data preview/plots
└── mod_report.R      # Report generation
```

## bslib Component Patterns

### Page Layout
```r
app_ui <- function() {
  bslib::page_navbar(
    title = "CPET Lab",
    theme = bslib::bs_theme(
      bootswatch = "flatly",
      primary = "#0d6efd"
    ),
    nav_panel("Upload", mod_upload_ui("upload")),
    nav_panel("Configure", mod_settings_ui("settings")),
    nav_panel("Preview", mod_preview_ui("preview")),
    nav_panel("Report", mod_report_ui("report"))
  )
}
```

### Cards
```r
bslib::card(
  bslib::card_header(
    class = "bg-primary text-white",
    "Card Title"
  ),
  bslib::card_body(
    # Content here
  ),
  bslib::card_footer(
    shiny::actionButton("btn", "Action")
  )
)
```

### Value Boxes (for peak values)
```r
bslib::value_box(
  title = "Peak VO2",
  value = paste(round(peak_vo2, 1), "mL/kg/min"),
  showcase = bsicons::bs_icon("heart-pulse"),
  theme = "primary"
)
```

### Layout Columns
```r
bslib::layout_columns(
  col_widths = c(4, 8),  # Bootstrap 12-column grid
  # Left column (4/12)
  bslib::card(...),
  # Right column (8/12)
  bslib::card(...)
)
```

### Accordion (collapsible sections)
```r
bslib::accordion(
  id = "settings_accordion",
  bslib::accordion_panel(
    title = "Averaging Settings",
    icon = bsicons::bs_icon("sliders"),
    # Content
  ),
  bslib::accordion_panel(
    title = "Threshold Detection",
    # Content
  )
)
```

## Module Pattern

### UI Function
```r
mod_example_ui <- function(id, language = "en") {
  ns <- shiny::NS(id)

  bslib::card(
    bslib::card_header(tr("example_title", language)),
    bslib::card_body(
      shiny::selectInput(
        ns("method"),
        label = tr("method_label", language),
        choices = c(
          tr("option_a", language) = "a",
          tr("option_b", language) = "b"
        )
      ),
      shiny::numericInput(
        ns("window"),
        label = tr("window_label", language),
        value = 30,
        min = 10,
        max = 60
      )
    )
  )
}
```

### Server Function
```r
mod_example_server <- function(id, language, cpet_data) {
  shiny::moduleServer(id, function(input, output, session) {

    # Reactive values
    rv <- shiny::reactiveValues(result = NULL)

    # Computed value depending on reactive input
    processed <- shiny::reactive({
      req(cpet_data())
      average(cpet_data(), method = input$method, window = input$window)
    })

    # Return for parent module
    return(list(
      processed = processed
    ))
  })
}
```

## i18n Integration

### Translation Helper
```r
# R/i18n.R
tr <- function(key, language = "en") {
  labels <- yaml::read_yaml(
    system.file("translations", paste0("labels_", language, ".yml"),
                package = "cardiometR")
  )
  labels[[key]] %||% key
}
```

### Dynamic Language Switching
```r
# In app_server.R
language <- shiny::reactiveVal("en")

shiny::observeEvent(input$language_toggle, {
  current <- language()
  language(if (current == "en") "fr" else "en")
})

# Pass to modules
mod_example_server("example", language = language, ...)
```

### UI with Language Toggle
```r
# In nav_spacer area
bslib::nav_spacer(),
bslib::nav_item(
  shiny::actionLink("language_toggle", "EN | FR")
)
```

## File Upload Pattern

```r
mod_upload_ui <- function(id, language = "en") {
  ns <- shiny::NS(id)

  bslib::card(
    bslib::card_header(tr("upload_title", language)),
    bslib::card_body(
      shiny::fileInput(
        ns("file"),
        label = tr("upload_prompt", language),
        accept = c(".xlsx", ".xls"),
        placeholder = tr("no_file_selected", language),
        buttonLabel = tr("browse", language)
      ),
      shiny::uiOutput(ns("validation_message"))
    )
  )
}

mod_upload_server <- function(id, language) {
  shiny::moduleServer(id, function(input, output, session) {

    cpet_data <- shiny::reactive({
      req(input$file)
      tryCatch(
        read_cosmed(input$file$datapath),
        error = function(e) {
          showNotification(e$message, type = "error")
          NULL
        }
      )
    })

    output$validation_message <- shiny::renderUI({
      req(cpet_data())
      validation <- validate(cpet_data())
      if (validation@is_valid) {
        shiny::tags$div(
          class = "alert alert-success",
          tr("file_valid", language())
        )
      } else {
        shiny::tags$div(
          class = "alert alert-danger",
          shiny::tags$ul(
            purrr::map(validation@errors, shiny::tags$li)
          )
        )
      }
    })

    return(list(data = cpet_data))
  })
}
```

## Interactive Plots with plotly

```r
output$cpet_plot <- plotly::renderPlotly({
  req(cpet_data())

  p <- ggplot2::ggplot(cpet_data()@breaths, ggplot2::aes(x = time_s)) +
    ggplot2::geom_line(ggplot2::aes(y = vo2_ml), color = "blue") +
    ggplot2::geom_line(ggplot2::aes(y = vco2_ml), color = "red") +
    ggplot2::labs(x = tr("time_s", language()), y = tr("gas_exchange", language())) +
    ggplot2::theme_minimal()

  plotly::ggplotly(p)
})
```

## Data Tables with DT

```r
output$stage_table <- DT::renderDataTable({
  req(stage_summary())

  DT::datatable(
    stage_summary(),
    options = list(
      pageLength = 10,
      scrollX = TRUE,
      language = list(
        url = if (language() == "fr") {
          "//cdn.datatables.net/plug-ins/1.10.25/i18n/French.json"
        }
      )
    ),
    rownames = FALSE
  )
})
```

## Files Reference

- `R/run_app.R` - Entry point
- `R/app_ui.R` - Main UI
- `R/app_server.R` - Main server
- `R/mod_*.R` - Individual modules
- `R/i18n.R` - Translation helper
- `inst/translations/*.yml` - Label files

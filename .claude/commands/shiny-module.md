---
description: Create Shiny module with UI and server functions for cardiometR app
argument-hint: [module-name]
allowed-tools: Read, Write, Edit, Glob
---

# Shiny Module Generator

Create a new Shiny module following cardiometR patterns with bslib and i18n support.

## Module to Create
- **Name**: $1
- **File**: `R/mod_$1.R`

## Instructions

1. Read existing modules in `R/mod_*.R` for patterns
2. Read `R/i18n.R` for the `tr()` translation helper
3. Create `R/mod_$1.R` with UI and server functions

## Module Template

```r
#' $1 Module UI
#'
#' @param id Module namespace ID
#' @param language Current language ("en" or "fr")
#'
#' @return A bslib UI component
#' @export
mod_$1_ui <- function(id, language = "en") {
  ns <- shiny::NS(id)

  bslib::card(
    bslib::card_header(
      tr("$1_title", language)
    ),
    bslib::card_body(
      # UI elements here
      shiny::textInput(
        ns("input_field"),
        label = tr("$1_input_label", language)
      ),
      shiny::actionButton(
        ns("submit"),
        label = tr("submit", language),
        class = "btn-primary"
      )
    )
  )
}

#' $1 Module Server
#'
#' @param id Module namespace ID
#' @param language Reactive language value
#' @param ... Additional reactive inputs from parent
#'
#' @return A list of reactive values for parent consumption
#' @export
mod_$1_server <- function(id, language, ...) {
  shiny::moduleServer(id, function(input, output, session) {

    # Reactive values for this module
    rv <- shiny::reactiveValues(
      result = NULL
    )

    # Event handlers
    shiny::observeEvent(input$submit, {
      # Process input
      rv$result <- input$input_field
    })

    # Return reactive values for parent
    return(
      list(
        result = shiny::reactive(rv$result)
      )
    )
  })
}
```

## After Creating

1. Add translation keys to `inst/translations/labels_en.yml`:
```yaml
$1_title: "Module Title"
$1_input_label: "Input Label"
```

2. Add French translations to `inst/translations/labels_fr.yml`:
```yaml
$1_title: "Titre du module"
$1_input_label: "Libellé d'entrée"
```

3. Register module in `R/app_ui.R` and `R/app_server.R`

## bslib Components Reference

Common bslib components for cardiometR:
- `bslib::card()`, `card_header()`, `card_body()` - Card layouts
- `bslib::layout_columns()` - Responsive column layouts
- `bslib::accordion()` - Collapsible sections
- `bslib::value_box()` - Display key metrics
- `bslib::nav_panel()` - Tab content

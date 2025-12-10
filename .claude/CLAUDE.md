# cardiometR Development Guidelines

## Package Overview
cardiometR is an R package for analyzing Cardiopulmonary Exercise Testing (CPET) data from COSMED Quark CPET metabolic carts, with S7 classes, a Shiny app, and bilingual PDF reports.

## R Package Conventions
- Use tidyverse style with modern pipe (`|>`, not `%>%`)
- All exported functions documented with roxygen2
- testthat for unit tests (edition 3)
- Follow rOpenSci package development standards
- Reference: [R Packages (2e)](https://r-pkgs.org/)

## S7 Class Patterns
- All CPET data structures use S7 classes from the S7 package
- Property access via `@` operator (e.g., `participant@name`)
- Include validators for required properties using `validator` argument
- Implement `print` methods for clean console output
- Use `class_character | NULL` pattern for optional properties
- Reference: [S7 Documentation](https://rconsortium.github.io/S7/)

## Code Organization
```
R/
├── classes.R           # All S7 class definitions
├── generics.R          # S7 generic function definitions
├── methods-average.R   # S7 methods for averaging
├── methods-validate.R  # S7 methods for validation
├── methods-thresholds.R # S7 methods for threshold detection
├── methods-print.R     # S7 print methods
├── import.R            # read_cosmed(), read_cpet()
├── process.R           # Stage extraction, peak determination
├── plot.R              # ggplot2 visualization functions
├── table.R             # gt table generation
├── report.R            # render_report() with typr
├── typst-helpers.R     # Typst content generation
├── i18n.R              # Translation helpers (tr())
├── run_app.R           # Shiny app launcher
├── app_ui.R            # Main Shiny UI
├── app_server.R        # Main Shiny server
├── mod_*.R             # Shiny modules
└── utils.R             # Utility functions
```

## Bilingual Support (EN/FR)
- All user-facing text via `tr(key, language)` helper function
- Labels stored in `inst/translations/labels_{en,fr}.yml`
- Reports support "en" and "fr" languages via ReportConfig class
- Shiny app has language toggle

## CPET Analysis Standards
- Follow ATS/ACCP Guidelines (DOI: 10.1164/rccm.167.2.211)
- Peak values: 30-second rolling average by default
- Threshold methods: V-slope, VE/VO2, VE/VCO2, end-tidal gases
- Physiological validation ranges per clinical guidelines

## Testing Guidelines
- Test S7 objects with `expect_s7_class()` or check `inherits(x, "ClassName")`
- Mock CPET data with realistic physiological values:
  - Resting VO2: 150-400 mL/min
  - Resting RER: 0.70-0.90
  - Max HR: 60-220 bpm
  - Max RER: 0.70-1.30
- Test edge cases: missing HR, aberrant breaths, incomplete stages

## Dependencies
- **Core**: S7, dplyr, tidyr, purrr, readxl, ggplot2, gt, scales, zoo
- **CLI/Utils**: cli, rlang, glue, yaml
- **Reports**: typr (Typst compilation, no Quarto)
- **Shiny**: shiny (>= 1.7.0), bslib, DT
- **Suggested**: testthat, shinytest2, knitr, rmarkdown, plotly, patchwork

## Key References
- [spiro package](https://docs.ropensci.org/spiro/) - rOpenSci CPET package
- [whippr package](https://fmmattioni.github.io/whippr/) - VO2 kinetics
- [typr package](https://cran.r-project.org/web/packages/typr/) - Typst in R
- [Typst docs](https://typst.app/docs/) - PDF template language

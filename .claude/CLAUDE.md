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
├── classes.R                 # All S7 class definitions
├── generics.R                # S7 generic function definitions
├── analyze.R                 # analyze_cpet() orchestration
├── methods-*.R               # S7 methods (average, peaks, thresholds, …)
├── import*.R                 # Dialect-based import engine
├── plots.R                   # ggplot2 visualization functions
├── report.R                  # generate_report() public API
├── report-data.R             # Template data builders
├── report-graphs.R           # Report graph export and cache
├── report-interpretation.R   # Auto / visual interpretation
├── report-typst.R            # Typst render helpers
├── i18n.R                    # Translation helpers (tr())
├── run_app.R / app_*.R       # Shiny launcher and shell
├── mod_*.R                   # Shiny modules
└── utils.R                   # Shared helpers (filter_exercise_data, …)
```

## Bilingual Support (EN/FR)
- All user-facing text via `tr(key, language)` helper function
- Labels stored in `inst/translations/labels_{en,fr}.yml`
- Reports support "en" and "fr" languages via ReportConfig class
- Shiny app has language toggle

## CPET Analysis Standards
- Follow ATS/ACCP Guidelines (DOI: 10.1164/rccm.167.2.211)
- Peak values: 30-second rolling average on exercise breaths only
- Threshold methods: V-slope (slope-increase constraint), VE/VO2 dual condition when possible, VE/VCO2, end-tidal gases
- Normative z-scores may use estimated SDs; surface `sd_source` to users
- Physiological validation ranges per clinical guidelines

## Testing Guidelines
- Test S7 objects with `expect_s7_class()` or check `inherits(x, "S7_object")`
- Mock CPET data with realistic physiological values:
  - Resting VO2: 150-400 mL/min
  - Resting RER: 0.70-0.90
  - Max HR: 60-220 bpm
  - Max RER: 0.70-1.30
- Test edge cases: missing HR, aberrant breaths, incomplete stages, recovery spikes

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

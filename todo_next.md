# cardiometR - Next Steps

## Completed

- [x] S7 class infrastructure (Participant, CpetData, CpetAnalysis, etc.)
- [x] COSMED file import (`read_cpet()`, `read_cosmed()`)
- [x] Data validation (`validate()`)
- [x] Peak value detection (`find_peaks()`)
- [x] Threshold detection (`detect_thresholds()`)
- [x] Stage extraction and summarization
- [x] 9-panel CPET plots with stage averages
- [x] Normative data for athletes (cycling, running, triathlon)
- [x] Bilingual support (EN/FR) with `tr()` helper
- [x] PDF report generation with Typst
- [x] Shiny app with bslib components
- [x] Custom Claude Code agents (slash commands, skills)

---

## Priority 1: Package Polish

### Documentation
- [ ] Write package vignette (`vignettes/introduction.Rmd`)
- [ ] Add usage examples to function documentation
- [ ] Create pkgdown site for GitHub Pages

### Testing
- [ ] Add Shiny module tests with `shinytest2`
- [ ] Increase test coverage for edge cases
- [ ] Add integration tests for full workflow

### Code Quality
- [ ] Fix roxygen warnings (missing `@name` in methods files)
- [ ] Run `lintr` and fix style issues
- [ ] Add type checking with `checkmate` or assertions

---

## Priority 2: Additional Features

### Data Import
- [ ] Support Parvo TrueOne format
- [ ] Support Cortex MetaLyzer format
- [ ] Support generic CSV with column mapping
- [ ] Auto-detect file format from content

### Analysis
- [ ] VO2 kinetics analysis (on-kinetics, off-kinetics)
- [ ] Oxygen uptake efficiency slope (OUES)
- [ ] Ventilatory efficiency (VE/VCO2 slope)
- [ ] Exercise oscillatory ventilation detection
- [ ] Fat oxidation rates (FATmax)

### Normative Data
- [ ] Add sedentary population norms
- [ ] Add clinical population norms (heart failure, COPD)
- [ ] Age-specific percentile calculations
- [ ] Sex-specific reference equations

### Visualization
- [ ] Interactive plots with `plotly` in Shiny app
- [ ] Customizable plot themes
- [ ] Export plots as SVG/PDF
- [ ] Comparison overlay (multiple tests)

---

## Priority 3: Shiny App Enhancements

### UX Improvements
- [ ] Progress indicators during analysis
- [ ] Keyboard shortcuts
- [ ] Responsive design for tablets
- [ ] Dark mode theme option

### Data Management
- [ ] Save/load analysis sessions
- [ ] Batch processing multiple files
- [ ] Export results to Excel
- [ ] Database integration (SQLite/PostgreSQL)

### Collaboration
- [ ] Share reports via URL
- [ ] Multi-user support
- [ ] Audit trail for clinical use

---

## Priority 4: Clinical Integration

### Standards Compliance
- [ ] ATS/ACCP guideline checklist in reports
- [ ] HIPAA-compliant data handling
- [ ] HL7 FHIR export for EHR integration

### Interpretation
- [ ] AI-assisted interpretation suggestions
- [ ] Differential diagnosis support
- [ ] Risk stratification algorithms

---

## Priority 5: Distribution

### CRAN Submission
- [ ] Pass `R CMD check --as-cran` with 0 errors/warnings/notes
- [ ] Write CRAN submission comments
- [ ] Prepare NEWS.md for release

### Alternative Distribution
- [ ] Docker container for Shiny app
- [ ] ShinyApps.io deployment guide
- [ ] Posit Connect deployment guide

---

## Technical Debt

- [ ] Refactor `report.R` - split into smaller files
- [ ] Optimize `plot_cpet_panel()` for large datasets
- [ ] Cache translation files in Shiny app
- [ ] Add logging with `logger` package
- [ ] Implement proper error classes with `rlang`

---

## Ideas for Future Versions

- Machine learning for threshold detection
- Wearable device data integration (Garmin, Wahoo)
- Training zone recommendations
- Longitudinal tracking across multiple tests
- API for external applications

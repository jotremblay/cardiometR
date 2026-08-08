# cardiometR - Next Steps

## Completed (through v0.6.x roadmap)

- [x] S7 class infrastructure
- [x] Dialect-based COSMED / CSV import with import report
- [x] Peak, threshold, stage, quality, economy, norms
- [x] Bilingual Shiny app + Typst PDF reports
- [x] Quarto vignettes + pkgdown site
- [x] Clinical safety: exercise-only peaks/VT, SD caveats, economy steady-state gate, missing-graph banner
- [x] Exported `analyze_cpet()` orchestration
- [x] Split `report.R` into focused report-* files

---

## Priority now: Package polish / CRAN path

- [ ] Pass `R CMD check --as-cran` with 0 errors/warnings/notes
- [ ] Add usage examples to remaining function docs
- [ ] Increase edge-case coverage beyond current golden/integration suite

---

## Next: Shiny UX

- [x] Progress indicators + non-silent metric failures
- [x] Local session save/load
- [x] Stronger shinytest2 / testServer flows

---

## Deferred features

### Data Import
- [ ] Parvo TrueOne / Cortex MetaLyzer dialect YAML
- [ ] Richer interactive column mapping UI

### Analysis
- [ ] VO2 kinetics, OUES, VE/VCO2 slope as first-class metrics
- [ ] Exercise oscillatory ventilation detection
- [ ] Fat oxidation rates (FATmax)
- [ ] Age-specific sport norms with tabulated SDs

### Distribution / clinical
- [ ] Docker / ShinyApps.io / Posit Connect guides
- [ ] HIPAA / FHIR / multi-user (not near-term)

---

## Technical debt remaining

- [ ] Optimize `plot_cpet_panel()` for large datasets
- [ ] Add structured logging with `logger`
- [ ] Typed error classes with `rlang`

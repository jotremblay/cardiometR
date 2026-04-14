# cardiometR (development version)

# cardiometR 0.6.0

* Added `NEWS.md`, `CONTRIBUTING.md`, and R-CMD-check GitHub Action.
* New vignettes: `s7-classes`, `reports`, `shiny-app`.
* Added `@examples` to the individual plot functions
  (`plot_v_slope`, `plot_ventilatory_equivalents`, `plot_gas_exchange`,
  `plot_heart_rate`, `plot_power`).
* Shiny: guard `mod_participant` save-edits with `req(cpet_data())`,
  route error toast through `tr("error_label")`, wrap hardcoded
  "Info" / "Breaths" via `tr()`, switch settings-badge tracking from
  `observe()` to `observeEvent()`.
* Documentation: fixed pkgdown URL, removed broken navbar link to
  non-existent visualization vignette, added `Language: en-US` to
  `DESCRIPTION`.

# cardiometR 0.1.0

Initial public release.

## Features

* COSMED Quark CPET Excel import via `read_cpet()` / `read_cosmed()`.
* S7 class hierarchy for type-safe CPET data: `Participant`, `CpetMetadata`,
  `CpetData`, `PeakValues`, `Thresholds`, `CpetAnalysis`, `ReportConfig`, and
  quality classes (`ExerciseQualityCriteria`, `ProtocolQuality`,
  `DataQualityReport`, `QualityAssessment`).
* Peak detection via 30 s rolling average per ATS/ACCP guidelines
  (`find_peaks()`).
* Ventilatory threshold methods: V-slope, VE/VO2, VE/VCO2, end-tidal gases
  (`detect_thresholds()`).
* Test quality assessment (`assess_quality()` and sub-assessments).
* Nine-panel clinical CPET plot (`plot_cpet_panel()`), plus individual
  plots and normative-comparison overlays.
* Bilingual (EN/FR) Typst PDF report via `generate_report()` /
  `ReportConfig`.
* Interactive Shiny application with EN/FR toggle (`run_app()`).
* Normative comparisons against FRIEND (general population) and CHEER
  (endurance athlete) registries.

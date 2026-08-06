# cardiometR (development version)

## Import rewritten to read files in any language

The importer no longer assumes an English COSMED export with one fixed layout.
It works out what it is reading from the file's contents, so exports in French,
and exports configured with different columns, now import correctly.

* `read_cpet()` gains `sheet`, `mapping`, `units`, and `quiet`. It prints a
  summary of what it recognised, what it converted, and what it left out.
* New `preview_cpet_columns()` shows how a file's columns will be read without
  importing it, and suggests a name for anything it does not recognise.
* New `list_cpet_dialects()` lists the formats available.
* Formats are described by YAML files under `inst/dialects`. Support for
  another cart, or for a local variation of an existing one, needs a new file
  rather than a change to the package. Files placed in the user's own
  configuration directory are picked up automatically.
* Column headers are matched in English and French at the same time. The
  language of a file is a property of the file, not of the interface language,
  so a French export opened in an English session reads correctly.
* Participant and test details are found by reading the label next to each
  value rather than by row number. A French Omnia export puts the protocol on
  row 13 and the ergometer on row 14, where the English export has them on
  rows 8 and 7, so fixed positions returned the wrong cells entirely.
* Units are read and converted to what the analysis expects. A cart reporting
  VO2 in L/min, or speed in m/s, is now handled instead of producing results
  that are wrong by a factor of a thousand while still looking plausible.
* Phase labels are translated to `rest`, `warmup`, `exercise` and `recovery`
  whatever the file's language. Previously a French file matched none of the
  English phase names, and every breath was silently treated as exercise,
  corrupting the stage table, the resting values, and threshold detection.
* `read_cosmed()` finds the data sheet whatever the export language calls it.
  Its `sheet` default changes from `"Data"` to `NULL`. The old default made
  `readxl` error outright on a French export.
* Time written as `hh:mm:ss` is parsed. `as.numeric()` returned `NA` for it,
  which emptied the whole table and then reported a missing column.

## Analysis and display fixes

* Analysis settings are no longer silently reset. Every `updateSelectInput()`
  in the settings module passed new `choices` without `selected`, and Shiny
  falls back to the first choice when told to do that. The observer that
  re-labels the dropdowns runs once at start-up, so before a user touched
  anything the averaging method had already flipped from rolling to time, and
  the athlete level from recreational to elite. The level list was also
  declared in a different order there, starting with elite, which is where the
  reset landed. Every participant was therefore compared against elite
  normative data, and switching language discarded any choice made since.
* Peak power output now gets its own normative comparison. It borrowed the
  z-score of maximal aerobic power, which is the same number only when the
  participant completes the final stage. When a test ends part way through a
  stage, as maximal tests usually do, the two diverge.
* The report preview names the sex rather than printing the stored code, so it
  reads "Male" or "Homme" instead of "M". The PDF was already correct.

* Resting values are now taken from the rest phase. `compute_resting_values()`
  selected breaths tagged `stage == 0`, but `extract_stages()` gives stage 0 to
  rest, warmup and recovery alike, so averaging the last minute of the leading
  block landed at the end of the warmup. On the bundled example that reported a
  resting VO2 of 2672 mL/min and a heart rate of 124 bpm, against true values of
  659 mL/min and 96 bpm. The reported duration was wrong the same way, covering
  the whole pre-exercise period rather than the rest block. Files with no phase
  column still fall back to the previous behaviour.
* Age is shown in whole years. COSMED stores it as a fraction, so a real export
  reads 31.2902181427408. That reached the participant panel, where it broke the
  layout, and the report. It also made `generate_report()` fail outright, since
  the predicted-values note formatted age with `%d`, which is an error in R for
  anything but a whole number. The precise value is unchanged on the object,
  where prediction equations and normative lookups still use it.

## Import fixes

* The test date is read from the file. It previously fell back to the current
  date without warning whenever the date arrived as an Excel serial, which was
  always, so every imported test was dated the day it was opened.
* The date of birth is read rather than discarded.
* Speed is imported. The column mapping looked for a column called `Speed`,
  which no COSMED export contains; the real one is `mark Speed`, in m/s.
  Treadmill running economy therefore never had a data source.
* Optional columns that are entirely missing or entirely zero are dropped at
  import. A cycle test carries an all-zero speed channel, and code that tested
  only whether the column existed read that as a treadmill test.
* `FeO2` and `FeCO2` survive import. They were renamed by the column mapping
  and then dropped by a whitelist immediately below it.
* `CpetMetadata` gains `modality`, read from the protocol and ergometer text in
  either language, so a protocol named in French is understood.

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

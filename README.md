# cardiometR

<!-- badges: start -->
[![R-CMD-check](https://github.com/jotremblay/cardiometR/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/jotremblay/cardiometR/actions/workflows/R-CMD-check.yaml)
[![Lifecycle: experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT)
<!-- badges: end -->

Analyze **Cardiopulmonary Exercise Testing (CPET)** data from COSMED Quark metabolic carts in R — type-safe S7 data structures, ATS/ACCP-based peak and threshold detection, a Shiny clinical interface, and bilingual (EN/FR) Typst PDF reports.

## Features

- Import COSMED Quark Excel exports, delimited files, and other carts
  through a described dialect; every import carries a report of how the
  file was read
- S7 classes with built-in physiological validation
- Peak detection via rolling averages (ATS/ACCP), plus resting values
  taken from the rest block
- Ventilatory threshold methods: V-slope, VE/VO2, VE/VCO2, end-tidal
  gases, with a consensus estimate and a plausible range
- Threshold review in the app: accept the proposal, correct it by hand,
  or compare the methods
- Test-quality and maximal-effort assessment against ACSM criteria
- Nine-panel clinical CPET plot
- Normative comparison against athlete and general-population registries
- Movement economy, power metrics, and longitudinal comparison
- Interactive Shiny app with EN/FR toggle
- Typst-based clinical PDF reports (bilingual)

## Installation

**1. Install the package.** Every package the app needs comes with it.

```r
# install.packages("pak")
pak::pak("jotremblay/cardiometR")
```

**2. Install Quarto.** It bundles Typst, which renders the PDF reports.
Download it from [quarto.org](https://quarto.org/docs/get-started/). The
app runs without it, but the PDF button stays disabled.

**3. Check the machine.** This says what is present and what to fix.

```r
cardiometR::check_setup()
```

**4. Start the app.**

```r
cardiometR::run_app()
```

There is a sample COSMED export in the package, so you can try the app
before you have data of your own:

```r
system.file("extdata", "example_cosmed.xlsx", package = "cardiometR")
```

## Quick Start

```r
library(cardiometR)

# Import COSMED file
data <- read_cpet("path/to/cosmed_export.xlsx")

# Validate and analyze
validation <- validate(data)
averaged <- average(data, method = "rolling", window = 30)
peaks <- find_peaks(averaged)

# Create analysis object
analysis <- CpetAnalysis(
  data = averaged,
  peaks = peaks,
  validation = validation
)

# Generate 9-panel CPET plot
plot_cpet_panel(analysis)

# Launch interactive app
run_app()
```

## S7 Class Structure

| Class | Description |
|-------|-------------|
| `Participant` | Patient demographics (ID, name, age, sex, height, weight) |
| `CpetMetadata` | Test information (date, device, protocol, conditions) |
| `CpetData` | Breath-by-breath data container |
| `PeakValues` | Maximal values (VO2peak, HRmax, VEmax, RERmax) |
| `Thresholds` | Ventilatory thresholds (VT1, VT2) |
| `CpetAnalysis` | Complete analysis combining all components |
| `ReportConfig` | PDF report configuration |

Access properties with the `@` operator:

```r
# View participant info
data@participant@age
data@participant@weight_kg

# View peak values
peaks@vo2_peak       # mL/min
peaks@vo2_kg_peak    # mL/kg/min
peaks@hr_peak        # bpm
```

## Visualization

The 9-panel CPET display follows clinical standards:

```r
# English labels
plot_cpet_panel(analysis, language = "en")

# French labels
plot_cpet_panel(analysis, language = "fr")
```

Individual plots available:

- `plot_v_slope()` - V-slope for threshold detection
- `plot_ventilatory_equivalents()` - VE/VO2 and VE/VCO2
- `plot_heart_rate()` - HR response with zones
- `plot_power()` - Power output with VO2 overlay

## PDF Reports

Generate professional bilingual reports:
```r
config <- ReportConfig(
  language = "fr",
  institution = "Centre ÉPIC",
  technician = "Dr. Smith"
)

generate_report(
  analysis = analysis,
  output_file = "cpet_report.pdf",
  config = config
)
```

## Interactive Application

Launch the Shiny app for clinical use:

```r
run_app()                 # French interface (default)
run_app(language = "en")  # English interface
```

The app has five steps, one per tab:

1. **Téléverser / Upload** — drop a COSMED file. The validation card
   reports errors, warnings and how the file was read.
2. **Configurer / Configure** — correct the participant fields and set
   the analysis. Every accordion header summarises what is set inside it.
3. **Résultats / Results** — peak values, resting values, thresholds,
   the nine-panel plot, and normative z-scores.
4. **Qualité / Quality** — ACSM maximal-effort criteria, protocol and
   data quality, stage-by-stage deviations.
5. **Rapport / Report** — configure the header, read the preview, and
   render the PDF.

**Reviewing the thresholds.** The Results tab proposes thresholds and
lets the operator correct them. Three modes: read the automatic
consensus, move VT1 and VT2 by hand, or compare the detection methods
side by side and keep one. A manual correction re-reads heart rate and
power at the new points, and the report prints it as the detection
method.

## Quality Assessment

Assess test quality based on ACSM guidelines:

```r
quality <- assess_quality(data, rpe = 18, lactate = 9.5)

# Check maximal effort criteria
quality@exercise_criteria@determination
#> "maximal"

# Overall quality grade
quality@overall_grade
#> "A"
```

## Documentation

- **Vignettes**: `introduction`, `s7-classes`, `shiny-app`, `reports`
  — open one with `vignette("introduction", package = "cardiometR")`
- **Function reference**: see `?cardiometR` for package help
- **Setup check**: `check_setup()` reports what is missing on a machine
- **Translations**: `inst/translations/labels_{en,fr}.yml`

## References

- ATS/ACCP Statement on Cardiopulmonary Exercise Testing. *Am J Respir Crit Care Med*. 2003;167(2):211-277.

## Related Packages

- [spiro](https://docs.ropensci.org/spiro/) - rOpenSci CPET package
- [whippr](https://fmmattioni.github.io/whippr/) - VO2 kinetics analysis

## License

MIT License. See [LICENSE](LICENSE) for details.

## Citation

```
Tremblay J (2026). cardiometR: Cardiopulmonary Exercise Testing Analysis with S7 Classes.
R package version 0.6.0. https://github.com/jotremblay/cardiometR
```

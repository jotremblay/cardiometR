# cardiometR

<!-- badges: start -->
[![R-CMD-check](https://github.com/jotremblay/cardiometR/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/jotremblay/cardiometR/actions/workflows/R-CMD-check.yaml)
[![Lifecycle: experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT)
<!-- badges: end -->

Analyze **Cardiopulmonary Exercise Testing (CPET)** data from COSMED Quark metabolic carts in R — type-safe S7 data structures, ATS/ACCP-based peak and threshold detection, a Shiny clinical interface, and bilingual (EN/FR) Typst PDF reports.

## Features

- Import COSMED Quark Excel exports with automatic parsing
- S7 classes with built-in physiological validation
- Peak detection via rolling averages (ATS/ACCP)
- Ventilatory threshold methods: V-slope, VE/VO2, VE/VCO2, end-tidal gases
- Test-quality and maximal-effort assessment
- Nine-panel clinical CPET plot
- Interactive Shiny app with EN/FR toggle
- Typst-based PDF reports (bilingual)
- Normative comparison against athlete and general-population registries

## Installation

```r
# Install from GitHub
# install.packages("pak")
pak::pak("jotremblay/cardiometR")
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
run_app()           # English interface
run_app(language = "fr")  # French interface
```

The app provides:
- Drag-and-drop file import
- Participant info editing
- Interactive visualization
- Quality assessment
- PDF report generation

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

- **Vignette**: `vignette("introduction", package = "cardiometR")`
- **Function reference**: See `?cardiometR` for package help
- **Translations**: `inst/translations/labels_{en,fr}.yml`

## References

- ATS/ACCP Statement on Cardiopulmonary Exercise Testing. *Am J Respir Crit Care Med*. 2003;167(2):211-277.
- Beaver WL, Wasserman K, Whipp BJ. A new method for detecting anaerobic threshold by gas exchange. *J Appl Physiol*. 1986;60(6):2020-2027.

## Related Packages

- [spiro](https://docs.ropensci.org/spiro/) - rOpenSci CPET package
- [whippr](https://fmmattioni.github.io/whippr/) - VO2 kinetics analysis

## License

MIT License. See [LICENSE](LICENSE) for details.

## Citation

```
Tremblay J (2026). cardiometR: Cardiopulmonary Exercise Testing Analysis with S7 Classes.
R package version 0.1.0. https://github.com/jotremblay/cardiometR
```

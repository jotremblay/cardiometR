---
description: Add bilingual label to translation files (English and French)
argument-hint: [key] [english-value] [french-value]
allowed-tools: Read, Edit
---

# Internationalization Label Manager

Add or update a translation label in both English and French YAML files.

## Label to Add/Update
- **Key**: $1
- **English**: $2
- **French**: $3

## Instructions

1. Read `inst/translations/labels_en.yml`
2. Read `inst/translations/labels_fr.yml`
3. Add the new key-value pair to both files
4. Maintain alphabetical order if possible

## File Locations

- English: `inst/translations/labels_en.yml`
- French: `inst/translations/labels_fr.yml`

## YAML Format

```yaml
# labels_en.yml
$1: "$2"

# labels_fr.yml
$1: "$3"
```

## Common Label Categories

### App UI Labels
```yaml
app_title: "CPET Analysis"
upload_prompt: "Drop COSMED xlsx file here"
generate_report: "Generate PDF Report"
language_toggle: "Language"
```

### Participant Labels
```yaml
participant_name: "Name"
participant_age: "Age (years)"
participant_sex: "Sex"
participant_height: "Height (cm)"
participant_weight: "Weight (kg)"
```

### Report Labels
```yaml
report_title: "CPET Analysis Report"
stage_results: "Stage Results"
peak_values: "Peak Values"
threshold_results: "Ventilatory Thresholds"
```

### Units
```yaml
unit_ml_min: "mL/min"
unit_ml_kg_min: "mL/kg/min"
unit_l_min: "L/min"
unit_bpm: "bpm"
unit_watts: "W"
```

### Validation Messages
```yaml
error_missing_file: "Please upload a file"
error_invalid_format: "Invalid file format"
warning_missing_hr: "Heart rate data missing"
```

## After Adding

The `tr(key, language)` function in `R/i18n.R` will automatically pick up the new labels.

Usage in R:
```r
tr("$1", "en")  # Returns: "$2"
tr("$1", "fr")  # Returns: "$3"
```

Usage in Shiny:
```r
shiny::textOutput(tr("$1", language()))
```

---
name: cpet-analysis
description: CPET domain knowledge for cardiometR. Use when implementing CPET analysis functions, threshold detection, peak determination, data validation, or any physiological calculations. Includes ATS/ACCP guidelines and reference values.
allowed-tools: Read, Write, Edit, Glob
---

# CPET Analysis Domain Knowledge

## When to Use This Skill

- Implementing CPET data processing algorithms
- Detecting ventilatory thresholds (VT1/VT2)
- Determining peak/max values with appropriate averaging
- Validating physiological plausibility of data
- Implementing quality control checks
- Understanding CPET variables and their relationships

## Key CPET Variables

| Variable | Units | Description |
|----------|-------|-------------|
| VO2 | mL/min | Oxygen consumption |
| VO2/kg | mL/kg/min | Weight-relative VO2 |
| VCO2 | mL/min | Carbon dioxide production |
| VE | L/min | Minute ventilation |
| RER | ratio | Respiratory exchange ratio (VCO2/VO2) |
| HR | bpm | Heart rate |
| VE/VO2 | ratio | Ventilatory equivalent for O2 |
| VE/VCO2 | ratio | Ventilatory equivalent for CO2 |
| PETO2 | mmHg | End-tidal O2 partial pressure |
| PETCO2 | mmHg | End-tidal CO2 partial pressure |
| BF | breaths/min | Breathing frequency |
| VT | L | Tidal volume |

## Reference Guidelines

See [CPET-GUIDELINES.md](CPET-GUIDELINES.md) for detailed information on:
- ATS/ACCP Statement guidelines
- Physiological reference ranges
- Threshold detection methods
- Peak value determination protocols
- Data quality criteria

## Quick Reference: Physiological Ranges

### Resting Values
- VO2: 3.5 mL/kg/min (approximately 250 mL/min for 70kg person)
- RER: 0.70-0.90
- HR: 60-100 bpm
- VE: 6-8 L/min

### Maximal Exercise
- Peak RER: > 1.10 (indicates maximal effort)
- Peak HR: ~220 - age (± 10-15 bpm)
- Peak VE: can exceed 150 L/min in athletes

### Threshold Detection
- VT1 (aerobic threshold): typically 50-60% VO2max
- VT2 (respiratory compensation): typically 70-80% VO2max

## Files in cardiometR

- `R/process.R` - Peak determination, stage extraction
- `R/methods-validate.R` - Data quality validation
- `R/methods-thresholds.R` - Threshold detection algorithms

# CPET Clinical Guidelines Reference

## ATS/ACCP Statement on Cardiopulmonary Exercise Testing

Reference: Am J Respir Crit Care Med. 2003;167:211-277
DOI: 10.1164/rccm.167.2.211

### Test Quality Criteria

#### Pre-test Requirements
- Equipment calibration within 24 hours
- Volume calibration: ± 3% accuracy
- Gas analyzer calibration: ± 0.03% O2, ± 0.02% CO2
- Document ambient conditions (temperature, humidity, pressure)

#### During Test
- Minimum 3-minute rest period before exercise
- Consistent mouthpiece/mask seal
- No talking during measurements
- Continuous monitoring of ECG, SpO2

#### Data Quality Flags
- Aberrant breaths (coughs, swallows, sighs)
- Equipment artifacts
- Movement artifacts
- Loss of seal

---

## Physiological Reference Ranges

### Resting Phase (should observe)
| Parameter | Expected Range | Flag if Outside |
|-----------|---------------|-----------------|
| VO2 | 200-400 mL/min | < 150 or > 500 |
| RER | 0.70-0.90 | < 0.65 or > 1.00 |
| HR | 50-100 bpm | < 40 or > 120 |
| VE | 5-12 L/min | < 4 or > 20 |
| PETCO2 | 36-42 mmHg | < 30 or > 48 |

### Maximal Exercise Indicators
| Criterion | Value | Interpretation |
|-----------|-------|----------------|
| RER peak | > 1.10 | Strong evidence of max effort |
| RER peak | > 1.15 | Very strong evidence |
| HR peak | > 85% predicted | Supports max effort |
| RPE | ≥ 17 (6-20 scale) | Subjective max |
| Lactate | > 8 mmol/L | Metabolic max |

### Predicted Peak HR
```
Predicted HRmax = 220 - age (±10-15 bpm)
Alternative: 208 - (0.7 × age)
```

### Predicted Peak VO2 (mL/kg/min)
General population equations (vary by age, sex, activity level):
- Sedentary males: 42 - (0.35 × age)
- Sedentary females: 36 - (0.30 × age)
- Active individuals: Add 10-20%

---

## Data Averaging Methods

### Peak VO2 Determination
From: Sports Medicine, 2023 (Systematic Review)

| Method | Description | Use Case |
|--------|-------------|----------|
| Highest single breath | Max of raw data | Not recommended (too variable) |
| 15-second average | Rolling mean | Quick tests |
| 30-second average | Rolling mean | Standard recommendation |
| 60-second average | Rolling mean | High variability data |
| 20-breath average | Fixed N breaths | Alternative to time |

**Recommendation**: Use 30-second rolling average as default for peak VO2.

### Stage Summarization
For step protocols (e.g., Bruce, Balke):
- **Last 30 seconds**: Most common, represents steady-state
- **Last 60 seconds**: More stable for short stages
- **Mean of stage**: Appropriate for ramp protocols

---

## Threshold Detection Methods

### VT1 (Ventilatory Threshold 1 / Aerobic Threshold)

#### V-slope Method (Beaver et al., 1986)
1. Plot VCO2 vs VO2
2. Identify breakpoint where slope increases
3. Before VT1: slope ≈ 1.0
4. After VT1: slope > 1.0

```r
# Implementation approach
# Fit two-segment linear regression
# Find breakpoint that minimizes total residual error
```

#### Ventilatory Equivalents Method
1. Plot VE/VO2 and VE/VCO2 vs VO2
2. VT1 = point where VE/VO2 starts to rise
3. VE/VCO2 should still be decreasing or flat at VT1

#### End-tidal Gas Method
1. Plot PETO2 and PETCO2 vs VO2
2. VT1 = point where PETO2 starts to rise
3. PETCO2 should still be rising or flat at VT1

### VT2 (Ventilatory Threshold 2 / Respiratory Compensation Point)

#### Ventilatory Equivalents Method
1. Continue from VT1 analysis
2. VT2 = point where VE/VCO2 starts to rise
3. Both VE/VO2 and VE/VCO2 rising after VT2

#### End-tidal Gas Method
1. VT2 = point where PETCO2 starts to decline
2. PETO2 continues rising

### Threshold Confidence Levels
Report confidence based on:
- **High**: Multiple methods agree within 5% VO2
- **Moderate**: Methods agree within 10% VO2
- **Low**: Methods disagree or unclear breakpoints
- **Unable**: Insufficient data quality or no clear breakpoint

---

## Data Validation Checks

### Physiological Plausibility
```r
# Flag if outside these ranges
validate_breath <- function(breath) {
  flags <- list()

  if (breath$vo2_ml < 100 || breath$vo2_ml > 7000) {
    flags$vo2 <- "VO2 outside physiological range"
  }

  if (breath$rer < 0.5 || breath$rer > 2.0) {
    flags$rer <- "RER outside physiological range"
  }

  if (!is.na(breath$hr_bpm)) {
    if (breath$hr_bpm < 30 || breath$hr_bpm > 250) {
      flags$hr <- "HR outside physiological range"
    }
  }

  flags
}
```

### Aberrant Breath Detection
Flag breaths that deviate > 3 SD from local mean (rolling window):
- Coughs: sudden spike in VE with low VT
- Swallows: missing or very small VT
- Sighs: isolated high VT with normal VE

### Test Termination Criteria
Early termination may indicate:
- Equipment failure
- Patient distress
- Arrhythmia
- Excessive blood pressure response
- Desaturation (SpO2 < 85%)

---

## COSMED Quark CPET Specifics

### Expected Column Names (xlsx export)
```
t (Time)
VO2 (mL/min or L/min)
VCO2 (mL/min or L/min)
VE (L/min)
RER or R
HR (bpm)
Power or W (Watts)
BF or Rf (breaths/min)
VT (L)
PETO2 (mmHg)
PETCO2 (mmHg)
```

### Unit Conversions
- If VO2 in L/min: multiply by 1000 for mL/min
- If VE in mL/min: divide by 1000 for L/min

### Metadata Location
- Patient info: typically in first few rows
- Calibration data: separate sheet or header section
- Protocol info: may be in filename or header

---
description: Implement S7 method for existing generic in cardiometR
argument-hint: [generic-name] [class-name]
allowed-tools: Read, Write, Edit, Glob
---

# S7 Method Implementation

Add an S7 method implementation for an existing generic.

## Method to Create
- **Generic**: $1
- **Class**: $2

## Instructions

1. Read `R/generics.R` to find the generic definition for `$1`
2. Read `R/classes.R` to understand the `$2` class structure
3. Check existing methods in `R/methods-*.R` files for patterns

4. Implement the method following this pattern:

```r
#' @rdname $1
#' @export
method($1, $2) <- function(x, ...) {
  # Implementation
  # Access properties via x@property_name
}
```

5. Add to appropriate file:
   - `R/methods-average.R` for averaging methods
   - `R/methods-validate.R` for validation methods
   - `R/methods-thresholds.R` for threshold detection
   - `R/methods-print.R` for print/format methods
   - Create new `R/methods-{generic}.R` if needed

## Common Patterns

### Returning modified S7 object
```r
method(average, CpetData) <- function(x, method = "time", window = 30) {
  averaged_breaths <- x@breaths |>
    # processing...

  CpetData(
    participant = x@participant,
    metadata = x@metadata,
    breaths = averaged_breaths,
    is_averaged = TRUE,
    averaging_window = window
  )
}
```

### Returning new S7 object
```r
method(find_peaks, CpetData) <- function(x, averaging = 30) {
  # Calculate peaks...

  PeakValues(
    vo2_peak = max_vo2,
    vo2_kg_peak = max_vo2_kg,
    # ...
  )
}
```

### Validation method pattern
```r
method(validate, CpetData) <- function(x, ...) {
  errors <- list()
  warnings <- list()

  # Check conditions...
  if (nrow(x@breaths) < 10) {
    errors <- c(errors, "Too few breaths recorded")
  }

  ValidationReport(
    is_valid = length(errors) == 0,
    errors = errors,
    warnings = warnings,
    info = list()
  )
}
```

---
description: Generate roxygen2 documentation and check for issues
allowed-tools: Bash, Read, Glob
---
# Documentation Generator

Run roxygen2 to generate documentation and check for issues.

## Instructions

1. Run `devtools::document()` to generate man pages and NAMESPACE
2. Check for documentation warnings
3. Report any missing or incomplete documentation

## Document Command

```bash
cd /Users/jtremblay/GitHub/cardiometR
Rscript -e "devtools::document()"
```

## After Running

Check for these common issues:

### Missing @param tags
Look for functions with undocumented parameters:
```
Warning: Undocumented arguments in 'function_name': param1, param2
```

### Missing @return tags
```
Warning: Missing return value documentation for 'function_name'
```

### Missing @export tags
Functions intended for users should have `@export`

### S7 Documentation Pattern

For S7 classes:
```r
#' CpetData Class
#'
#' @description
#' Container for CPET breath-by-breath data with participant and metadata.
#'
#' @param participant A Participant S7 object
#' @param metadata A CpetMetadata S7 object
#' @param breaths A tibble of breath-by-breath measurements
#' @param stages Optional stage annotations
#' @param is_averaged Logical, whether data has been averaged
#' @param averaging_window Numeric, window size if averaged
#'
#' @return A CpetData S7 object
#'
#' @examples
#' participant <- Participant(id = "001", name = "Test", age = 30,
#'                            sex = "M", height_cm = 175, weight_kg = 70)
#' metadata <- CpetMetadata(test_date = Sys.Date(), device = "COSMED",
#'                          protocol = "Incremental")
#' data <- CpetData(participant = participant, metadata = metadata,
#'                  breaths = breath_data, is_averaged = FALSE)
#'
#' @export
CpetData <- new_class(...)
```

For S7 methods:
```r
#' @rdname average
#' @export
method(average, CpetData) <- function(x, method = "time", window = 30) {
  ...
}
```

## Checklist After Documenting

- [ ] All exported functions have `@export`
- [ ] All parameters have `@param` descriptions
- [ ] All functions have `@return` descriptions
- [ ] S7 classes have complete slot documentation
- [ ] Examples run without errors (`devtools::run_examples()`)
- [ ] NAMESPACE is properly generated
- [ ] man/ files are created for all exports

## Quick Fixes

```r
# Check a specific file's documentation
roxygen2::roxygenise(files = "R/classes.R")

# Preview documentation
?function_name

# Check all examples run
devtools::run_examples()

# Spell check documentation
spelling::spell_check_package()
```

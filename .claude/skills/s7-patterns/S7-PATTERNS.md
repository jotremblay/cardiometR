# S7 Code Patterns for cardiometR

## Class Definition with Typed Properties

```r
library(S7)

#' Participant Class
#'
#' @description Demographics and physical characteristics
#' @export
Participant <- new_class("Participant",
  properties = list(
    # Required string properties
    id = class_character,
    name = class_character,

    # Required numeric properties
    age = class_numeric,
    height_cm = class_numeric,
    weight_kg = class_numeric,

    # Validated property with custom validator
    sex = new_property(class_character,
      validator = function(value) {
        if (!value %in% c("M", "F", "O")) {
          return("sex must be 'M', 'F', or 'O'")
        }
        NULL
      }
    ),

    # Optional property (nullable)
    sport = class_character | NULL
  )
)
```

## Property Validation Patterns

### Range validation
```r
age = new_property(class_numeric,
  validator = function(value) {
    if (value < 0 || value > 120) {
      return("age must be between 0 and 120")
    }
    NULL
  }
)
```

### Enum validation
```r
protocol = new_property(class_character,
  validator = function(value) {
    valid <- c("step", "ramp", "constant")
    if (!value %in% valid) {
      return(paste("protocol must be one of:", paste(valid, collapse = ", ")))
    }
    NULL
  }
)
```

### Length validation
```r
id = new_property(class_character,
  validator = function(value) {
    if (nchar(value) == 0) {
      return("id cannot be empty")
    }
    NULL
  }
)
```

## Cross-Property Validation

```r
CpetData <- new_class("CpetData",
  properties = list(
    breaths = class_data.frame,
    is_averaged = class_logical,
    averaging_window = class_numeric | NULL
  ),
  validator = function(self) {
    errors <- character()

    # Check required columns in breaths
    required_cols <- c("time_s", "vo2_ml", "vco2_ml", "ve_l", "rer")
    missing <- setdiff(required_cols, names(self@breaths))
    if (length(missing) > 0) {
      errors <- c(errors,
        paste("breaths missing columns:", paste(missing, collapse = ", "))
      )
    }

    # Check averaging consistency
    if (self@is_averaged && is.null(self@averaging_window)) {
      errors <- c(errors,
        "averaging_window required when is_averaged is TRUE"
      )
    }

    # Return NULL if valid, error message(s) if not
    if (length(errors) > 0) {
      return(paste(errors, collapse = "; "))
    }
    NULL
  }
)
```

## Generic Definition

```r
#' Average CPET Data
#'
#' @description Apply averaging to breath-by-breath data
#' @param x A CpetData object
#' @param method Averaging method: "time", "breath", or "rolling"
#' @param window Window size (seconds or breaths)
#' @return An averaged CpetData object
#' @export
average <- new_generic("average", "x")
```

## Method Implementation

```r
#' @rdname average
#' @export
method(average, CpetData) <- function(x, method = c("time", "breath", "rolling"),
                                       window = 30) {
  method <- match.arg(method)

  averaged_breaths <- switch(method,
    time = average_by_time(x@breaths, window),
    breath = average_by_breath(x@breaths, window),
    rolling = average_rolling(x@breaths, window)
  )

  # Return new CpetData with averaged data
  CpetData(
    participant = x@participant,
    metadata = x@metadata,
    breaths = averaged_breaths,
    stages = x@stages,
    is_averaged = TRUE,
    averaging_window = window
  )
}
```

## Print Method

```r
#' @export
method(print, CpetData) <- function(x, ...) {
  cli::cli_h1("CpetData")
  cli::cli_text("Participant: {x@participant@name} ({x@participant@id})")
  cli::cli_text("Test date: {x@metadata@test_date}")
  cli::cli_text("Device: {x@metadata@device}")
  cli::cli_text("Breaths: {nrow(x@breaths)}")
  cli::cli_text("Averaged: {x@is_averaged}")
  if (x@is_averaged) {
    cli::cli_text("Window: {x@averaging_window}s")
  }
  invisible(x)
}
```

## Nullable Properties Pattern

```r
# For optional properties, use union type
optional_field = class_character | NULL

# In constructor, NULL is the default
CpetMetadata <- new_class("CpetMetadata",
  properties = list(
    test_date = class_Date,
    device = class_character,
    # Optional fields
    calibration_date = class_Date | NULL,
    temperature_c = class_numeric | NULL,
    technician = class_character | NULL
  )
)

# Usage - optional fields can be omitted
meta <- CpetMetadata(
  test_date = Sys.Date(),
  device = "COSMED Quark"
  # calibration_date, temperature_c, technician default to NULL
)
```

## Composing Classes

```r
# CpetAnalysis composes multiple S7 objects
CpetAnalysis <- new_class("CpetAnalysis",
  properties = list(
    data = CpetData,           # Required S7 object
    peaks = PeakValues | NULL, # Optional S7 object
    thresholds = Thresholds | NULL,
    validation = ValidationReport | NULL
  )
)

# Access nested properties
analysis <- CpetAnalysis(data = cpet_data)
analysis@data@participant@name
analysis@data@breaths
```

## Converting to/from tibble

```r
# Extract data as tibble
as_tibble.CpetData <- function(x, ...) {
  x@breaths |>
    dplyr::mutate(
      participant_id = x@participant@id,
      test_date = x@metadata@test_date
    )
}

# For S7, register as method
method(tibble::as_tibble, CpetData) <- as_tibble.CpetData
```

## Testing S7 Objects

```r
test_that("CpetData validates required columns", {
  participant <- Participant(id = "001", name = "Test", age = 30,
                             sex = "M", height_cm = 175, weight_kg = 70)
  metadata <- CpetMetadata(test_date = Sys.Date(), device = "Test",
                           protocol = "step")

  # Valid data
  valid_breaths <- tibble::tibble(
    time_s = 1:10, vo2_ml = 300, vco2_ml = 250, ve_l = 15, rer = 0.83
  )
  expect_no_error(
    CpetData(participant = participant, metadata = metadata,
             breaths = valid_breaths, is_averaged = FALSE)
  )

  # Invalid data - missing column
  invalid_breaths <- tibble::tibble(time_s = 1:10, vo2_ml = 300)
  expect_error(
    CpetData(participant = participant, metadata = metadata,
             breaths = invalid_breaths, is_averaged = FALSE),
    "missing columns"
  )
})
```

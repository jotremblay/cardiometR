---
description: Generate S7 class definition for cardiometR CPET data structures
argument-hint: [ClassName] [brief description]
allowed-tools: Read, Write, Edit, Glob
---

# S7 Class Generator for cardiometR

Generate a new S7 class definition following cardiometR patterns.

## Class to Create
- **Name**: $1
- **Description**: $ARGUMENTS

## Instructions

1. First, read the existing classes in `R/classes.R` to understand the current patterns
2. Read `implementation_plan.md` sections on S7 classes (lines 56-175) for reference patterns

3. Generate the S7 class with:
   - Proper `new_class()` definition
   - Typed properties using `class_*` or `new_property()`
   - Optional properties use `class_type | NULL` pattern
   - Validators for required constraints
   - roxygen2 documentation block

4. Add the class to `R/classes.R` (create if doesn't exist)

5. Create a basic `print` method in `R/methods-print.R`

## S7 Pattern Template

```r
#' $1 Class
#'
#' @description
#' [Description of what this class represents]
#'
#' @param property1 Description
#' @param property2 Description
#'
#' @return A $1 S7 object
#' @export
$1 <- new_class("$1",
 properties = list(
    # Required properties
    id = class_character,

    # Optional properties
    notes = class_character | NULL,

    # Validated properties
    status = new_property(class_character,
      validator = function(value) {
        if (!value %in% c("valid", "invalid")) {
          return("status must be 'valid' or 'invalid'")
        }
        NULL
      }
    )
  ),
  validator = function(self) {
    # Cross-property validation
    NULL
  }
)
```

## After Creating

Suggest adding a print method and any relevant generics for this class.

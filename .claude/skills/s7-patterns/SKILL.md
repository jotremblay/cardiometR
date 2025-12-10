---
name: s7-patterns
description: S7 OOP patterns for cardiometR CPET data structures. Use when creating S7 classes, implementing methods, designing object hierarchies, or refactoring code to use S7 for type-safe CPET analysis.
allowed-tools: Read, Write, Edit, Glob
---

# S7 Development Patterns for cardiometR

## When to Use This Skill

- Creating new S7 classes for CPET data structures
- Implementing S7 generics and methods
- Designing class inheritance hierarchies
- Adding property validation to classes
- Refactoring existing code to use S7

## cardiometR Class Hierarchy

```
Participant        # Demographics
CpetMetadata       # Test conditions
    │
    ├── CpetData   # Main container (has Participant + CpetMetadata)
    │       │
    │       ├── averaged via average() generic
    │       └── validated via validate() generic
    │
    ├── PeakValues     # Peak/max results
    ├── Thresholds     # VT1/VT2 results
    └── ValidationReport
            │
            └── CpetAnalysis  # Complete analysis (has all above)

ReportConfig       # Report generation settings
```

## Key Patterns

See [S7-PATTERNS.md](S7-PATTERNS.md) for detailed code patterns including:
- Class definitions with typed properties
- Property validators
- Cross-property validation
- Generic and method definitions
- Print method implementation
- Nullable properties pattern

## Quick Reference

### Create a class
```r
ClassName <- new_class("ClassName",
  properties = list(
    required_prop = class_character,
    optional_prop = class_numeric | NULL
  )
)
```

### Create a generic
```r
my_generic <- new_generic("my_generic", "x")
```

### Create a method
```r
method(my_generic, ClassName) <- function(x, ...) {
  # Implementation
}
```

### Access properties
```r
object@property_name
```

## Files in cardiometR

- `R/classes.R` - All class definitions
- `R/generics.R` - All generic definitions
- `R/methods-*.R` - Method implementations by category

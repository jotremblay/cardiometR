---
description: Run R CMD check on cardiometR package and summarize results
allowed-tools: Bash, Read
---

# Package Check Runner

Run R CMD check on cardiometR and provide a summary of issues.

## Instructions

1. Run `devtools::check()` to perform R CMD check
2. Parse the output for errors, warnings, and notes
3. Summarize issues with file locations and suggested fixes

## Check Command

```r
# Run in R console or via Rscript
devtools::check(document = TRUE, cran = FALSE)
```

Or via command line:
```bash
cd /Users/jtremblay/GitHub/cardiometR
Rscript -e "devtools::check(document = TRUE)"
```

## After Running

Summarize results in this format:

### Errors (must fix)
- [ ] Error description - `file.R:line`

### Warnings (should fix)
- [ ] Warning description - `file.R:line`

### Notes (consider fixing)
- [ ] Note description - `file.R:line`

## Common Issues and Fixes

### Missing documentation
```
Warning: Undocumented arguments in documentation object 'function_name'
```
Fix: Add `@param` tags to roxygen2 comments

### Missing exports
```
Warning: Object 'function_name' not found
```
Fix: Add `@export` tag or check NAMESPACE

### S7 class issues
```
Note: Undefined global functions or variables: new_class, method
```
Fix: Add `@importFrom S7 new_class method` or use `S7::` prefix

### Namespace conflicts
```
Warning: replacing previous import
```
Fix: Use explicit imports with `@importFrom pkg fun`

### Missing dependencies
```
Error: there is no package called 'xyz'
```
Fix: Add to Imports or Suggests in DESCRIPTION

## Quick Fix Commands

```r
# Regenerate documentation
devtools::document()

# Check specific file for issues
lintr::lint("R/problem_file.R")

# Run tests only
devtools::test()

# Build and check as CRAN would
devtools::check(cran = TRUE)
```

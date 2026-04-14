# Contributing to cardiometR

Thanks for considering a contribution. This guide covers the basics.

## Development setup

```r
# install.packages("devtools")
devtools::load_all()
devtools::test()
devtools::check()    # must be 0 errors, 0 warnings, 0 notes
devtools::document() # after changing roxygen
```

## Code style

* Tidyverse style with the modern pipe (`|>`), not `%>%`.
* Use `cli` for user-facing messages; no `message()`/`warning()`/`stop()`
  directly.
* Keep non-ASCII out of R source — use `\uXXXX` escapes in strings; keep
  roxygen comments ASCII (transliterate accented words).

## S7 conventions

* All CPET data structures are S7 classes defined in `R/classes.R`.
* Generics live in `R/generics.R`; method implementations in
  `R/methods-*.R`.
* Attach each `method()` block with both `@rdname` and `@name`, e.g.:

  ```r
  #' @rdname assess_quality
  #' @name assess_quality-CpetData
  method(assess_quality, CpetData) <- function(x, ...) { ... }
  ```

* Access properties with `@`; never with `$`.

## Roxygen

* Every exported function needs `@description`, `@param` for each
  argument, `@return`, and at least one `@examples` block (wrap with
  `\dontrun{}` when it requires a data file).
* Run `devtools::document()` before committing. Re-run `R CMD check`.

## Shiny modules

* One module per file, named `R/mod_<area>.R`.
* UI and server functions co-located.
* All user-facing text goes through `tr(key, language)`. Add keys to both
  `inst/translations/labels_en.yml` and `inst/translations/labels_fr.yml`
  — the test suite enforces EN/FR key parity.
* Guard observers with `shiny::req()` when they depend on upstream data.

## Tests

* Unit tests in `tests/testthat/test-*.R`.
* Shiny module tests use `shiny::testServer()`; end-to-end tests (if
  added) use `shinytest2::AppDriver`.
* Mock realistic physiology: resting VO2 150–400 mL/min, resting RER
  0.70–0.90, max HR 60–220 bpm, max RER 0.70–1.30.

## Commits & PRs

* Conventional-style messages (`feat:`, `fix:`, `chore:`, `docs:` …) are
  preferred but not required.
* Keep commits focused. Run the test suite locally before pushing.
* Open a PR against `main`; CI (R CMD check) must pass.

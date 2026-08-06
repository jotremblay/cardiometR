# Dialect discovery, loading, merging, and scoring.
#
# A dialect is a YAML file describing one metabolic cart's export: which column
# names it uses, how its header block is laid out, and how to recognise it.
# Adding support for a new cart means adding a file, not writing R code.

# Cache, keyed on the file path plus its modification time, so editing a
# dialect during a session takes effect without restarting R. Mirrors the
# .translation_cache pattern in R/i18n.R.
.dialect_cache <- new.env(parent = emptyenv())


#' Where dialect files are looked for
#'
#' In order of precedence, highest first: the `cardiometR.dialect_dir` option,
#' the `CARDIOMETR_DIALECTS` environment variable, the user's own configuration
#' directory, then the copies shipped with the package. A file in an earlier
#' directory replaces one of the same name later, so a user can override a
#' packaged dialect without editing the installation.
#'
#' @return A character vector of existing directories.
#'
#' @keywords internal
dialect_search_path <- function() {
  from_env <- Sys.getenv("CARDIOMETR_DIALECTS", unset = "")
  candidates <- c(
    getOption("cardiometR.dialect_dir", default = character()),
    if (nzchar(from_env)) strsplit(from_env, .Platform$path.sep, fixed = TRUE)[[1L]],
    file.path(tools::R_user_dir("cardiometR", "config"), "dialects"),
    system.file("dialects", package = "cardiometR")
  )
  candidates <- unique(candidates[nzchar(candidates)])
  candidates[dir.exists(candidates)]
}


#' Force every string in a nested list to UTF-8
#'
#' `yaml::read_yaml()` returns strings in the native encoding on Windows when
#' the session locale is not UTF-8. The accent folding in [norm_key()] compares
#' against literal UTF-8 code points, so a mismatch there would silently break
#' every accented alias on one platform only.
#'
#' @param x A list, or anything else.
#'
#' @return `x` with all character data marked UTF-8.
#'
#' @keywords internal
force_utf8 <- function(x) {
  if (is.character(x)) {
    return(enc2utf8(x))
  }
  if (is.list(x)) {
    out <- lapply(x, force_utf8)
    names(out) <- if (is.null(names(x))) NULL else enc2utf8(names(x))
    return(out)
  }
  x
}


#' Read one dialect YAML file, with caching
#'
#' @param path Path to a `.yml` file.
#'
#' @return The parsed list.
#'
#' @keywords internal
read_dialect_file <- function(path) {
  key <- paste(normalizePath(path, mustWork = FALSE),
               as.numeric(file.mtime(path)), sep = "@")
  if (exists(key, envir = .dialect_cache)) {
    return(get(key, envir = .dialect_cache))
  }
  parsed <- force_utf8(yaml::read_yaml(path))
  assign(key, parsed, envir = .dialect_cache)
  parsed
}


#' Clear the dialect cache
#'
#' Useful when editing a dialect file during a session.
#'
#' @return Invisibly `NULL`.
#'
#' @keywords internal
clear_dialect_cache <- function() {
  rm(list = ls(envir = .dialect_cache), envir = .dialect_cache)
  invisible(NULL)
}


#' Locate every available dialect file
#'
#' @return A named character vector of paths, keyed on the dialect's file stem.
#'   Files beginning with an underscore are fragments, not dialects, and are
#'   excluded.
#'
#' @keywords internal
find_dialect_files <- function() {
  found <- character()
  for (dir in dialect_search_path()) {
    files <- list.files(dir, pattern = "[.]ya?ml$", full.names = TRUE)
    stems <- sub("[.]ya?ml$", "", basename(files))
    keep <- !startsWith(stems, "_") & !(stems %in% names(found))
    found <- c(found, stats::setNames(files[keep], stems[keep]))
  }
  found
}


#' Find a shared fragment such as `_core.yml`
#'
#' @param name Fragment name without extension.
#'
#' @return A path, or `NA` when the fragment does not exist.
#'
#' @keywords internal
find_dialect_fragment <- function(name) {
  for (dir in dialect_search_path()) {
    for (ext in c(".yml", ".yaml")) {
      path <- file.path(dir, paste0(name, ext))
      if (file.exists(path)) {
        return(path)
      }
    }
  }
  NA_character_
}


# Recursive merge: values in `over` replace values in `under`, except that two
# lists are merged element by element and two plain vectors are concatenated.
merge_dialect <- function(under, over) {
  if (is.null(over)) return(under)
  if (is.null(under)) return(over)
  if (!is.list(under) || !is.list(over)) return(over)

  out <- under
  for (key in names(over)) {
    if (key %in% names(under)) {
      if (is.list(under[[key]]) && is.list(over[[key]])) {
        out[[key]] <- merge_dialect(under[[key]], over[[key]])
      } else if (!is.list(under[[key]]) && !is.list(over[[key]])) {
        out[[key]] <- unique(c(under[[key]], over[[key]]))
      } else {
        out[[key]] <- over[[key]]
      }
    } else {
      out[[key]] <- over[[key]]
    }
  }
  out
}


# Flatten the columns block into a normalised-key to canonical-name lookup.
compile_column_lookup <- function(spec) {
  lookup <- character()
  for (canonical in names(spec$columns)) {
    entry <- spec$columns[[canonical]]
    aliases <- c(canonical, unlist(entry$aliases, use.names = FALSE))
    # A brand file may add bare aliases under a separate `aliases:` block.
    extra <- spec$aliases[[canonical]]
    aliases <- unique(c(aliases, unlist(extra, use.names = FALSE)))
    keys <- norm_key(aliases)
    keys <- keys[nzchar(keys)]
    # First declaration wins, so a brand file never silently steals a key that
    # another column already owns.
    keys <- keys[!keys %in% names(lookup)]
    lookup[keys] <- canonical
  }
  lookup
}


# Flatten a vocabulary block, canonical -> language -> aliases.
compile_value_vocab <- function(block) {
  lookup <- character()
  for (canonical in names(block)) {
    aliases <- unlist(block[[canonical]], use.names = FALSE)
    keys <- norm_key(c(canonical, aliases))
    keys <- keys[nzchar(keys) & !keys %in% names(lookup)]
    lookup[keys] <- canonical
  }
  lookup
}


compile_metadata_lookup <- function(block) {
  lookup <- character()
  for (field in names(block)) {
    aliases <- unlist(block[[field]], use.names = FALSE)
    keys <- norm_key(c(field, aliases))
    keys <- keys[nzchar(keys) & !keys %in% names(lookup)]
    lookup[keys] <- field
  }
  lookup
}


#' Resolve a format name to a dialect file
#'
#' A dialect can be referred to either by its file stem (`cosmed-omnia`) or by
#' the short `name` it declares inside (`cosmed`). Both are accepted so that a
#' user is not obliged to know how the file happens to be called.
#'
#' @param name Dialect name or file stem.
#'
#' @return The path to the matching file, or `NA`.
#'
#' @keywords internal
resolve_dialect_path <- function(name) {
  files <- find_dialect_files()
  if (length(files) == 0) {
    return(NA_character_)
  }
  if (name %in% names(files)) {
    return(unname(files[[name]]))
  }
  declared <- vapply(files, function(path) {
    as.character(read_dialect_file(path)$name %||% NA_character_)
  }, character(1))
  hit <- which(!is.na(declared) & declared == name)
  if (length(hit) == 0) {
    return(NA_character_)
  }
  unname(files[[hit[[1L]]]])
}


#' Every name a dialect answers to, for error messages
#'
#' @return A character vector of stems and declared names.
#'
#' @keywords internal
known_dialect_names <- function() {
  files <- find_dialect_files()
  if (length(files) == 0) {
    return(character())
  }
  declared <- vapply(files, function(path) {
    as.character(read_dialect_file(path)$name %||% NA_character_)
  }, character(1))
  sort(unique(stats::na.omit(c(names(files), unname(declared)))))
}


#' Load a dialect by name, merged with its base and compiled
#'
#' @param name Dialect name or file stem.
#'
#' @return A list holding the merged specification plus the compiled lookup
#'   tables used by the import engine.
#'
#' @keywords internal
load_dialect <- function(name) {
  path <- resolve_dialect_path(name)
  if (is.na(path)) {
    cli::cli_abort(c(
      "Unknown format {.val {name}}.",
      "i" = "Available: {.val {known_dialect_names()}}.",
      "i" = "See {.fn list_cpet_dialects} for where these come from."
    ))
  }

  cache_key <- paste0("compiled:", path, ":",
                      as.numeric(file.mtime(path)), ":",
                      paste(dialect_search_path(), collapse = "|"))
  if (exists(cache_key, envir = .dialect_cache)) {
    return(get(cache_key, envir = .dialect_cache))
  }

  spec <- read_dialect_file(path)

  base_name <- spec$extends %||% "_core"
  base_path <- find_dialect_fragment(base_name)
  if (!is.na(base_path)) {
    spec <- merge_dialect(read_dialect_file(base_path), spec)
  }

  required <- names(spec$columns)[
    vapply(spec$columns, function(e) isTRUE(e$required), logical(1))
  ]
  text_columns <- names(spec$columns)[
    vapply(spec$columns, function(e) identical(e$type, "text"), logical(1))
  ]
  declared_units <- vapply(
    spec$columns,
    function(e) if (is.null(e$unit)) NA_character_ else as.character(e$unit),
    character(1)
  )

  compiled <- list(
    name = spec$name %||% name,
    label = spec$label %||% (spec$name %||% name),
    spec = spec,
    path = path,
    extensions = spec$extensions %||% c("xlsx", "xls"),
    sheet = spec$sheet,
    layout = spec$layout,
    ignore = unlist(spec$ignore, use.names = FALSE) %||% character(),
    ignore_patterns = unlist(spec$ignore_patterns, use.names = FALSE) %||% character(),
    required = required,
    text_columns = text_columns,
    declared_units = declared_units,
    lookup = compile_column_lookup(spec),
    metadata_lookup = compile_metadata_lookup(spec$metadata_labels %||% list()),
    phase_vocab = compile_value_vocab(spec$vocabularies$phase %||% list()),
    sex_vocab = compile_value_vocab(spec$vocabularies$sex %||% list()),
    modality_patterns = unlist(spec$vocabularies$modality, use.names = TRUE)
  )

  assign(cache_key, compiled, envir = .dialect_cache)
  compiled
}


#' List the metabolic cart formats cardiometR can read
#'
#' Formats are described by YAML files. Alongside the ones shipped with the
#' package, files placed in your own configuration directory are picked up
#' automatically, so support for a new cart, or for a local variation of an
#' existing one, needs no change to the package.
#'
#' @return A data frame with one row per format: its `name`, a human-readable
#'   `label`, the file `extensions` it applies to, and the `path` it was read
#'   from.
#'
#' @examples
#' list_cpet_dialects()
#'
#' @seealso [read_cpet()] to read a file, [preview_cpet_columns()] to see how a
#'   file's columns would be interpreted.
#'
#' @export
list_cpet_dialects <- function() {
  files <- find_dialect_files()
  if (length(files) == 0) {
    return(data.frame(
      name = character(), label = character(),
      extensions = character(), path = character(),
      stringsAsFactors = FALSE
    ))
  }
  rows <- lapply(names(files), function(name) {
    dialect <- load_dialect(name)
    data.frame(
      name = dialect$name,
      label = dialect$label,
      extensions = paste(dialect$extensions, collapse = ", "),
      path = dialect$path,
      stringsAsFactors = FALSE
    )
  })
  out <- do.call(rbind, rows)
  out[order(out$name), , drop = FALSE]
}


#' Score how well a dialect matches a file
#'
#' @param dialect A compiled dialect.
#' @param evidence A list holding `extension`, `sheet_names`, `grid` and
#'   `header_candidates`.
#'
#' @return A list with the numeric `score` and the `why`, a character vector
#'   naming the rules that matched.
#'
#' @keywords internal
score_dialect <- function(dialect, evidence) {
  score <- 0
  why <- character()

  if (!is.null(evidence$extension) &&
      tolower(evidence$extension) %in% tolower(dialect$extensions)) {
    score <- score + 1
    why <- c(why, "file extension")
  }

  for (rule in dialect$spec$detect %||% list()) {
    weight <- rule$weight %||% 1
    matched <- switch(rule$where %||% "",
      sheet_names = any(grepl(rule$matches, evidence$sheet_names %||% character())),
      cell = {
        grid <- evidence$grid
        row <- rule$row %||% 1L
        col <- rule$col %||% 1L
        !is.null(grid) && nrow(grid) >= row && ncol(grid) >= col &&
          !is.na(grid[row, col]) && grepl(rule$matches, grid[row, col])
      },
      header = any(grepl(rule$matches, evidence$header_candidates %||% character())),
      FALSE
    )
    if (isTRUE(matched)) {
      score <- score + weight
      why <- c(why, sprintf("%s matches %s", rule$where, rule$matches))
    }
  }

  list(score = score, why = why)
}

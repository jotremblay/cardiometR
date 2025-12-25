# Package-level cache for translations
# This environment persists across function calls within the session
.translation_cache <- new.env(parent = emptyenv())

#' Get Translated Label
#'
#' Retrieves a translated label from the package translation files.
#' Translations are cached on first load for performance.
#'
#' @param key The translation key to look up
#' @param language Language code: "en" (English) or "fr" (French)
#'
#' @return The translated string, or the key itself if not found
#'
#' @examples
#' tr("app_title", "en")
#' tr("app_title", "fr")
#'
#' @export
tr <- function(key, language = "en") {
  if (!language %in% c("en", "fr")) {
    cli::cli_warn("Unknown language '{language}', defaulting to 'en'")
    language <- "en"
  }

  # Check cache first
  cache_key <- paste0("labels_", language)
  if (!exists(cache_key, envir = .translation_cache)) {
    # Load and cache translations
    labels_file <- system.file(
      "translations",
      paste0("labels_", language, ".yml"),
      package = "cardiometR"
    )

    if (labels_file == "") {
      cli::cli_warn("Translation file not found for language '{language}'")
      return(key)
    }

    assign(cache_key, yaml::read_yaml(labels_file), envir = .translation_cache)
  }

  labels <- get(cache_key, envir = .translation_cache)
  labels[[key]] %||% key
}

#' Load All Translations for a Language
#'
#' Loads all translation labels for the specified language.
#' Uses cached translations if available.
#'
#' @param language Language code: "en" (English) or "fr" (French)
#'
#' @return A named list of all translation labels
#'
#' @keywords internal
load_translations <- function(language = "en") {
  if (!language %in% c("en", "fr")) {
    cli::cli_warn("Unknown language '{language}', defaulting to 'en'")
    language <- "en"
  }

  # Check cache first
  cache_key <- paste0("labels_", language)
  if (exists(cache_key, envir = .translation_cache)) {
    return(get(cache_key, envir = .translation_cache))
  }

  labels_file <- system.file(
    "translations",
    paste0("labels_", language, ".yml"),
    package = "cardiometR"
  )

  if (labels_file == "") {
    cli::cli_abort("Translation file not found for language '{language}'")
  }

  labels <- yaml::read_yaml(labels_file)
  assign(cache_key, labels, envir = .translation_cache)
  labels
}

#' Clear Translation Cache
#'
#' Clears the cached translations, forcing a reload on next access.
#' Useful during development when modifying translation files.
#'
#' @return Invisibly returns NULL
#'
#' @keywords internal
clear_translation_cache <- function() {
  rm(list = ls(envir = .translation_cache), envir = .translation_cache)
  invisible(NULL)
}

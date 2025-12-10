#' Get Translated Label
#'
#' Retrieves a translated label from the package translation files.
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

  labels_file <- system.file(
    "translations",
    paste0("labels_", language, ".yml"),
    package = "cardiometR"
  )

  if (labels_file == "") {
    cli::cli_warn("Translation file not found for language '{language}'")
    return(key)
 }

  labels <- yaml::read_yaml(labels_file)
  labels[[key]] %||% key
}

#' Load All Translations for a Language
#'
#' Loads all translation labels for the specified language.
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

  labels_file <- system.file(
    "translations",
    paste0("labels_", language, ".yml"),
    package = "cardiometR"
  )

  if (labels_file == "") {
    cli::cli_abort("Translation file not found for language '{language}'")
  }

  yaml::read_yaml(labels_file)
}

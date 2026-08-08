# Small cross-cutting helpers.

#' Age in completed years, for display
#'
#' COSMED stores age as a fraction of a year, computed from the date of birth
#' and the test date. A real export reads 31.2902181427408. That number is
#' worth keeping, since age at test is a genuine covariate, but nobody writes
#' it on a report.
#'
#' Completed years, not rounded years: someone aged 31.9 is 31, not 32.
#'
#' @param age Age in years, possibly fractional.
#'
#' @return A character string of whole years, or `"--"` when age is missing.
#'
#' @keywords internal
format_age <- function(age) {
  if (length(age) == 0 || !is.finite(age[[1L]])) {
    return("--")
  }
  as.character(floor(age[[1L]]))
}


#' Did the cart record a usable rest phase?
#'
#' @param df A breath table.
#'
#' @return `TRUE` when the `phase` column exists and marks at least one row as
#'   rest.
#'
#' @keywords internal
has_rest_phase <- function(df) {
  "phase" %in% names(df) && any(!is.na(df$phase) & df$phase == "rest")
}

#' Turn a missing number into an absent one
#'
#' The optional metadata properties are typed as "a number, or nothing". A
#' value that could not be read is nothing, not `NA`: carrying `NA` through
#' means every consumer has to test for it separately.
#'
#' @param x A length-one value.
#'
#' @return `NULL` when `x` is missing, otherwise `x`.
#'
#' @keywords internal
or_null <- function(x) {
  if (length(x) == 0 || all(is.na(x))) NULL else x
}


#' Does a column carry a usable signal?
#'
#' A column can be present and still say nothing. COSMED exports a speed
#' channel on every test, including cycle tests, where it is filled with zeros.
#' Code that treats mere presence as evidence of a treadmill therefore switches
#' a cycle test to treadmill display, treadmill prediction equations, and a
#' stage table full of zeros.
#'
#' @param df A data frame.
#' @param col Name of the column to test.
#'
#' @return `TRUE` when the column exists and holds at least one value greater
#'   than zero. Non-numeric columns count as signal when they hold any
#'   non-missing value.
#'
#' @keywords internal
has_signal <- function(df, col) {
  if (!col %in% names(df)) {
    return(FALSE)
  }
  values <- df[[col]]
  if (!is.numeric(values)) {
    # A text column of empty strings is as empty as one of NA. COSMED writes
    # an event-marker column on every test, usually with nothing in it.
    return(any(!is.na(values) & nzchar(trimws(as.character(values)))))
  }
  any(!is.na(values) & values > 0)
}


#' Filter breath data to the exercise phase
#'
#' Removes rest, warmup, and recovery. Prefers phase labels when present,
#' because stage numbers can mark recovery as a positive stage.
#'
#' @param breaths Data frame with breath-by-breath data.
#'
#' @return Filtered data frame containing only exercise-phase breaths.
#'
#' @keywords internal
filter_exercise_data <- function(breaths) {
  if (nrow(breaths) == 0) {
    return(breaths)
  }

  # Phase labels are the most reliable exercise window.
  if ("phase" %in% names(breaths) && any(!is.na(breaths$phase))) {
    exclude <- c("rest", "warmup", "recovery", "cool")
    return(breaths |> dplyr::filter(!tolower(.data$phase) %in% exclude))
  }

  if ("stage_name" %in% names(breaths) && any(!is.na(breaths$stage_name))) {
    return(
      breaths |>
        dplyr::filter(
          !grepl(
            "^(rest|warmup|warm.?up|recovery|cool)",
            tolower(as.character(.data$stage_name))
          )
        )
    )
  }

  if ("stage" %in% names(breaths) && any(!is.na(breaths$stage))) {
    return(breaths |> dplyr::filter(.data$stage > 0))
  }

  if ("power_w" %in% names(breaths) && any(!is.na(breaths$power_w))) {
    return(breaths |> dplyr::filter(.data$power_w > 0))
  }

  breaths
}


#' Drop optional columns that hold no information
#'
#' Columns that are entirely missing, or entirely zero, survive import and then
#' mislead every downstream test that checks only for their presence. Required
#' columns are never dropped, and neither are the text columns, whose zeros are
#' meaningful.
#'
#' @param df A data frame of breath-by-breath data.
#' @param protect Column names that must be kept whatever they contain.
#'
#' @return `df` without the uninformative optional columns.
#'
#' @keywords internal
drop_empty_columns <- function(df, protect = character()) {
  candidates <- setdiff(names(df), protect)
  if (length(candidates) == 0) {
    return(df)
  }
  empty <- vapply(candidates, function(nm) !has_signal(df, nm), logical(1))
  if (!any(empty)) {
    return(df)
  }
  df[, setdiff(names(df), candidates[empty]), drop = FALSE]
}

# S7 Methods for Stage Extraction and Summarization

#' @rdname extract_stages
#' @export
method(extract_stages, CpetData) <- function(x,
                                              protocol = c("step", "ramp", "auto"),
                                              stage_duration = 180,
                                              ...) {
  protocol <- match.arg(protocol)
  breaths <- x@breaths

  # Try to use phase column if available
  if ("phase" %in% names(breaths)) {
    stages <- extract_stages_from_phase(breaths, protocol, stage_duration)
  } else if ("power_w" %in% names(breaths)) {
    # Try to detect from power changes
    stages <- extract_stages_from_power(breaths, protocol, stage_duration)
  } else {
    # Fall back to time-based stages
    stages <- extract_stages_by_time(breaths, stage_duration)
  }

  # Return CpetData with stages populated
  CpetData(
    participant = x@participant,
    metadata = x@metadata,
    breaths = dplyr::left_join(breaths, stages, by = "time_s"),
    stages = stages,
    is_averaged = x@is_averaged,
    averaging_window = x@averaging_window
  )
}


#' Extract Stages from Phase Column
#'
#' @param breaths Breath data tibble
#' @param protocol Protocol type
#' @param stage_duration Stage duration in seconds
#' @return Tibble with stage annotations
#' @keywords internal
extract_stages_from_phase <- function(breaths, protocol, stage_duration) {
  # Get unique phases
  phases <- breaths |>
    dplyr::mutate(phase = tolower(phase)) |>
    dplyr::filter(!is.na(phase))

  # Identify exercise portion
  exercise_phases <- c("exercise", "warmup", "work", "exer")
  exercise_data <- phases |>
    dplyr::filter(grepl(paste(exercise_phases, collapse = "|"), phase))

  if (nrow(exercise_data) == 0) {
    # No exercise phase found, use all non-rest data
    exercise_data <- phases |>
      dplyr::filter(!grepl("rest|recovery|cool", phase))
  }

  if (nrow(exercise_data) == 0) {
    return(extract_stages_by_time(breaths, stage_duration))
  }

  # For step protocol, create stages based on duration
  if (protocol %in% c("step", "auto")) {
    exercise_start <- min(exercise_data$time_s)
    exercise_end <- max(exercise_data$time_s)

    n_stages <- ceiling((exercise_end - exercise_start) / stage_duration)

    stages <- tibble::tibble(
      time_s = breaths$time_s,
      stage = dplyr::case_when(
        time_s < exercise_start ~ 0L,  # Rest
        time_s > exercise_end ~ n_stages + 1L,  # Recovery
        TRUE ~ as.integer(ceiling((time_s - exercise_start) / stage_duration))
      ),
      stage_name = dplyr::case_when(
        stage == 0L ~ "Rest",
        stage > n_stages ~ "Recovery",
        TRUE ~ paste("Stage", stage)
      )
    )
  } else {
    # Ramp protocol - stages based on power increments
    if ("power_w" %in% names(breaths)) {
      stages <- extract_stages_from_power(breaths, protocol, stage_duration)
    } else {
      stages <- extract_stages_by_time(breaths, stage_duration)
    }
  }

  stages
}


#' Extract Stages from Power Changes
#'
#' @param breaths Breath data tibble
#' @param protocol Protocol type
#' @param stage_duration Stage duration for step protocols
#' @return Tibble with stage annotations
#' @keywords internal
extract_stages_from_power <- function(breaths, protocol, stage_duration) {
  # Get power data
  power_data <- breaths |>
    dplyr::filter(!is.na(power_w))

  if (nrow(power_data) == 0) {
    return(extract_stages_by_time(breaths, stage_duration))
  }

  # Detect power changes
  power_changes <- power_data |>
    dplyr::mutate(
      power_diff = power_w - dplyr::lag(power_w, default = 0),
      power_change = abs(power_diff) > 10  # Threshold for detecting new stage
    )

  # For step protocol, group by power level
  if (protocol %in% c("step", "auto")) {
    # Round power to nearest increment (e.g., 25W or 30W stages)
    power_increment <- detect_power_increment(power_data$power_w)

    stages <- breaths |>
      dplyr::mutate(
        power_rounded = round(power_w / power_increment) * power_increment,
        stage = dplyr::dense_rank(power_rounded)
      ) |>
      dplyr::mutate(
        stage = dplyr::if_else(is.na(stage), 0L, as.integer(stage)),
        stage_name = dplyr::if_else(stage == 0, "Rest", paste("Stage", stage))
      ) |>
      dplyr::select(time_s, stage, stage_name, power_rounded)

  } else {
    # Ramp - use time windows
    stages <- extract_stages_by_time(breaths, stage_duration)
  }

  stages
}


#' Extract Stages by Time Windows
#'
#' @param breaths Breath data tibble
#' @param stage_duration Stage duration in seconds
#' @return Tibble with stage annotations
#' @keywords internal
extract_stages_by_time <- function(breaths, stage_duration) {
  start_time <- min(breaths$time_s)
  end_time <- max(breaths$time_s)

  stages <- breaths |>
    dplyr::mutate(
      stage = as.integer(ceiling((time_s - start_time) / stage_duration)),
      stage = pmax(stage, 1L),
      stage_name = paste("Stage", stage)
    ) |>
    dplyr::select(time_s, stage, stage_name)

  stages
}


#' Detect Power Increment Used in Protocol
#'
#' @param power Power values
#' @return Detected power increment
#' @keywords internal
detect_power_increment <- function(power) {
  # Get unique non-zero power values
  unique_power <- sort(unique(power[power > 0]))

  if (length(unique_power) < 2) {
    return(25)  # Default increment
  }

  # Calculate differences between consecutive power levels
  diffs <- diff(unique_power)
  diffs <- diffs[diffs > 5]  # Filter out noise

  if (length(diffs) == 0) {
    return(25)
  }

  # Find most common increment
  common_increment <- round(median(diffs) / 5) * 5
  common_increment <- max(common_increment, 10)

  common_increment
}


#' @rdname summarize_stages
#' @export
method(summarize_stages, CpetData) <- function(x,
                                                method = c("last30s", "last60s", "mean", "peak"),
                                                ...) {
  method <- match.arg(method)

  has_stage_col <- "stage" %in% names(x@breaths)
  has_stages_df <- !is.null(x@stages) && inherits(x@stages, "data.frame") &&
                   nrow(x@stages) > 0 && "time_s" %in% names(x@stages)

  if (!has_stages_df && !has_stage_col) {
    cli::cli_abort("No stages defined. Use extract_stages() first.")
  }

  breaths <- x@breaths

  # Use stage column from breaths
  if (!has_stage_col && has_stages_df) {
    breaths <- dplyr::left_join(breaths, x@stages, by = "time_s")
  }

  # Determine summary function based on method
  summary_fn <- switch(method,
    last30s = function(df) summarize_last_n_seconds(df, 30),
    last60s = function(df) summarize_last_n_seconds(df, 60),
    mean = function(df) summarize_mean(df),
    peak = function(df) summarize_peak(df)
  )

  # Group by stage and summarize
  stage_summary <- breaths |>
    dplyr::filter(!is.na(stage), stage > 0) |>  # Exclude rest (stage 0)
    dplyr::group_by(stage) |>
    dplyr::group_modify(~ summary_fn(.x)) |>
    dplyr::ungroup()

  # Add stage names if available
  if ("stage_name" %in% names(breaths)) {
    stage_names <- breaths |>
      dplyr::filter(!is.na(stage_name)) |>
      dplyr::distinct(stage, stage_name)

    stage_summary <- stage_summary |>
      dplyr::left_join(stage_names, by = "stage")
  }

  tibble::as_tibble(stage_summary)
}


#' Summarize Last N Seconds of Stage
#'
#' @param df Stage data
#' @param seconds Number of seconds from end of stage
#' @return Summary tibble
#' @keywords internal
summarize_last_n_seconds <- function(df, seconds) {
  max_time <- max(df$time_s)
  cutoff <- max_time - seconds

  last_data <- df |>
    dplyr::filter(time_s >= cutoff)

  if (nrow(last_data) == 0) {
    last_data <- df  # Use all data if stage shorter than window
  }

  summarize_mean(last_data)
}


#' Summarize Stage by Mean
#'
#' @param df Stage data
#' @return Summary tibble
#' @keywords internal
summarize_mean <- function(df) {
  tibble::tibble(
    time_s = mean(df$time_s, na.rm = TRUE),
    duration_s = max(df$time_s) - min(df$time_s),
    vo2_ml = mean(df$vo2_ml, na.rm = TRUE),
    vco2_ml = mean(df$vco2_ml, na.rm = TRUE),
    ve_l = mean(df$ve_l, na.rm = TRUE),
    rer = mean(df$rer, na.rm = TRUE),
    hr_bpm = if ("hr_bpm" %in% names(df)) mean(df$hr_bpm, na.rm = TRUE) else NA_real_,
    power_w = if ("power_w" %in% names(df)) mean(df$power_w, na.rm = TRUE) else NA_real_,
    n_breaths = nrow(df)
  )
}


#' Summarize Stage by Peak Values
#'
#' @param df Stage data
#' @return Summary tibble
#' @keywords internal
summarize_peak <- function(df) {
  tibble::tibble(
    time_s = mean(df$time_s, na.rm = TRUE),
    duration_s = max(df$time_s) - min(df$time_s),
    vo2_ml = max(df$vo2_ml, na.rm = TRUE),
    vco2_ml = max(df$vco2_ml, na.rm = TRUE),
    ve_l = max(df$ve_l, na.rm = TRUE),
    rer = max(df$rer, na.rm = TRUE),
    hr_bpm = if ("hr_bpm" %in% names(df)) max(df$hr_bpm, na.rm = TRUE) else NA_real_,
    power_w = if ("power_w" %in% names(df)) max(df$power_w, na.rm = TRUE) else NA_real_,
    n_breaths = nrow(df)
  )
}

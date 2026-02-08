# S7 Methods for Stage Extraction and Summarization

#' @name extract_stages
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

  # For step protocol, group by power level
  if (protocol %in% c("step", "auto")) {
    # Round power to nearest increment (e.g., 25W or 30W stages)
    power_increment <- detect_power_increment(power_data$power_w)

    stages <- breaths |>
      dplyr::mutate(
        power_rounded = ifelse(is.na(power_w), NA_real_,
                               round(power_w / power_increment) * power_increment)
      ) |>
      dplyr::mutate(
        stage = dplyr::if_else(
          !is.na(power_rounded) & power_rounded > 0,
          as.integer(match(power_rounded, sort(unique(power_rounded[power_rounded > 0])))),
          0L
        ),
        stage_name = dplyr::if_else(stage == 0, "Rest", paste("Stage", stage))
      ) |>
      dplyr::select(time_s, stage, stage_name, power_rounded)

  } else {
    # Ramp - smooth power and detect increments
    avg_interval <- mean(diff(power_data$time_s), na.rm = TRUE)
    if (is.na(avg_interval) || avg_interval <= 0) {
      avg_interval <- 1
    }

    smooth_window_s <- min(30, max(10, stage_duration / 6))
    k <- max(5, round(smooth_window_s / avg_interval))
    k <- min(k, nrow(power_data))

    power_smooth <- if (k >= 5) {
      zoo::rollmean(power_data$power_w, k = k, fill = NA, align = "center")
    } else {
      power_data$power_w
    }

    power_used <- ifelse(is.na(power_smooth), power_data$power_w, power_smooth)
    power_increment <- detect_power_increment(power_used)

    if (is.null(power_increment) || is.na(power_increment) || power_increment <= 0) {
      stages <- extract_stages_by_time(breaths, stage_duration)
    } else {
      # Merge smoothed power back to full breaths by time
      power_smoothed <- tibble::tibble(
        time_s = power_data$time_s,
        power_used = power_used
      )

      breaths_with_power <- breaths |>
        dplyr::left_join(power_smoothed, by = "time_s") |>
        dplyr::mutate(
          power_used = ifelse(is.na(power_used), power_w, power_used),
          power_rounded = ifelse(
            is.na(power_used),
            NA_real_,
            round(power_used / power_increment) * power_increment
          ),
          stage = dplyr::if_else(
            !is.na(power_rounded) & power_rounded > 0,
            as.integer(match(power_rounded, sort(unique(power_rounded[power_rounded > 0])))),
            0L
          ),
          stage_name = dplyr::if_else(stage == 0, "Rest", paste("Stage", stage))
        ) |>
        dplyr::select(time_s, stage, stage_name, power_rounded)

      stages <- breaths_with_power
    }
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


#' Detect Protocol Configuration from CPET Data
#'
#' Automatically detect exercise protocol parameters (modality, stage duration,
#' intensity increment, starting intensity) from breath-by-breath CPET data.
#'
#' @param data A CpetData S7 object containing breath-by-breath measurements
#'
#' @return A ProtocolConfig S7 object with detected parameters
#'
#' @details
#' The detection algorithm:
#' 1. Determines modality from available columns (speed for treadmill, power for cycling)
#' 2. Rounds the intensity signal to detect plateaus (5W for cycling, 0.5 km/h for treadmill)
#' 3. Uses run-length encoding on the exercise portion to find stage boundaries
#' 4. Estimates stage duration from the median plateau length (rounded to nearest 30s)
#' 5. Estimates increment from the median positive difference between consecutive levels
#'
#' @examples
#' \dontrun{
#' config <- detect_protocol_config(cpet_data)
#' config@modality
#' config@stage_duration_s
#' }
#'
#' @export
detect_protocol_config <- function(data) {
  breaths <- data@breaths

  # Determine modality

  has_speed <- "speed_kmh" %in% names(breaths) &&
    any(!is.na(breaths$speed_kmh) & breaths$speed_kmh > 0)
  has_power <- "power_w" %in% names(breaths) &&
    any(!is.na(breaths$power_w) & breaths$power_w > 0)

  modality <- if (has_speed) "treadmill" else if (has_power) "cycling" else "other"

  # Get intensity signal and rounding unit
  if (modality == "treadmill") {
    intensity <- breaths$speed_kmh
    round_unit <- 0.5
  } else if (modality == "cycling") {
    intensity <- breaths$power_w
    round_unit <- 5
  } else {
    return(ProtocolConfig(modality = "other"))
  }

  # Round intensity to detect plateaus
  intensity_rounded <- round(intensity / round_unit) * round_unit

  # Get exercise portion only (intensity > 0)
  exercise_mask <- !is.na(intensity_rounded) & intensity_rounded > 0
  time_ex <- breaths$time_s[exercise_mask]
  intensity_ex <- intensity_rounded[exercise_mask]

  if (length(intensity_ex) < 10) {
    return(ProtocolConfig(modality = modality))
  }

  # Detect level changes using run-length encoding
  rle_result <- rle(intensity_ex)
  levels <- rle_result$values

  # Calculate duration of each plateau using time differences
  cum_lengths <- cumsum(rle_result$lengths)
  start_indices <- c(1, cum_lengths[-length(cum_lengths)] + 1)

  plateau_durations <- purrr::map2_dbl(start_indices, cum_lengths, function(s, e) {
    time_ex[e] - time_ex[s]
  })

  # Filter out very short plateaus (noise) - keep plateaus > 30s
  valid <- plateau_durations > 30
  levels <- levels[valid]
  plateau_durations <- plateau_durations[valid]

  if (length(levels) < 2) {
    return(ProtocolConfig(modality = modality))
  }

  # Detect stage duration: median of plateau durations, rounded to nearest 30s
  stage_duration <- round(stats::median(plateau_durations) / 30) * 30
  stage_duration <- max(stage_duration, 60)

  # Detect increment: median difference between consecutive levels
  level_diffs <- diff(levels)
  positive_diffs <- level_diffs[level_diffs > 0]

  increment <- if (length(positive_diffs) == 0) {
    round_unit
  } else {
    inc <- round(stats::median(positive_diffs) / round_unit) * round_unit
    max(inc, round_unit)
  }

  # Starting intensity: first level
  starting_intensity <- levels[1]

  ProtocolConfig(
    modality = modality,
    starting_intensity = starting_intensity,
    increment_size = increment,
    stage_duration_s = stage_duration
  )
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


#' @name summarize_stages
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

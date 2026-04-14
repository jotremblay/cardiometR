# Visualization Functions for cardiometR
# CPET-specific plots based on clinical guidelines

#' Filter exercise data by removing warmup/rest/recovery phases
#'
#' @description
#' Filters breath-by-breath data to exercise-only phases by detecting and
#' removing warmup (stage 0), rest, and recovery data. Uses stage annotations,
#' power output, or phase labels depending on what is available.
#'
#' @param breaths Data frame with breath-by-breath data
#'
#' @return Filtered data frame containing only exercise phase data
#'
#' @keywords internal
filter_exercise_data <- function(breaths) {
  if (nrow(breaths) == 0) return(breaths)

  if ("stage" %in% names(breaths) && any(!is.na(breaths$stage))) {
    return(breaths |> dplyr::filter(stage > 0))
  }

  if ("power_w" %in% names(breaths) && any(!is.na(breaths$power_w))) {
    return(breaths |> dplyr::filter(power_w > 0))
  }

  if ("phase" %in% names(breaths) && any(!is.na(breaths$phase))) {
    exclude <- c("rest", "warmup", "recovery", "cool")
    return(breaths |> dplyr::filter(!tolower(phase) %in% exclude))
  }

  breaths
}

#' Calculate expected VO2 for treadmill exercise
#'
#' @description
#' Calculates expected VO2 from treadmill speed using the equation from
#' Leger & Mercier (1984): VO2 (mL/kg/min) = 2.209 + 3.163 * speed (km/h).
#' Returns absolute VO2 in mL/min.
#'
#' @param speed_kmh Treadmill speed in km/h (numeric vector)
#' @param weight_kg Body weight in kilograms
#'
#' @return Numeric vector of expected VO2 values in mL/min
#'
#' @references
#' Leger, L., & Mercier, D. (1984). Gross energy cost of horizontal treadmill
#' and track running. Sports Medicine, 1(4), 270-277.
#'
#' @export
calculate_expected_vo2_treadmill <- function(speed_kmh, weight_kg) {
  vo2_rel <- 2.209 + 3.163 * speed_kmh
  vo2_rel * weight_kg
}

#' Calculate expected VO2 for cycling exercise
#'
#' @description
#' Calculates the expected metabolic cost (VO2, mL/min) of upright cycle
#' ergometry from the externally measured power output. Uses the ACSM
#' metabolic equation for leg cycling (ACSM Guidelines, 10th ed., Chap. 6):
#' `VO2 (mL/min) = 10.8 * power_w + 7 * body_mass_kg`. The first term
#' captures the near-linear slope of measured VO2 versus work rate
#' (~ 10-11 mL.W-1.min-1 across the 18-25 % gross-efficiency range), and
#' the second term approximates unloaded pedaling plus resting metabolism
#' (~ 3.5 mL.kg-1.min-1 x body mass).
#'
#' @param power_w Power output in watts (numeric vector).
#' @param weight_kg Body mass in kilograms; used for the resting/unloaded
#'   component.
#' @param slope_ml_per_w Slope of the VO2-Power relation, in mL.O2.W-1.min-1.
#'   Defaults to the ACSM value of `10.8` (range 8-12 across elite to
#'   low-efficiency riders).
#' @return Numeric vector of expected VO2 values in mL/min.
#' @export
calculate_expected_vo2_cycling <- function(power_w, weight_kg,
                                           slope_ml_per_w = 10.8) {
  slope_ml_per_w * power_w + 7 * (weight_kg %||% 75)
}

#' Calculate stage averages for CPET data
#'
#' @description
#' Calculates rolling 30-second averages at the end of each power stage.
#' Uses power levels to identify stages and averages the last 30 seconds of each.
#'
#' @param breaths Data frame with breath-by-breath data
#' @param window_seconds Averaging window in seconds (default 30)
#' @param protocol_config Optional ProtocolConfig S7 object. When provided with
#'   an `increment_size` property, uses that instead of auto-detecting the power
#'   increment from noisy BxB data.
#'
#' @return Data frame with one row per stage containing averaged values
#'
#' @keywords internal
calculate_stage_averages <- function(breaths, window_seconds = 30, protocol_config = NULL) {
  if (nrow(breaths) == 0 || !"time_s" %in% names(breaths)) {
    return(tibble::tibble())
  }

  breaths <- filter_exercise_data(breaths)
  if (nrow(breaths) == 0) return(tibble::tibble())

  breaths <- breaths |>
    dplyr::arrange(time_s)

  group_col <- NULL

  # Prefer explicit stage annotations if present
  if ("stage" %in% names(breaths) && any(!is.na(breaths$stage))) {
    breaths <- breaths |>
      dplyr::mutate(.stage_group = stage)
    group_col <- ".stage_group"
  } else if ("power_w" %in% names(breaths) && any(!is.na(breaths$power_w))) {
    # Use rounded power stages when repeated levels exist
    power_increment <- if (!is.null(protocol_config) && !is.null(protocol_config@increment_size)) {
      protocol_config@increment_size
    } else {
      detect_power_increment(breaths$power_w[!is.na(breaths$power_w)])
    }
    breaths <- breaths |>
      dplyr::mutate(
        .stage_power = ifelse(is.na(power_w), NA_real_,
                              round(power_w / power_increment) * power_increment)
      )

    unique_stage_power <- sort(unique(breaths$.stage_power[!is.na(breaths$.stage_power)]))
    avg_breaths_per_stage <- if (length(unique_stage_power) > 0) {
      nrow(breaths) / length(unique_stage_power)
    } else {
      0
    }

    if (length(unique_stage_power) <= 20 || avg_breaths_per_stage >= 5) {
      breaths <- breaths |>
        dplyr::mutate(.stage_group = .stage_power)
      group_col <- ".stage_group"
    }
  }

  # Fallback to time-based bins for ramp/no-power data
  if (is.null(group_col)) {
    breaths <- breaths |>
      dplyr::mutate(
        .stage_group = floor(time_s / window_seconds)
      )
    group_col <- ".stage_group"
  }

  # Summarize each group using the last window_seconds
  stage_avg <- breaths |>
    dplyr::group_by(.data[[group_col]]) |>
    dplyr::group_modify(function(df, ...) {
      max_time <- max(df$time_s, na.rm = TRUE)
      min_time_window <- max_time - window_seconds
      stage_end <- df |>
        dplyr::filter(time_s >= min_time_window)
      if (nrow(stage_end) == 0) stage_end <- df

      numeric_cols <- names(stage_end)[sapply(stage_end, is.numeric)]
      numeric_cols <- setdiff(numeric_cols, c(".stage_group", ".stage_power", "stage"))

      stage_end |>
        dplyr::summarise(
          dplyr::across(dplyr::all_of(numeric_cols), ~ mean(.x, na.rm = TRUE)),
          n_breaths = nrow(stage_end),
          .groups = "drop"
        )
    }) |>
    dplyr::ungroup()

  stage_avg |>
    dplyr::filter(!is.na(vo2_ml))
}

#' Build VT shaded-band layers for a given x-axis unit
#'
#' Returns a list of ggplot2 layers that draw shaded vertical bands for
#' VT1 and VT2 ranges. `x_unit` controls which slot is used:
#' - `"vo2"`: expects `analysis@vt1_range` / `@vt2_range` as length-2 VO2 (mL/min)
#' - `"time_s"` or `"time_min"`: skipped (time-axis VT bands not implemented)
#' - `"power_w"`: skipped unless an explicit range is provided.
#'
#' Falls back to `geom_vline` at a legacy single value when only
#' `thresholds@vt1_vo2` / `vt2_vo2` are present (and x_unit is `"vo2"`).
#' @keywords internal
vt_band_layers <- function(analysis, thresholds = NULL, x_unit = "vo2") {
  pal <- palette_cardiometr()
  layers <- list()
  if (x_unit != "vo2") return(layers)

  vt1_r <- tryCatch(analysis@vt1_range, error = function(e) NULL)
  vt2_r <- tryCatch(analysis@vt2_range, error = function(e) NULL)

  add_band <- function(rng, color) {
    if (is.numeric(rng) && length(rng) == 2 && all(is.finite(rng))) {
      list(ggplot2::annotate("rect", xmin = rng[1], xmax = rng[2],
                             ymin = -Inf, ymax = Inf,
                             fill = color, alpha = 0.15))
    } else NULL
  }
  add_line <- function(val, color) {
    if (is.numeric(val) && length(val) == 1 && is.finite(val)) {
      list(ggplot2::geom_vline(xintercept = val, color = color,
                               linetype = "dashed", linewidth = 0.6))
    } else NULL
  }

  b1 <- add_band(vt1_r, pal[["bluish_green"]])
  b2 <- add_band(vt2_r, pal[["vermillion"]])
  if (is.null(b1) && !is.null(thresholds)) {
    v1 <- tryCatch(thresholds@vt1_vo2, error = function(e) NULL)
    b1 <- add_line(v1, pal[["bluish_green"]])
  }
  if (is.null(b2) && !is.null(thresholds)) {
    v2 <- tryCatch(thresholds@vt2_vo2, error = function(e) NULL)
    b2 <- add_line(v2, pal[["vermillion"]])
  }
  c(b1, b2)
}

is_cpet_analysis <- function(x) {
  inherits(x, "CpetAnalysis") || grepl("CpetAnalysis$", class(x)[1])
}

resolve_plot_data <- function(x, prefer_stage_summary = FALSE) {
  if (is_cpet_analysis(x)) {
    stage_summary <- x@stage_summary
    use_stage <- isTRUE(prefer_stage_summary) &&
      !is.null(stage_summary) &&
      nrow(stage_summary) > 0

    list(
      data = if (use_stage) stage_summary else x@data@breaths,
      breaths = x@data@breaths,
      stage_summary = stage_summary,
      participant = x@data@participant,
      thresholds = x@thresholds,
      using_stage_summary = use_stage
    )
  } else {
    list(
      data = x@breaths,
      breaths = x@breaths,
      stage_summary = NULL,
      participant = x@participant,
      thresholds = NULL,
      using_stage_summary = FALSE
    )
  }
}

#' Plot CPET 9-Panel Display
#'
#' @description
#' Creates the standard 9-panel CPET visualization used in clinical practice.
#' Panels include gas exchange, ventilatory, and cardiovascular responses.
#' Data points represent 30-second rolling averages at the end of each stage.
#' Linear relationships include regression lines with 95% confidence intervals.
#'
#' @param x A CpetData or CpetAnalysis object
#' @param thresholds Optional Thresholds object to overlay VT1/VT2 markers
#' @param show_peaks Logical; show peak value annotations (default TRUE)
#' @param time_axis Time axis: "seconds", "minutes", or "auto" (default)
#' @param language Language for labels: "en" or "fr" (default "en")
#' @param averaging_window Seconds to average at end of each stage (default 30)
#' @param expected_efficiency Gross mechanical efficiency for expected VO2 line
#'   (default 0.20). Set to NULL to hide expected line.
#' @param modality Exercise modality: "cycling", "treadmill", or NULL for
#'   auto-detection from data columns (default NULL)
#' @param dark Logical; use the dark `theme_cardiometr_dark()` theme
#'   (default FALSE).
#'
#' @return A ggplot2 patchwork object with 9 panels
#'
#' @examples
#' \dontrun{
#' data <- read_cosmed("cpet_file.xlsx")
#' plot_cpet_panel(data)
#' }
#'
#' @export
plot_cpet_panel <- function(x,
                            thresholds = NULL,
                            show_peaks = TRUE,
                            time_axis = "auto",
                            language = "en",
                            averaging_window = 30,
                            expected_efficiency = 0.20,
                            modality = NULL,
                            dark = FALSE) {
  if (!requireNamespace("patchwork", quietly = TRUE)) {
    cli::cli_abort(c(
      "The {.pkg patchwork} package is required for the 9-panel plot",
      "i" = "Install it with {.code install.packages('patchwork')}"
    ))
  }
  # Extract data and config from CpetAnalysis if needed
  protocol_config <- NULL
  if (inherits(x, "CpetAnalysis") || grepl("CpetAnalysis$", class(x)[1])) {
    data <- x@data
    protocol_config <- x@protocol_config
    if (is.null(thresholds) && !is.null(x@thresholds)) {
      thresholds <- x@thresholds
    }
  } else {
    data <- x
  }

  breaths <- data@breaths
  if (!"stage" %in% names(breaths)) {
    stages_join <- tryCatch(data@stages, error = function(e) NULL)
    if (is.data.frame(stages_join) && all(c("time_s", "stage") %in% names(stages_join))) {
      breaths <- dplyr::left_join(breaths, stages_join[, c("time_s", "stage")],
                                   by = "time_s")
    }
  }
  breaths <- filter_exercise_data(breaths)
  weight_kg <- data@participant@weight_kg

  # Auto-detect modality if not specified
  if (is.null(modality)) {
    if ("speed_kmh" %in% names(breaths) && any(!is.na(breaths$speed_kmh))) {
      modality <- "treadmill"
    } else if ("power_w" %in% names(breaths) && any(!is.na(breaths$power_w))) {
      modality <- "cycling"
    }
  }

  # Use pre-computed stage summary if available, otherwise calculate
  if (inherits(x, "CpetAnalysis") &&
      !is.null(x@stage_summary) && nrow(x@stage_summary) > 0) {
    stage_avg <- x@stage_summary
  } else {
    stage_avg <- calculate_stage_averages(breaths, window_seconds = averaging_window,
                                          protocol_config = protocol_config)
  }

  # Determine time unit
  max_time <- max(breaths$time_s, na.rm = TRUE)
  if (time_axis == "auto") {
    time_axis <- if (max_time > 300) "minutes" else "seconds"
  }

  if (time_axis == "minutes") {
    stage_avg <- stage_avg |>
      dplyr::mutate(time_plot = time_s / 60)
    time_label <- if (language == "fr") "Temps (min)" else "Time (min)"
  } else {
    stage_avg <- stage_avg |>
      dplyr::mutate(time_plot = time_s)
    time_label <- if (language == "fr") "Temps (s)" else "Time (s)"
  }

  peak_point <- NULL
  if (show_peaks) {
    peaks <- NULL
    if (inherits(x, "CpetAnalysis") || grepl("CpetAnalysis$", class(x)[1])) {
      peaks <- x@peaks
    }
    if (is.null(peaks)) {
      peaks <- tryCatch(find_peaks(data), error = function(e) NULL)
    }

    if (!is.null(peaks) && length(peaks@vo2_peak) > 0 && !anyNA(peaks@vo2_peak)) {
      peak_idx <- which.min(abs(breaths$vo2_ml - peaks@vo2_peak))
      peak_time_s <- breaths$time_s[peak_idx]
      peak_power <- if ("power_w" %in% names(breaths)) breaths$power_w[peak_idx] else NA_real_
      peak_hr <- if (!is.null(peaks@hr_peak)) peaks@hr_peak else NA_real_

      peak_point <- list(
        time_s = peak_time_s,
        time_plot = if (time_axis == "minutes") peak_time_s / 60 else peak_time_s,
        power_w = peak_power,
        vo2_peak = peaks@vo2_peak,
        hr_peak = peak_hr
      )
    }
  }

  # Common theme - cardiometR palette, tuned to panel size
  theme_cpet <- theme_cardiometr(base_size = 9, dark = dark) +
    ggplot2::theme(
      axis.title = ggplot2::element_text(size = 8, face = "bold"),
      axis.text = ggplot2::element_text(size = 7),
      plot.title = ggplot2::element_text(size = 9, face = "bold", hjust = 0.5),
      legend.text = ggplot2::element_text(size = 7),
      legend.key.size = ggplot2::unit(0.4, "lines"),
      plot.margin = ggplot2::margin(5, 5, 5, 5)
    )
  pal <- palette_cardiometr()

  # Resolve CpetAnalysis for VT band overlays (may be NULL when x is CpetData)
  analysis_for_bands <- if (inherits(x, "CpetAnalysis") ||
                            grepl("CpetAnalysis$", class(x)[1])) x else NULL
  vt_bands_vo2 <- if (!is.null(analysis_for_bands)) {
    vt_band_layers(analysis_for_bands, thresholds, x_unit = "vo2")
  } else list()

  # Panel 1: O2 Pulse vs Power - KEY RELATIONSHIP (stroke volume response)
  if ("power_w" %in% names(stage_avg) && !all(is.na(stage_avg$power_w)) &&
      any(stage_avg$power_w > 0, na.rm = TRUE) &&
      "hr_bpm" %in% names(stage_avg) && !all(is.na(stage_avg$hr_bpm))) {
    # Filter to exercise phase (power > 0)
    stage_ex <- stage_avg |>
      dplyr::filter(power_w > 0) |>
      dplyr::mutate(o2_pulse = vo2_ml / hr_bpm)
    p1 <- ggplot2::ggplot(stage_ex, ggplot2::aes(x = power_w, y = o2_pulse)) +
      ggplot2::geom_point(size = 2.5, alpha = 0.9, color = "#2E86AB") +
      ggplot2::geom_line(color = "#2E86AB", linewidth = 0.8, alpha = 0.6) +
      ggplot2::labs(
        title = if (language == "fr") "Pouls O\u2082 vs Puissance" else "O\u2082 Pulse vs Power",
        x = if (language == "fr") "Puissance (W)" else "Power (W)",
        y = if (language == "fr") "V\u0307O\u2082/FC (mL/batt)" else "V\u0307O\u2082/HR (mL/beat)"
      ) +
      theme_cpet
  } else if ("hr_bpm" %in% names(stage_avg) && !all(is.na(stage_avg$hr_bpm))) {
    stage_o2p <- stage_avg |>
      dplyr::mutate(o2_pulse = vo2_ml / hr_bpm)
    p1 <- ggplot2::ggplot(stage_o2p, ggplot2::aes(x = time_plot, y = o2_pulse)) +
      ggplot2::geom_point(size = 2.5, alpha = 0.9, color = "#2E86AB") +
      ggplot2::geom_line(color = "#2E86AB", linewidth = 0.8, alpha = 0.6) +
      ggplot2::labs(
        title = if (language == "fr") "Pouls O\u2082" else "O\u2082 Pulse",
        x = time_label,
        y = if (language == "fr") "V\u0307O\u2082/FC (mL/batt)" else "V\u0307O\u2082/HR (mL/beat)"
      ) +
      theme_cpet
  } else {
    p1 <- ggplot2::ggplot() +
      ggplot2::annotate("text", x = 0.5, y = 0.5,
                        label = if (language == "fr") "Donn\u00e9es non disponibles" else "Data not available") +
      ggplot2::theme_void()
  }

  # Panel 2: VO2 vs Power - LINEAR RELATIONSHIP
  if ("power_w" %in% names(stage_avg) && !all(is.na(stage_avg$power_w)) &&
      any(stage_avg$power_w > 0, na.rm = TRUE)) {
    stage_ex <- stage_avg |> dplyr::filter(power_w > 0)
    p2 <- ggplot2::ggplot(stage_ex, ggplot2::aes(x = power_w, y = vo2_ml)) +
      ggplot2::geom_point(size = 2.5, alpha = 0.9, color = "#E94F37") +
      ggplot2::geom_smooth(method = "lm", se = TRUE, color = "#E94F37",
                           fill = "#E94F37", alpha = 0.2, linewidth = 1) +
      ggplot2::labs(
        title = if (language == "fr") "V\u0307O\u2082 vs Puissance" else "V\u0307O\u2082 vs Power",
        x = if (language == "fr") "Puissance (W)" else "Power (W)",
        y = "V\u0307O\u2082 (mL/min)"
      ) +
      theme_cpet

    # Overlay expected VO2 line
    if (!is.null(expected_efficiency) && modality == "cycling") {
      power_seq <- seq(min(stage_ex$power_w, na.rm = TRUE),
                       max(stage_ex$power_w, na.rm = TRUE),
                       length.out = 50)
      expected_df <- tibble::tibble(
        power_w = power_seq,
        vo2_expected = calculate_expected_vo2_cycling(power_seq, weight_kg)
      )
      expected_label <- if (language == "fr") "Attendu" else "Expected"
      p2 <- p2 +
        ggplot2::geom_line(data = expected_df,
                           ggplot2::aes(x = power_w, y = vo2_expected),
                           linetype = "dashed", color = "gray50", linewidth = 0.8,
                           inherit.aes = FALSE) +
        ggplot2::annotate("text",
                          x = max(expected_df$power_w),
                          y = max(expected_df$vo2_expected),
                          label = expected_label,
                          hjust = 1.1, vjust = -0.5, size = 2.5, color = "gray50")
    } else if (!is.null(expected_efficiency) && modality == "treadmill" &&
               "speed_kmh" %in% names(stage_avg)) {
      stage_ex_speed <- stage_ex |> dplyr::filter(!is.na(speed_kmh))
      if (nrow(stage_ex_speed) > 0) {
        speed_seq <- seq(min(stage_ex_speed$speed_kmh, na.rm = TRUE),
                         max(stage_ex_speed$speed_kmh, na.rm = TRUE),
                         length.out = 50)
        expected_df <- tibble::tibble(
          speed_kmh = speed_seq,
          vo2_expected = calculate_expected_vo2_treadmill(speed_seq, weight_kg)
        )
        # Map speed to power axis using the stage data relationship
        speed_power_fit <- stats::lm(power_w ~ speed_kmh, data = stage_ex_speed)
        expected_df$power_w <- stats::predict(speed_power_fit, newdata = expected_df)
        expected_label <- if (language == "fr") "Attendu" else "Expected"
        p2 <- p2 +
          ggplot2::geom_line(data = expected_df,
                             ggplot2::aes(x = power_w, y = vo2_expected),
                             linetype = "dashed", color = "gray50", linewidth = 0.8,
                             inherit.aes = FALSE) +
          ggplot2::annotate("text",
                            x = max(expected_df$power_w),
                            y = max(expected_df$vo2_expected),
                            label = expected_label,
                            hjust = 1.1, vjust = -0.5, size = 2.5, color = "gray50")
      }
    }

    if (!is.null(peak_point) && length(peak_point$power_w) > 0 &&
        !anyNA(peak_point$power_w)) {
      p2 <- p2 +
        ggplot2::annotate(
          "point",
          x = peak_point$power_w,
          y = peak_point$vo2_peak,
          color = "#C0392B",
          size = 3
        ) +
        ggplot2::annotate(
          "text",
          x = peak_point$power_w,
          y = peak_point$vo2_peak,
          label = if (language == "fr") "V\u0307O\u2082 pic" else "V\u0307O\u2082 peak",
          hjust = -0.1,
          vjust = -0.8,
          size = 3,
          color = "#C0392B"
        )
    }
  } else {
    p2 <- ggplot2::ggplot(stage_avg, ggplot2::aes(x = time_plot, y = vo2_ml)) +
      ggplot2::geom_point(size = 2.5, alpha = 0.9, color = "#E94F37") +
      ggplot2::geom_line(color = "#E94F37", linewidth = 0.8, alpha = 0.6) +
      ggplot2::labs(
        title = if (language == "fr") "V\u0307O\u2082" else "V\u0307O\u2082",
        x = time_label,
        y = "V\u0307O\u2082 (mL/min)"
      ) +
      theme_cpet
    if (!is.null(peak_point) && length(peak_point$time_plot) > 0 &&
        !anyNA(peak_point$time_plot)) {
      p2 <- p2 +
        ggplot2::annotate(
          "point",
          x = peak_point$time_plot,
          y = peak_point$vo2_peak,
          color = "#C0392B",
          size = 3
        ) +
        ggplot2::annotate(
          "text",
          x = peak_point$time_plot,
          y = peak_point$vo2_peak,
          label = if (language == "fr") "V\u0307O\u2082 pic" else "V\u0307O\u2082 peak",
          hjust = -0.1,
          vjust = -0.8,
          size = 3,
          color = "#C0392B"
        )
    }
  }

  # Panel 3: VE vs VCO2 - LINEAR RELATIONSHIP (VE/VCO2 slope)
  p3 <- ggplot2::ggplot(stage_avg, ggplot2::aes(x = vco2_ml, y = ve_l)) +
    ggplot2::geom_point(size = 2.5, alpha = 0.9, color = "#1B998B") +
    ggplot2::geom_smooth(method = "lm", se = TRUE, color = "#1B998B",
                         fill = "#1B998B", alpha = 0.2, linewidth = 1) +
    ggplot2::labs(
      title = "V\u0307E vs V\u0307CO\u2082",
      x = "V\u0307CO\u2082 (mL/min)",
      y = "VE (L/min)"
    ) +
    theme_cpet

  # Panel 4: V-Slope (VCO2 vs VO2) - KEY FOR THRESHOLD DETECTION
  p4 <- ggplot2::ggplot(stage_avg, ggplot2::aes(x = vo2_ml, y = vco2_ml)) +
    ggplot2::geom_point(size = 2.5, alpha = 0.9, color = "#6B4C9A") +
    ggplot2::geom_abline(slope = 1, intercept = 0, linetype = "dashed",
                         color = "gray50", linewidth = 0.8) +
    ggplot2::geom_smooth(method = "lm", se = TRUE, color = "#6B4C9A",
                         fill = "#6B4C9A", alpha = 0.2, linewidth = 1) +
    ggplot2::labs(
      title = "V-Slope",
      x = "V\u0307O\u2082 (mL/min)",
      y = "V\u0307CO\u2082 (mL/min)"
    ) +
    theme_cpet

  # Overlay VT bands on V-slope (panel 4, x = VO2)
  if (length(vt_bands_vo2) > 0) p4 <- Reduce(`+`, vt_bands_vo2, init = p4)

  # Panel 5: VE/VO2 and VE/VCO2 vs VO2 (Ventilatory Equivalents)
  stage_ve <- stage_avg |>
    dplyr::mutate(
      ve_vo2 = ve_l * 1000 / vo2_ml,
      ve_vco2 = ve_l * 1000 / vco2_ml
    )

  stage_ve_long <- stage_ve |>
    tidyr::pivot_longer(
      cols = c(ve_vo2, ve_vco2),
      names_to = "variable",
      values_to = "value"
    ) |>
    dplyr::mutate(
      variable = factor(variable,
                        levels = c("ve_vo2", "ve_vco2"),
                        labels = c("VE/VO2", "VE/VCO2"))
    )

  p5 <- ggplot2::ggplot(stage_ve_long, ggplot2::aes(x = vo2_ml, y = value, color = variable)) +
    ggplot2::geom_point(size = 2.5, alpha = 0.9) +
    ggplot2::geom_line(linewidth = 0.8, alpha = 0.6) +
    ggplot2::scale_color_manual(
      values = c("VE/VO2" = "#2E86AB", "VE/VCO2" = "#E94F37"),
      labels = c("V\u0307E/V\u0307O\u2082", "V\u0307E/V\u0307CO\u2082")
    ) +
    ggplot2::labs(
      title = if (language == "fr") "\u00c9quivalents ventilatoires" else "Ventilatory Equivalents",
      x = "V\u0307O\u2082 (mL/min)",
      y = NULL,
      color = NULL
    ) +
    theme_cpet +
    ggplot2::theme(legend.position = "bottom")

  # Overlay VT bands on ventilatory equivalents (panel 5, x = VO2)
  if (length(vt_bands_vo2) > 0) p5 <- Reduce(`+`, vt_bands_vo2, init = p5)

  # Panel 6: RER vs VO2
  p6 <- ggplot2::ggplot(stage_avg, ggplot2::aes(x = vo2_ml, y = rer)) +
    ggplot2::geom_point(size = 2.5, alpha = 0.9, color = "#F77F00") +
    ggplot2::geom_line(color = "#F77F00", linewidth = 0.8, alpha = 0.6) +
    ggplot2::geom_hline(yintercept = 1.0, linetype = "dashed", color = "gray50", linewidth = 0.8) +
    ggplot2::labs(
      title = "RER vs V\u0307O\u2082",
      x = "V\u0307O\u2082 (mL/min)",
      y = "RER"
    ) +
    theme_cpet

  # Overlay VT bands on RER vs VO2 (panel 6, x = VO2)
  if (length(vt_bands_vo2) > 0) p6 <- Reduce(`+`, vt_bands_vo2, init = p6)

  # Panel 7: PETO2 and PETCO2 vs VO2 (if available)
  if ("peto2_mmhg" %in% names(stage_avg) && "petco2_mmhg" %in% names(stage_avg) &&
      !all(is.na(stage_avg$peto2_mmhg))) {
    stage_pet <- stage_avg |>
      tidyr::pivot_longer(
        cols = c(peto2_mmhg, petco2_mmhg),
        names_to = "variable",
        values_to = "value"
      ) |>
      dplyr::mutate(
        variable = factor(variable,
                          levels = c("peto2_mmhg", "petco2_mmhg"),
                          labels = c("PETO2", "PETCO2"))
      )

    p7 <- ggplot2::ggplot(stage_pet, ggplot2::aes(x = vo2_ml, y = value, color = variable)) +
      ggplot2::geom_point(size = 2.5, alpha = 0.9) +
      ggplot2::geom_line(linewidth = 0.8, alpha = 0.6) +
      ggplot2::scale_color_manual(
        values = c("PETO2" = "#2E86AB", "PETCO2" = "#E94F37"),
        labels = c("P\u2091\u209cO\u2082", "P\u2091\u209cCO\u2082")
      ) +
      ggplot2::labs(
        title = if (language == "fr") "P\u2091\u209cO\u2082/P\u2091\u209cCO\u2082" else "P\u2091\u209cO\u2082/P\u2091\u209cCO\u2082",
        x = "V\u0307O\u2082 (mL/min)",
        y = "mmHg",
        color = NULL
      ) +
      theme_cpet +
      ggplot2::theme(legend.position = "bottom")
  } else {
    # Alternative: O2 Pulse vs VO2
    if ("hr_bpm" %in% names(stage_avg) && !all(is.na(stage_avg$hr_bpm))) {
      stage_o2p <- stage_avg |>
        dplyr::mutate(o2_pulse = vo2_ml / hr_bpm)

      p7 <- ggplot2::ggplot(stage_o2p, ggplot2::aes(x = vo2_ml, y = o2_pulse)) +
        ggplot2::geom_point(size = 2.5, alpha = 0.9, color = "#6B4C9A") +
        ggplot2::geom_line(color = "#6B4C9A", linewidth = 0.8, alpha = 0.6) +
        ggplot2::labs(
          title = if (language == "fr") "Pouls O\u2082" else "O\u2082 Pulse",
          x = "V\u0307O\u2082 (mL/min)",
          y = "V\u0307O\u2082/HR (mL/beat)"
        ) +
        theme_cpet
    } else {
      p7 <- ggplot2::ggplot() +
        ggplot2::annotate("text", x = 0.5, y = 0.5,
                          label = if (language == "fr") "Donn\u00e9es non disponibles" else "Data not available") +
        ggplot2::theme_void()
    }
  }

  # Overlay VT bands on panel 7 when its x-axis is VO2 (both variants use VO2)
  if (length(vt_bands_vo2) > 0) p7 <- Reduce(`+`, vt_bands_vo2, init = p7)

  # Panel 8: HR vs Power - SHOWS CARDIOVASCULAR RESPONSE
  if ("hr_bpm" %in% names(stage_avg) && !all(is.na(stage_avg$hr_bpm)) &&
      "power_w" %in% names(stage_avg) && any(stage_avg$power_w > 0, na.rm = TRUE)) {
    stage_ex <- stage_avg |> dplyr::filter(power_w > 0)

    p8 <- ggplot2::ggplot(stage_ex, ggplot2::aes(x = power_w, y = hr_bpm)) +
      ggplot2::geom_point(size = 2.5, alpha = 0.9, color = "#1B998B") +
      ggplot2::geom_smooth(method = "lm", se = TRUE, color = "#1B998B",
                           fill = "#1B998B", alpha = 0.2, linewidth = 1) +
      ggplot2::labs(
        title = if (language == "fr") "FC vs Puissance" else "HR vs Power",
        x = if (language == "fr") "Puissance (W)" else "Power (W)",
        y = if (language == "fr") "FC (bpm)" else "HR (bpm)"
      ) +
      theme_cpet
    if (!is.null(peak_point) && length(peak_point$power_w) > 0 &&
        !anyNA(peak_point$power_w) && length(peak_point$hr_peak) > 0 &&
        !anyNA(peak_point$hr_peak)) {
      p8 <- p8 +
        ggplot2::annotate(
          "point",
          x = peak_point$power_w,
          y = peak_point$hr_peak,
          color = "#0F766E",
          size = 3
        ) +
        ggplot2::annotate(
          "text",
          x = peak_point$power_w,
          y = peak_point$hr_peak,
          label = if (language == "fr") "FC max" else "HRpeak",
          hjust = -0.1,
          vjust = -0.8,
          size = 3,
          color = "#0F766E"
        )
    }
  } else if ("hr_bpm" %in% names(stage_avg) && !all(is.na(stage_avg$hr_bpm))) {
    p8 <- ggplot2::ggplot(stage_avg, ggplot2::aes(x = time_plot, y = hr_bpm)) +
      ggplot2::geom_point(size = 2.5, alpha = 0.9, color = "#1B998B") +
      ggplot2::geom_line(color = "#1B998B", linewidth = 0.8, alpha = 0.6) +
      ggplot2::labs(
        title = if (language == "fr") "Fr\u00e9quence cardiaque" else "Heart Rate",
        x = time_label,
        y = if (language == "fr") "FC (bpm)" else "HR (bpm)"
      ) +
      theme_cpet
    if (!is.null(peak_point) && length(peak_point$hr_peak) > 0 &&
        !anyNA(peak_point$hr_peak) && length(peak_point$time_plot) > 0 &&
        !anyNA(peak_point$time_plot)) {
      p8 <- p8 +
        ggplot2::annotate(
          "point",
          x = peak_point$time_plot,
          y = peak_point$hr_peak,
          color = "#0F766E",
          size = 3
        ) +
        ggplot2::annotate(
          "text",
          x = peak_point$time_plot,
          y = peak_point$hr_peak,
          label = if (language == "fr") "FC max" else "HRpeak",
          hjust = -0.1,
          vjust = -0.8,
          size = 3,
          color = "#0F766E"
        )
    }
  } else {
    p8 <- ggplot2::ggplot() +
      ggplot2::annotate("text", x = 0.5, y = 0.5,
                        label = if (language == "fr") "Donn\u00e9es non disponibles" else "Data not available") +
      ggplot2::theme_void()
  }

  # Panel 9: VE vs Time - SHOWS VENTILATORY RESPONSE PATTERN
  p9 <- ggplot2::ggplot(stage_avg, ggplot2::aes(x = time_plot, y = ve_l)) +
    ggplot2::geom_point(size = 2.5, alpha = 0.9, color = "#6B4C9A") +
    ggplot2::geom_line(color = "#6B4C9A", linewidth = 0.8, alpha = 0.6) +
    ggplot2::labs(
      title = if (language == "fr") "Ventilation vs Temps" else "Ventilation vs Time",
      x = time_label,
      y = "VE (L/min)"
    ) +
    theme_cpet


  # Combine using patchwork::wrap_plots
  combined <- patchwork::wrap_plots(
    p1, p2, p3,
    p4, p5, p6,
    p7, p8, p9,
    ncol = 3,
    nrow = 3
  )

  bg_fill <- if (isTRUE(dark)) "#212529" else "white"
  title_col <- if (isTRUE(dark)) "#E5E7EB" else "#1f2d3d"
  combined +
    patchwork::plot_annotation(
      title = sprintf("%s - %s",
                      data@participant@name,
                      as.character(data@metadata@test_date)),
      theme = ggplot2::theme(
        plot.title = ggplot2::element_text(size = 11, face = "bold",
                                           hjust = 0.5, color = title_col),
        plot.background = ggplot2::element_rect(fill = bg_fill, color = NA),
        panel.background = ggplot2::element_rect(fill = bg_fill, color = NA)
      )
    )
}


#' Plot V-Slope for Threshold Detection
#'
#' @description
#' Creates a V-slope plot (VCO2 vs VO2) with optional threshold markers.
#' Used for visual verification of ventilatory threshold detection.
#'
#' @param x A CpetData object
#' @param thresholds Optional Thresholds object
#' @param show_identity Show identity line (slope = 1)
#' @param language Language for labels
#'
#' @return A ggplot2 object
#'
#' @examples
#' \dontrun{
#' file <- system.file("extdata", "example_cosmed.xlsx", package = "cardiometR")
#' data <- read_cpet(file)
#' plot_v_slope(data)
#' }
#'
#' @export
plot_v_slope <- function(x,
                         thresholds = NULL,
                         show_identity = TRUE,
                         language = "en") {
  plot_data <- resolve_plot_data(x, prefer_stage_summary = TRUE)
  breaths <- filter_exercise_data(plot_data$data)
  if (is.null(thresholds)) thresholds <- plot_data$thresholds

  point_size <- if (plot_data$using_stage_summary) 2.4 else 1.5
  point_alpha <- if (plot_data$using_stage_summary) 0.9 else 0.6

  p <- ggplot2::ggplot(breaths, ggplot2::aes(x = vo2_ml, y = vco2_ml)) +
    ggplot2::geom_point(size = point_size, alpha = point_alpha, color = "#2E86AB") +
    ggplot2::labs(
      title = if (language == "fr") "V-Slope (V\u0307CO\u2082 vs V\u0307O\u2082)" else "V-Slope (V\u0307CO\u2082 vs V\u0307O\u2082)",
      x = "V\u0307O\u2082 (mL/min)",
      y = "V\u0307CO\u2082 (mL/min)"
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      plot.title = ggplot2::element_text(size = 14, face = "bold"),
      axis.title = ggplot2::element_text(size = 11)
    )

  if (show_identity) {
    p <- p + ggplot2::geom_abline(
      slope = 1, intercept = 0,
      linetype = "dashed", color = "gray50"
    )
  }

  # Add threshold markers if provided
  if (!is.null(thresholds) && length(thresholds@vt1_vo2) > 0 && !is.na(thresholds@vt1_vo2)) {
    vt1_vco2 <- breaths$vco2_ml[which.min(abs(breaths$vo2_ml - thresholds@vt1_vo2))]
    p <- p +
      ggplot2::geom_vline(xintercept = thresholds@vt1_vo2, color = "#E94F37", linetype = "dashed") +
      ggplot2::annotate("text", x = thresholds@vt1_vo2, y = max(breaths$vco2_ml, na.rm = TRUE),
                        label = "VT1", color = "#E94F37", hjust = -0.2, fontface = "bold")
  }

  if (!is.null(thresholds) && length(thresholds@vt2_vo2) > 0 && !is.na(thresholds@vt2_vo2)) {
    p <- p +
      ggplot2::geom_vline(xintercept = thresholds@vt2_vo2, color = "#F77F00", linetype = "dashed") +
      ggplot2::annotate("text", x = thresholds@vt2_vo2, y = max(breaths$vco2_ml, na.rm = TRUE),
                        label = "VT2", color = "#F77F00", hjust = -0.2, fontface = "bold")
  }

  p
}


#' Plot Ventilatory Equivalents
#'
#' @description
#' Plots VE/VO2 and VE/VCO2 against time or VO2 for threshold detection.
#'
#' @param x A CpetData object
#' @param x_axis X-axis variable: "time" or "vo2"
#' @param thresholds Optional Thresholds object
#' @param language Language for labels
#'
#' @return A ggplot2 object
#'
#' @examples
#' \dontrun{
#' file <- system.file("extdata", "example_cosmed.xlsx", package = "cardiometR")
#' plot_ventilatory_equivalents(read_cpet(file))
#' }
#'
#' @export
plot_ventilatory_equivalents <- function(x,
                                          x_axis = "time",
                                          thresholds = NULL,
                                          language = "en") {
  plot_data <- resolve_plot_data(x, prefer_stage_summary = TRUE)
  breaths <- filter_exercise_data(plot_data$data)
  if (is.null(thresholds)) thresholds <- plot_data$thresholds

  point_size <- if (plot_data$using_stage_summary) 2.4 else 1.2
  point_alpha <- if (plot_data$using_stage_summary) 0.9 else 0.6

  # Calculate ventilatory equivalents
  breaths <- breaths |>
    dplyr::mutate(
      ve_vo2 = ve_l * 1000 / vo2_ml,
      ve_vco2 = ve_l * 1000 / vco2_ml,
      time_min = time_s / 60
    )

  # Prepare long format for plotting
  breaths_long <- breaths |>
    tidyr::pivot_longer(
      cols = c(ve_vo2, ve_vco2),
      names_to = "variable",
      values_to = "value"
    ) |>
    dplyr::mutate(
      variable = factor(variable,
                        levels = c("ve_vo2", "ve_vco2"),
                        labels = c("VE/VO2", "VE/VCO2"))
    )

  # Set x-axis
  if (x_axis == "vo2") {
    x_var <- "vo2_ml"
    x_label <- "V\u0307O\u2082 (mL/min)"
  } else {
    x_var <- "time_min"
    x_label <- if (language == "fr") "Temps (min)" else "Time (min)"
  }

  p <- ggplot2::ggplot(breaths_long, ggplot2::aes(x = .data[[x_var]], y = value, color = variable)) +
    ggplot2::geom_point(size = point_size, alpha = point_alpha) +
    ggplot2::scale_color_manual(
      values = c("VE/VO2" = "#2E86AB", "VE/VCO2" = "#E94F37"),
      name = NULL,
      labels = c("V\u0307E/V\u0307O\u2082", "V\u0307E/V\u0307CO\u2082")
    ) +
    ggplot2::labs(
      title = if (language == "fr") "\u00c9quivalents ventilatoires" else "Ventilatory Equivalents",
      x = x_label,
      y = if (language == "fr") "\u00c9quivalent ventilatoire" else "Ventilatory Equivalent"
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      plot.title = ggplot2::element_text(size = 14, face = "bold"),
      legend.position = "bottom"
    )

  # Add threshold markers if provided
  if (!is.null(thresholds) && length(thresholds@vt1_vo2) > 0 && !is.na(thresholds@vt1_vo2)) {
    if (x_axis == "vo2") {
      p <- p + ggplot2::geom_vline(xintercept = thresholds@vt1_vo2, color = "gray40", linetype = "dashed")
    }
  }

  p
}


#' Plot Gas Exchange Time Series
#'
#' @description
#' Plots VO2, VCO2, and RER against time with optional smoothing.
#'
#' @param x A CpetData object
#' @param variables Variables to plot: any of "vo2", "vco2", "rer", "ve"
#' @param smooth Apply LOESS smoothing
#' @param normalize Normalize to body weight (show mL/kg/min for VO2)
#' @param language Language for labels
#'
#' @return A ggplot2 object
#'
#' @examples
#' \dontrun{
#' file <- system.file("extdata", "example_cosmed.xlsx", package = "cardiometR")
#' plot_gas_exchange(read_cpet(file))
#' }
#'
#' @export
plot_gas_exchange <- function(x,
                               variables = c("vo2", "vco2", "rer"),
                               smooth = FALSE,
                               normalize = FALSE,
                               language = "en") {
  plot_source <- resolve_plot_data(x, prefer_stage_summary = TRUE)
  breaths <- filter_exercise_data(plot_source$data)
  weight_kg <- plot_source$participant@weight_kg

  breaths <- breaths |>
    dplyr::mutate(time_min = time_s / 60)

  if (normalize && "vo2" %in% variables) {
    breaths <- breaths |>
      dplyr::mutate(vo2_ml = vo2_ml / weight_kg)
  }

  # Build plot data
  gas_data <- breaths |>
    dplyr::select(time_min, dplyr::any_of(c(
      vo2 = "vo2_ml",
      vco2 = "vco2_ml",
      rer = "rer",
      ve = "ve_l"
    ))) |>
    dplyr::rename_with(~ gsub("_ml|_l", "", .x))

  # Filter to requested variables
  vars_present <- intersect(variables, names(gas_data))

  if (length(vars_present) == 0) {
    cli::cli_abort("None of the requested variables found in data")
  }

  plot_long <- gas_data |>
    tidyr::pivot_longer(
      cols = dplyr::all_of(vars_present),
      names_to = "variable",
      values_to = "value"
    ) |>
    dplyr::mutate(
      variable = toupper(variable),
      variable = factor(variable, levels = toupper(variables))
    )

  # Color palette
  colors <- c(
    "VO2" = "#2E86AB",
    "VCO2" = "#E94F37",
    "RER" = "#F77F00",
    "VE" = "#1B998B"
  )

  point_size <- if (plot_source$using_stage_summary) 2.2 else 1
  point_alpha <- if (plot_source$using_stage_summary) 0.9 else 0.5

  p <- ggplot2::ggplot(plot_long, ggplot2::aes(x = time_min, y = value, color = variable)) +
    ggplot2::geom_point(size = point_size, alpha = point_alpha) +
    ggplot2::scale_color_manual(values = colors, name = NULL) +
    ggplot2::facet_wrap(~ variable, scales = "free_y", ncol = 1) +
    ggplot2::labs(
      title = if (language == "fr") "\u00c9changes gazeux" else "Gas Exchange",
      x = if (language == "fr") "Temps (min)" else "Time (min)",
      y = NULL
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      plot.title = ggplot2::element_text(size = 14, face = "bold"),
      strip.text = ggplot2::element_text(face = "bold"),
      legend.position = "none"
    )

  if (smooth) {
    p <- p + ggplot2::geom_smooth(method = "loess", se = FALSE, linewidth = 1)
  }

  p
}


#' Plot Heart Rate Response
#'
#' @description
#' Plots heart rate against time or VO2 with optional target zones.
#'
#' @param x A CpetData object
#' @param x_axis X-axis variable: "time" or "vo2"
#' @param show_zones Show training zones based on peak HR
#' @param language Language for labels
#'
#' @return A ggplot2 object
#'
#' @examples
#' \dontrun{
#' file <- system.file("extdata", "example_cosmed.xlsx", package = "cardiometR")
#' plot_heart_rate(read_cpet(file), show_zones = TRUE)
#' }
#'
#' @export
plot_heart_rate <- function(x,
                            x_axis = "time",
                            show_zones = FALSE,
                            language = "en") {
  plot_data <- resolve_plot_data(x, prefer_stage_summary = TRUE)
  breaths <- filter_exercise_data(plot_data$data)
  age <- plot_data$participant@age

  if (!"hr_bpm" %in% names(breaths) || all(is.na(breaths$hr_bpm))) {
    cli::cli_abort("Heart rate data not available")
  }

  breaths <- breaths |>
    dplyr::mutate(time_min = time_s / 60)

  # Set x-axis
  if (x_axis == "vo2") {
    x_var <- "vo2_ml"
    x_label <- "V\u0307O\u2082 (mL/min)"
  } else {
    x_var <- "time_min"
    x_label <- if (language == "fr") "Temps (min)" else "Time (min)"
  }

  point_size <- if (plot_data$using_stage_summary) 2.4 else 1.2
  point_alpha <- if (plot_data$using_stage_summary) 0.9 else 0.6

  p <- ggplot2::ggplot(breaths, ggplot2::aes(x = .data[[x_var]], y = hr_bpm)) +
    ggplot2::geom_point(size = point_size, alpha = point_alpha, color = "#E94F37") +
    ggplot2::labs(
      title = if (language == "fr") "Fr\u00e9quence cardiaque" else "Heart Rate",
      x = x_label,
      y = "HR (bpm)"
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      plot.title = ggplot2::element_text(size = 14, face = "bold")
    )

  if (show_zones) {
    # Age-predicted max HR
    hr_max <- 220 - age
    peak_hr <- max(breaths$hr_bpm, na.rm = TRUE)

    # Use actual peak if higher than predicted
    hr_ref <- max(hr_max, peak_hr)

    # Zone thresholds (% of max)
    zone_breaks <- c(0.5, 0.6, 0.7, 0.8, 0.9, 1.0) * hr_ref

    p <- p +
      ggplot2::geom_hline(yintercept = zone_breaks, linetype = "dotted", color = "gray60", alpha = 0.7)
  }

  p
}


#' Plot Power Output Response
#'
#' @description
#' Plots power output against time with VO2 overlay option.
#' When show_vo2 is TRUE and expected_efficiency is provided, overlays
#' an expected VO2 dashed line based on mechanical efficiency.
#'
#' @param x A CpetData object
#' @param show_vo2 Overlay VO2 on secondary axis
#' @param expected_efficiency Gross mechanical efficiency for expected VO2 line
#'   (default 0.20). Set to NULL to hide expected line.
#' @param language Language for labels
#'
#' @return A ggplot2 object
#'
#' @examples
#' \dontrun{
#' file <- system.file("extdata", "example_cosmed.xlsx", package = "cardiometR")
#' plot_power(read_cpet(file))
#' }
#'
#' @export
plot_power <- function(x,
                       show_vo2 = TRUE,
                       expected_efficiency = 0.20,
                       language = "en") {
  plot_data <- resolve_plot_data(x, prefer_stage_summary = TRUE)
  breaths <- filter_exercise_data(plot_data$data)
  weight_kg <- plot_data$participant@weight_kg

  if (!"power_w" %in% names(breaths) || all(is.na(breaths$power_w))) {
    cli::cli_abort("Power data not available")
  }

  breaths <- breaths |>
    dplyr::mutate(time_min = time_s / 60)

  point_size_primary <- if (plot_data$using_stage_summary) 2.4 else 1.2
  point_alpha_primary <- if (plot_data$using_stage_summary) 0.9 else 0.6
  point_size_secondary <- if (plot_data$using_stage_summary) 2.1 else 1
  point_alpha_secondary <- if (plot_data$using_stage_summary) 0.7 else 0.4

  if (show_vo2) {
    # VO2 vs Power scatter with cycling-economy iso-lines. Each line is
    # VO2 = slope . P + 7 . body_mass, parameterised by an iso-slope in
    # mL O2 per watt (cycling economy). The 8-12 mL/W range spans typical
    # trained-to-recreational cyclists (lower slope = higher efficiency).
    power_range <- range(breaths$power_w, na.rm = TRUE)
    resting <- 7 * (weight_kg %||% 75)
    slopes <- c(8, 9, 10, 11, 12)
    pw_grid <- seq(max(0, power_range[1]), power_range[2], length.out = 80)
    iso_df <- purrr::map_dfr(slopes, function(s) {
      tibble::tibble(
        power_w = pw_grid,
        vo2_ml  = s * pw_grid + resting,
        slope   = s
      )
    }) |>
      dplyr::mutate(slope = factor(slope, levels = slopes,
                                   labels = paste0(slopes, " mL/W")))

    p <- ggplot2::ggplot(
        breaths |> dplyr::filter(!is.na(power_w), power_w >= 0),
        ggplot2::aes(x = power_w, y = vo2_ml)
      ) +
      ggplot2::geom_line(
        data = iso_df,
        ggplot2::aes(x = power_w, y = vo2_ml, color = slope, group = slope),
        linewidth = 0.8, alpha = 0.9, inherit.aes = FALSE
      ) +
      ggplot2::geom_point(
        size = point_size_primary,
        alpha = point_alpha_primary,
        color = "#2E86AB"
      ) +
      ggplot2::scale_color_viridis_d(
        option = "plasma", end = 0.85, direction = -1,
        name = if (language == "fr") "\u00c9conomie" else "Economy"
      ) +
      ggplot2::labs(
        title = if (language == "fr") "V\u0307O\u2082 vs Puissance" else "V\u0307O\u2082 vs Power",
        x = if (language == "fr") "Puissance (W)" else "Power (W)",
        y = "V\u0307O\u2082 (mL/min)"
      ) +
      ggplot2::theme_minimal() +
      ggplot2::theme(
        plot.title = ggplot2::element_text(size = 14, face = "bold"),
        legend.position = "right"
      )
  } else {
    p <- ggplot2::ggplot(breaths, ggplot2::aes(x = time_min, y = power_w)) +
      ggplot2::geom_point(
        size = point_size_primary,
        alpha = point_alpha_primary,
        color = "#1B998B"
      ) +
      ggplot2::labs(
        title = if (language == "fr") "Puissance" else "Power",
        x = if (language == "fr") "Temps (min)" else "Time (min)",
        y = "Power (W)"
      ) +
      ggplot2::theme_minimal() +
      ggplot2::theme(
        plot.title = ggplot2::element_text(size = 14, face = "bold")
      )
  }

  p
}


#' Plot Measured vs Predicted/Normative Values Comparison
#'
#' @description
#' Creates a bar chart comparing measured peak values against age/sex predicted
#' values and optionally against sport-specific normative data.
#' Shows VO2max (mL/kg/min) and for athletes: cycling efficiency (GE%) or
#' running economy (mL O2/kg/km). Includes citations for reference data.
#'
#' @param x A CpetAnalysis object with peaks data
#' @param sport Sport for normative comparison: "cycling", "running", "triathlon",
#'   or NULL for general population only (default NULL)
#' @param level Competitive level: "elite", "competitive", "recreational" (default "recreational")
#' @param language Language for labels: "en" or "fr" (default "en")
#' @param show_citation Logical; show citation below plot (default TRUE)
#' @param prediction_source Prediction equation source: `"jones"` (default)
#'   or `"prefaut"`.
#'
#' @return A ggplot2 object with citation attributes for bibliography
#'
#' @examples
#' \dontrun{
#' # Compare against predicted only
#' plot_predicted_comparison(analysis)
#'
#' # Compare against elite cyclist norms (includes gross efficiency)
#' plot_predicted_comparison(analysis, sport = "cycling", level = "elite")
#'
#' # Compare against recreational runner norms in French (includes running economy)
#' plot_predicted_comparison(analysis, sport = "running", level = "recreational", language = "fr")
#' }
#'
#' @export
plot_predicted_comparison <- function(x,
                                      sport = NULL,
                                      level = "recreational",
                                      language = "en",
                                      show_citation = TRUE,
                                      prediction_source = "jones") {

  if (!inherits(x, "CpetAnalysis") && !grepl("CpetAnalysis$", class(x)[1])) {
    cli::cli_abort("x must be a CpetAnalysis object")
  }

  peaks <- x@peaks
  participant <- x@data@participant

  if (is.null(peaks) || length(peaks@vo2_peak) == 0) {
    cli::cli_abort("No peak values available in analysis")
  }

  # Calculate predicted values
  predicted <- calculate_predicted_values(participant, prediction_source = prediction_source)

  # Get normative data if sport specified
  norms <- NULL
  citations <- list()
  if (!is.null(sport)) {
    norms <- get_normative_data(sport, level, participant@sex, participant@age)
    citations$vo2max <- norms$citation
    citations$vo2max_short <- norms$citation_short
  }

  # Always add predicted citation
  citations$predicted <- predicted$citation
  citations$predicted_short <- predicted$citation_short

  # Use VO2 in mL/kg/min for athlete comparisons (more meaningful)
  vo2_measured <- peaks@vo2_kg_peak
  vo2_predicted <- predicted$vo2_max_rel

  # Calculate efficiency/economy based on sport
  efficiency_measured <- NULL
  efficiency_norm <- NULL
  efficiency_label <- NULL

  if (!is.null(sport) && sport == "cycling" && !is.null(peaks@power_peak)) {
    # Calculate gross efficiency for cycling
    efficiency_measured <- calculate_gross_efficiency(
      vo2_ml = peaks@vo2_peak,
      power_w = peaks@power_peak,
      rer = peaks@rer_peak
    )
    efficiency_norm <- norms$efficiency_typical
    efficiency_label <- if (language == "fr") {
      "Efficacit\u00e9\n(%)"
    } else {
      "Efficiency\n(GE%)"
    }
    citations$efficiency <- norms$efficiency_citation
    citations$efficiency_short <- norms$efficiency_citation_short
  } else if (!is.null(sport) && sport == "running") {
    # Running economy not directly measured in CPET - use normative reference only
    efficiency_norm <- norms$economy_typical
    efficiency_label <- if (language == "fr") {
      "\u00c9conomie\n(mL/kg/km)"
    } else {
      "Economy\n(mL/kg/km)"
    }
    citations$economy <- norms$economy_citation
    citations$economy_short <- norms$economy_citation_short
  }

  # Build comparison data - VO2max only for main comparison
  labels_en <- "V\u0307O\u2082max
(mL/kg/min)"
  labels_fr <- "V\u0307O\u2082max
(mL/kg/min)"

  # Create data frame for VO2max plot
  plot_data <- tibble::tibble(
    parameter = factor(if (language == "fr") labels_fr else labels_en),
    measured = vo2_measured,
    predicted = vo2_predicted
  )

  # Add normative range if available
  if (!is.null(norms)) {
    plot_data$norm_typical <- norms$vo2max_typical
  }

  plot_data <- plot_data |>
    dplyr::mutate(
      pct_predicted = round(100 * measured / predicted, 0),
      pct_label = paste0(pct_predicted, "%")
    )

  # Reshape for grouped bar chart
  if (!is.null(norms)) {
    plot_long <- plot_data |>
      dplyr::select(parameter, measured, predicted, norm_typical) |>
      tidyr::pivot_longer(
        cols = c(measured, predicted, norm_typical),
        names_to = "type",
        values_to = "value"
      ) |>
      dplyr::filter(!is.na(value)) |>
      dplyr::mutate(
        type = factor(type,
                      levels = c("predicted", "norm_typical", "measured"),
                      labels = if (language == "fr") {
                        c("Pr\u00e9dit\n(population)", "Norme\nathl\u00e8tes", "Mesur\u00e9")
                      } else {
                        c("Predicted\n(population)", "Athlete\nnorm", "Measured")
                      })
      )

    fill_values <- c(
      "Predicted\n(population)" = "#E0E0E0",
      "Athlete\nnorm" = "#F77F00",
      "Measured" = "#2E86AB",
      "Pr\u00e9dit\n(population)" = "#E0E0E0",
      "Norme\nathl\u00e8tes" = "#F77F00",
      "Mesur\u00e9" = "#2E86AB"
    )
  } else {
    plot_long <- plot_data |>
      dplyr::select(parameter, measured, predicted) |>
      tidyr::pivot_longer(
        cols = c(measured, predicted),
        names_to = "type",
        values_to = "value"
      ) |>
      dplyr::mutate(
        type = factor(type,
                      levels = c("predicted", "measured"),
                      labels = if (language == "fr") c("Pr\u00e9dit", "Mesur\u00e9") else c("Predicted", "Measured"))
      )

    fill_values <- c(
      "Predicted" = "#CCCCCC", "Measured" = "#2E86AB",
      "Pr\u00e9dit" = "#CCCCCC", "Mesur\u00e9" = "#2E86AB"
    )
  }

  # Add efficiency panel if available (cycling only for now)
  if (!is.null(efficiency_measured) && !is.null(efficiency_norm)) {
    eff_data <- tibble::tibble(
      parameter = factor(efficiency_label),
      measured = efficiency_measured,
      predicted = NA_real_,
      norm_typical = efficiency_norm
    ) |>
      dplyr::mutate(
        pct_predicted = NA_real_,
        pct_label = sprintf("%.1f%%", measured)
      )

    eff_long <- eff_data |>
      dplyr::select(parameter, measured, norm_typical) |>
      tidyr::pivot_longer(
        cols = c(measured, norm_typical),
        names_to = "type",
        values_to = "value"
      ) |>
      dplyr::filter(!is.na(value)) |>
      dplyr::mutate(
        type = factor(type,
                      levels = c("norm_typical", "measured"),
                      labels = if (language == "fr") {
                        c("Norme\nathl\u00e8tes", "Mesur\u00e9")
                      } else {
                        c("Athlete\nnorm", "Measured")
                      })
      )

    plot_long <- dplyr::bind_rows(plot_long, eff_long)
    plot_data <- dplyr::bind_rows(plot_data, eff_data)
  }

  # Build subtitle
  if (!is.null(norms)) {
    subtitle_text <- if (language == "fr") {
      sprintf("Comparaison: %s, %d ans | Norme: %s",
              if (participant@sex == "M") "homme" else "femme",
              participant@age,
              norms$description)
    } else {
      sprintf("Comparison: %s, age %d | Norm: %s",
              if (participant@sex == "M") "male" else "female",
              participant@age,
              norms$description)
    }
  } else {
    subtitle_text <- if (language == "fr") {
      sprintf("Valeurs pr\u00e9dites pour %s, %d ans (%s)",
              if (participant@sex == "M") "homme" else "femme",
              participant@age,
              predicted$citation_short)
    } else {
      sprintf("Predicted values for %s, age %d (%s)",
              if (participant@sex == "M") "male" else "female",
              participant@age,
              predicted$citation_short)
    }
  }

  # Build caption with citations
  caption_parts <- c()
  if (!is.null(norms)) {
    caption_parts <- c(caption_parts, paste0("VO2max: ", norms$citation_short))
    if (!is.null(citations$efficiency_short)) {
      caption_parts <- c(caption_parts, paste0("GE: ", citations$efficiency_short))
    }
    if (!is.null(citations$economy_short)) {
      caption_parts <- c(caption_parts, paste0("RE: ", citations$economy_short))
    }
  }
  caption_parts <- c(caption_parts, paste0("Pred: ", predicted$citation_short))
  caption_text <- if (show_citation) paste(caption_parts, collapse = " | ") else NULL

  # Create the plot
  p <- ggplot2::ggplot(plot_long, ggplot2::aes(x = type, y = value, fill = type)) +
    ggplot2::geom_col(width = 0.7) +
    ggplot2::facet_wrap(~ parameter, scales = "free", nrow = 1) +
    ggplot2::scale_fill_manual(values = fill_values, name = "") +
    ggplot2::labs(
      title = if (language == "fr") "Valeurs Mesur\u00e9es vs R\u00e9f\u00e9rences" else "Measured vs Reference Values",
      subtitle = subtitle_text,
      x = "",
      y = if (language == "fr") "Valeur" else "Value",
      caption = caption_text
    ) +
    ggplot2::theme_minimal(base_size = 11) +
    ggplot2::theme(
      plot.title = ggplot2::element_text(size = 14, face = "bold", hjust = 0.5),
      plot.subtitle = ggplot2::element_text(size = 9, hjust = 0.5, color = "gray40"),
      plot.caption = ggplot2::element_text(size = 7, hjust = 0, color = "gray50", face = "italic"),
      strip.text = ggplot2::element_text(size = 11, face = "bold"),
      legend.position = "bottom",
      panel.grid.major.x = ggplot2::element_blank(),
      axis.text.x = ggplot2::element_text(size = 8)
    )

  # Store all citations as attributes for bibliography generation
  attr(p, "citations") <- citations
  if (!is.null(norms)) {
    attr(p, "citation") <- norms$citation
    attr(p, "citation_short") <- norms$citation_short
  }

  p
}


# ---------------------------------------------------------------------------
# Phase 4 additive plots: VO2-Power slope, Z-score strip, Longitudinal delta
# ---------------------------------------------------------------------------

#' Plot VO2-Power Submax Slope
#'
#' @description
#' Scatter of VO2 vs Power over the submax portion of the test with a linear
#' fit and 95% confidence ribbon. Submax cutoff is VT2 (from
#' `analysis@vt2_range[1]`) when available, else 85% of `analysis@ppo_watts`.
#' The slope +/- 95% CI (mL.min^-1.W^-1) is shown as a caption.
#'
#' @param analysis A CpetAnalysis S7 object.
#' @param language Language code (`"en"` or `"fr"`).
#' @param dark Logical; use the dark `theme_cardiometr_dark()` theme.
#' @return A ggplot2 object.
#' @examples
#' \dontrun{
#' plot_vo2_power_slope(analysis, language = "en")
#' }
#' @export
plot_vo2_power_slope <- function(analysis, language = "en", dark = FALSE) {
  pal <- palette_cardiometr()
  placeholder <- function(msg = tr("insufficient_stratum_data", language)) {
    ggplot2::ggplot() +
      ggplot2::annotate("text", x = 0.5, y = 0.5, label = msg,
                        color = "gray50", size = 4) +
      ggplot2::scale_x_continuous(limits = c(0, 1)) +
      ggplot2::scale_y_continuous(limits = c(0, 1)) +
      ggplot2::labs(title = tr("vo2_power_slope_title", language),
                    x = NULL, y = NULL) +
      theme_cardiometr(dark = dark) +
      ggplot2::theme(axis.text = ggplot2::element_blank(),
                     axis.ticks = ggplot2::element_blank())
  }
  breaths <- tryCatch(analysis@data@breaths, error = function(e) NULL)
  # Bail out for tests without power_w (e.g. treadmill): return placeholder gg
  if (is.null(breaths) || !"power_w" %in% names(breaths) ||
      !any(is.finite(breaths$power_w))) {
    return(placeholder())
  }
  stage_summary <- tryCatch(analysis@stage_summary, error = function(e) NULL)
  stages_df <- tryCatch(analysis@data@stages, error = function(e) NULL)

  vt2_power <- tryCatch({
    vt2r <- analysis@vt2_range
    if (is.numeric(vt2r) && length(vt2r) == 2) vt2r[1] else NULL
  }, error = function(e) NULL)

  cutoff <- vt2_power
  if (is.null(cutoff) || !is.finite(cutoff)) {
    ppo <- tryCatch(analysis@ppo_watts, error = function(e) NA_real_)
    cutoff <- if (is.numeric(ppo) && is.finite(ppo)) 0.85 * ppo else NA_real_
  }

  df <- if (!is.null(breaths) &&
            all(c("vo2_ml", "power_w") %in% names(breaths))) {
    d <- breaths |>
      dplyr::select(dplyr::any_of(c("time_s", "vo2_ml", "power_w"))) |>
      dplyr::filter(!is.na(.data$vo2_ml), !is.na(.data$power_w),
                    .data$power_w > 20)
    if (!is.null(stages_df) && is.data.frame(stages_df) &&
        all(c("time_s", "stage") %in% names(stages_df)) &&
        "time_s" %in% names(d)) {
      ex_rows <- stages_df |> dplyr::filter(!is.na(.data$stage), .data$stage > 0)
      if (nrow(ex_rows) > 0) {
        ex_start <- min(ex_rows$time_s, na.rm = TRUE)
        d <- d |> dplyr::filter(.data$time_s >= ex_start)
      }
    }
    if (is.finite(cutoff)) dplyr::filter(d, .data$power_w <= cutoff) else d
  } else NULL

  fit <- tryCatch(analysis@vo2_power_slope, error = function(e) NULL)
  if (is.null(fit) || !is.finite(fit$slope %||% NA_real_)) {
    fit <- tryCatch(
      fit_vo2_power_slope(breaths, stage_summary, vt2_power = cutoff,
                          stages = stages_df),
      error = function(e) NULL
    )
  }

  p <- ggplot2::ggplot()
  if (!is.null(df) && nrow(df) >= 2) {
    p <- p +
      ggplot2::geom_point(data = df,
                          ggplot2::aes(x = .data$power_w, y = .data$vo2_ml),
                          color = pal[["patient"]], size = 2, alpha = 0.7) +
      ggplot2::geom_smooth(data = df,
                           ggplot2::aes(x = .data$power_w, y = .data$vo2_ml),
                           method = "lm", formula = y ~ x, se = TRUE,
                           color = pal[["patient"]], fill = pal[["patient"]],
                           alpha = 0.2, linewidth = 0.9)
  } else {
    p <- p + ggplot2::annotate("text", x = 0.5, y = 0.5,
                               label = tr("insufficient_stratum_data", language),
                               color = "gray50")
  }

  caption <- NULL
  if (!is.null(fit) && is.finite(fit$slope %||% NA_real_)) {
    slope_s <- sprintf("%.2f", fit$slope)
    ci_s <- sprintf("[%.2f, %.2f]",
                    fit$slope_ci_low %||% NA_real_,
                    fit$slope_ci_high %||% NA_real_)
    cap_tpl <- tr("vo2_power_slope_caption", language)
    caption <- gsub("\\{slope\\}", slope_s,
                    gsub("\\{ci\\}", ci_s, cap_tpl))
  }

  p +
    ggplot2::labs(
      title = tr("vo2_power_slope_title", language),
      x = if (language == "fr") "Puissance (W)" else "Power (W)",
      y = "V\u0307O\u2082 (mL/min)",
      caption = caption
    ) +
    theme_cardiometr(dark = dark)
}


#' Plot Z-Score Strip for Key Metrics
#'
#' @description
#' Horizontal strip plot: one row per metric. Shows a grey u+/-1 SD band
#' (in z-units), dashed LLN/ULN at +/-1.645, the patient's z as a coloured dot,
#' and a right-aligned annotation with z and percentile. Metrics without a
#' valid z-score are rendered with a subdued placeholder label.
#'
#' @param analysis A CpetAnalysis S7 object (uses `@z_scores`).
#' @param metrics Character vector of metric keys drawn from
#'   `analysis@z_scores` (strip the `_z` suffix -- e.g. `"vo2_peak"`
#'   maps to `vo2_peak_z`).
#' @param language Language code.
#' @param dark Logical; use the dark `theme_cardiometr_dark()` theme.
#' @return A ggplot2 object.
#' @examples
#' \dontrun{
#' plot_zscore_strip(analysis)
#' }
#' @export
plot_zscore_strip <- function(analysis,
                              metrics = c("vo2_peak", "map_per_kg", "ppo"),
                              language = "en",
                              dark = FALSE) {
  pal <- palette_cardiometr()
  zs <- tryCatch(analysis@z_scores, error = function(e) NULL)
  # Early bail-out: no z-score data at all -> placeholder gg (never NULL)
  has_any_z <- is.list(zs) && length(zs) > 0 && any(vapply(zs, function(e) {
    z <- if (is.list(e)) e$z else e
    is.numeric(z) && length(z) >= 1 && is.finite(z[1])
  }, logical(1)))
  if (!has_any_z) {
    return(
      ggplot2::ggplot() +
        ggplot2::annotate("text", x = 0.5, y = 0.5,
                          label = tr("insufficient_stratum_data", language),
                          color = "gray50", size = 4) +
        ggplot2::scale_x_continuous(limits = c(0, 1)) +
        ggplot2::scale_y_continuous(limits = c(0, 1)) +
        ggplot2::labs(x = NULL, y = NULL) +
        theme_cardiometr(dark = dark) +
        ggplot2::theme(axis.text = ggplot2::element_blank(),
                       axis.ticks = ggplot2::element_blank())
    )
  }

  label_for <- function(m) {
    switch(m,
      vo2_peak   = if (language == "fr") "VO2pic" else "VO2peak",
      map_per_kg = if (language == "fr") "PMA/kg" else "MAP/kg",
      ppo        = if (language == "fr") "PPO" else "PPO",
      m)
  }

  rows <- purrr::map_dfr(metrics, function(m) {
    key <- paste0(m, "_z")
    entry <- if (is.list(zs)) zs[[key]] else NULL
    z <- if (is.list(entry)) entry$z else entry
    z <- suppressWarnings(as.numeric(z))
    if (!is.numeric(z) || !is.finite(z)) z <- NA_real_
    tibble::tibble(metric = label_for(m), z = z)
  })
  rows$metric <- factor(rows$metric, levels = rev(rows$metric))

  x_range <- c(-3, 3)
  finite_z <- rows$z[is.finite(rows$z)]
  if (length(finite_z) > 0) {
    x_range <- range(c(x_range, finite_z), na.rm = TRUE)
  }

  annot <- rows |>
    dplyr::mutate(
      label = dplyr::if_else(
        is.finite(.data$z),
        sprintf("z = %+.2f  (%s %.0f)",
                .data$z,
                tr("percentile_label", language),
                percentile_from_z(.data$z)),
        tr("insufficient_stratum_data", language)
      )
    )

  ggplot2::ggplot(rows, ggplot2::aes(x = .data$z, y = .data$metric)) +
    ggplot2::annotate("rect", xmin = -1, xmax = 1,
                      ymin = -Inf, ymax = Inf,
                      fill = pal[["stratum_band"]], alpha = 0.25) +
    ggplot2::geom_vline(xintercept = c(-1.645, 1.645),
                        color = pal[["stratum_band"]],
                        linetype = "dashed", linewidth = 0.5) +
    ggplot2::geom_vline(xintercept = 0, color = "gray60", linewidth = 0.3) +
    ggplot2::geom_point(data = dplyr::filter(rows, is.finite(.data$z)),
                        color = pal[["patient"]], size = 4) +
    ggplot2::geom_text(data = annot,
                       ggplot2::aes(x = max(x_range), label = .data$label),
                       hjust = 1, vjust = -0.7, size = 3,
                       color = ifelse(
                         is.finite(annot$z),
                         if (isTRUE(dark)) "#E5E7EB" else "black",
                         "gray60")) +
    ggplot2::scale_x_continuous(limits = x_range) +
    ggplot2::labs(
      x = tr("z_score_axis", language),
      y = NULL,
      title = NULL
    ) +
    theme_cardiometr(dark = dark)
}


#' Plot Longitudinal Delta Between Two CPET Tests
#'
#' @description
#' Dumbbell comparator across VO2peak, MAP/kg and PPO for a current vs prior
#' test. A typical-error band (+/-3 percent per Hopkins 2001) is shaded around
#' the prior value; the current value is coloured as "beyond typical error"
#' when the change exceeds the band, otherwise as "within noise". Returns
#' `NULL` when `prior_analysis` is NULL.
#'
#' @param current_analysis A CpetAnalysis S7 object (required).
#' @param prior_analysis A CpetAnalysis S7 object, or NULL.
#' @param language Language code.
#' @param dark Logical; use the dark `theme_cardiometr_dark()` theme.
#' @return A ggplot2 object, or NULL when no prior test is supplied.
#' @examples
#' \dontrun{
#' plot_longitudinal_delta(current, prior)
#' }
#' @export
plot_longitudinal_delta <- function(current_analysis,
                                    prior_analysis,
                                    language = "en",
                                    dark = FALSE) {
  if (is.null(prior_analysis)) return(NULL)
  pal <- palette_cardiometr()

  get_metric <- function(a, key) {
    tryCatch({
      val <- switch(key,
        vo2_peak   = a@peaks@vo2_kg_peak,
        map_per_kg = a@map_per_kg,
        ppo        = a@ppo_watts,
        NA_real_)
      if (is.numeric(val) && length(val) >= 1) as.numeric(val[1]) else NA_real_
    }, error = function(e) NA_real_)
  }

  metric_label <- function(m) switch(m,
    vo2_peak   = if (language == "fr") "VO2pic (mL/kg/min)" else "VO2peak (mL/kg/min)",
    map_per_kg = if (language == "fr") "PMA/kg (W/kg)" else "MAP/kg (W/kg)",
    ppo        = if (language == "fr") "PPO (W)" else "PPO (W)",
    m)

  metrics <- c("vo2_peak", "map_per_kg", "ppo")
  typical_error <- 0.03  # Hopkins 2001, \u00b13 percent

  df <- purrr::map_dfr(metrics, function(m) {
    prior <- get_metric(prior_analysis, m)
    curr <- get_metric(current_analysis, m)
    tibble::tibble(
      metric = metric_label(m),
      prior = prior,
      current = curr,
      te_low = prior * (1 - typical_error),
      te_high = prior * (1 + typical_error),
      beyond = is.finite(prior) && is.finite(curr) &&
               (curr < prior * (1 - typical_error) ||
                curr > prior * (1 + typical_error))
    )
  })
  df$metric <- factor(df$metric, levels = rev(df$metric))
  df$color_key <- ifelse(df$beyond,
                         tr("beyond_typical_error", language),
                         tr("within_noise", language))

  ggplot2::ggplot(df) +
    ggplot2::geom_errorbarh(
      ggplot2::aes(y = .data$metric,
                   xmin = .data$te_low, xmax = .data$te_high),
      color = pal[["stratum_band"]], height = 0.25, linewidth = 3, alpha = 0.4
    ) +
    ggplot2::geom_segment(
      ggplot2::aes(y = .data$metric, yend = .data$metric,
                   x = .data$prior, xend = .data$current),
      color = "gray60", linewidth = 0.6
    ) +
    ggplot2::geom_point(ggplot2::aes(y = .data$metric, x = .data$prior),
                        color = pal[["stratum_band"]], size = 3) +
    ggplot2::geom_point(
      ggplot2::aes(y = .data$metric, x = .data$current,
                   color = .data$color_key),
      size = 4
    ) +
    ggplot2::scale_color_manual(
      values = stats::setNames(
        c(pal[["vermillion"]], pal[["bluish_green"]]),
        c(tr("beyond_typical_error", language),
          tr("within_noise", language))
      ),
      name = NULL
    ) +
    ggplot2::labs(
      title = tr("longitudinal_title", language),
      x = NULL, y = NULL,
      caption = tr("typical_error_band", language)
    ) +
    theme_cardiometr(dark = dark)
}

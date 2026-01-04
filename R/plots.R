# Visualization Functions for cardiometR
# CPET-specific plots based on clinical guidelines

#' Calculate stage averages for CPET data
#'
#' @description
#' Calculates rolling 30-second averages at the end of each power stage.
#' Uses power levels to identify stages and averages the last 30 seconds of each.
#'
#' @param breaths Data frame with breath-by-breath data
#' @param window_seconds Averaging window in seconds (default 30)
#'
#' @return Data frame with one row per stage containing averaged values
#'
#' @keywords internal
calculate_stage_averages <- function(breaths, window_seconds = 30) {
  # Identify unique power stages
 power_stages <- unique(breaths$power_w)
  power_stages <- sort(power_stages[!is.na(power_stages)])

  # For each stage, get the last 30 seconds of data
  stage_data <- lapply(power_stages, function(pwr) {
    stage_breaths <- breaths |>
      dplyr::filter(power_w == pwr)

    if (nrow(stage_breaths) == 0) return(NULL)

    # Get max time for this stage and filter to last 30 seconds
    max_time <- max(stage_breaths$time_s, na.rm = TRUE)
    min_time_window <- max_time - window_seconds

    stage_end <- stage_breaths |>
      dplyr::filter(time_s >= min_time_window)

    if (nrow(stage_end) == 0) stage_end <- stage_breaths

    # Calculate means for all numeric columns
    numeric_cols <- names(stage_end)[sapply(stage_end, is.numeric)]

    averages <- stage_end |>
      dplyr::summarise(
        dplyr::across(dplyr::all_of(numeric_cols), ~ mean(.x, na.rm = TRUE))
      ) |>
      dplyr::mutate(
        power_w = pwr,
        n_breaths = nrow(stage_end)
      )

    averages
  })

  # Combine all stages
  dplyr::bind_rows(stage_data) |>
    dplyr::filter(!is.na(vo2_ml))
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
                            averaging_window = 30) {
  # Extract data from CpetAnalysis if needed
  if (inherits(x, "CpetAnalysis") || grepl("CpetAnalysis$", class(x)[1])) {
    data <- x@data
    if (is.null(thresholds) && !is.null(x@thresholds)) {
      thresholds <- x@thresholds
    }
  } else {
    data <- x
  }

  breaths <- data@breaths

  # Calculate stage averages (30-second rolling averages at end of each stage)
  stage_avg <- calculate_stage_averages(breaths, window_seconds = averaging_window)

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

  # Common theme - clean and professional
  theme_cpet <- ggplot2::theme_minimal(base_size = 9) +
    ggplot2::theme(
      panel.grid.minor = ggplot2::element_blank(),
      panel.grid.major = ggplot2::element_line(color = "gray90", linewidth = 0.3),
      axis.title = ggplot2::element_text(size = 8),
      axis.text = ggplot2::element_text(size = 7),
      plot.title = ggplot2::element_text(size = 9, face = "bold", hjust = 0.5),
      legend.text = ggplot2::element_text(size = 7),
      legend.key.size = ggplot2::unit(0.4, "lines"),
      plot.margin = ggplot2::margin(5, 5, 5, 5)
    )

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
        title = if (language == "fr") expression(Pouls~O[2]~vs~Puissance) else expression(O[2]~Pulse~vs~Power),
        x = if (language == "fr") "Puissance (W)" else "Power (W)",
        y = expression(VO[2]/FC~(mL/batt))
      ) +
      theme_cpet
  } else if ("hr_bpm" %in% names(stage_avg) && !all(is.na(stage_avg$hr_bpm))) {
    stage_o2p <- stage_avg |>
      dplyr::mutate(o2_pulse = vo2_ml / hr_bpm)
    p1 <- ggplot2::ggplot(stage_o2p, ggplot2::aes(x = time_plot, y = o2_pulse)) +
      ggplot2::geom_point(size = 2.5, alpha = 0.9, color = "#2E86AB") +
      ggplot2::geom_line(color = "#2E86AB", linewidth = 0.8, alpha = 0.6) +
      ggplot2::labs(
        title = if (language == "fr") expression(Pouls~O[2]) else expression(O[2]~Pulse),
        x = time_label,
        y = expression(VO[2]/FC~(mL/batt))
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
        title = if (language == "fr") expression(VO[2]~vs~Puissance) else expression(VO[2]~vs~Power),
        x = if (language == "fr") "Puissance (W)" else "Power (W)",
        y = expression(VO[2]~(mL/min))
      ) +
      theme_cpet
  } else {
    p2 <- ggplot2::ggplot(stage_avg, ggplot2::aes(x = time_plot, y = vo2_ml)) +
      ggplot2::geom_point(size = 2.5, alpha = 0.9, color = "#E94F37") +
      ggplot2::geom_line(color = "#E94F37", linewidth = 0.8, alpha = 0.6) +
      ggplot2::labs(
        title = if (language == "fr") expression(VO[2]) else expression(VO[2]),
        x = time_label,
        y = expression(VO[2]~(mL/min))
      ) +
      theme_cpet
  }

  # Panel 3: VE vs VCO2 - LINEAR RELATIONSHIP (VE/VCO2 slope)
  p3 <- ggplot2::ggplot(stage_avg, ggplot2::aes(x = vco2_ml, y = ve_l)) +
    ggplot2::geom_point(size = 2.5, alpha = 0.9, color = "#1B998B") +
    ggplot2::geom_smooth(method = "lm", se = TRUE, color = "#1B998B",
                         fill = "#1B998B", alpha = 0.2, linewidth = 1) +
    ggplot2::labs(
      title = expression(VE~vs~VCO[2]),
      x = expression(VCO[2]~(mL/min)),
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
      x = expression(VO[2]~(mL/min)),
      y = expression(VCO[2]~(mL/min))
    ) +
    theme_cpet

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
      labels = c(expression(VE/VO[2]), expression(VE/VCO[2]))
    ) +
    ggplot2::labs(
      title = if (language == "fr") "\u00c9quivalents ventilatoires" else "Ventilatory Equivalents",
      x = expression(VO[2]~(mL/min)),
      y = NULL,
      color = NULL
    ) +
    theme_cpet +
    ggplot2::theme(legend.position = "bottom")

  # Panel 6: RER vs VO2
  p6 <- ggplot2::ggplot(stage_avg, ggplot2::aes(x = vo2_ml, y = rer)) +
    ggplot2::geom_point(size = 2.5, alpha = 0.9, color = "#F77F00") +
    ggplot2::geom_line(color = "#F77F00", linewidth = 0.8, alpha = 0.6) +
    ggplot2::geom_hline(yintercept = 1.0, linetype = "dashed", color = "gray50", linewidth = 0.8) +
    ggplot2::labs(
      title = expression(RER~vs~VO[2]),
      x = expression(VO[2]~(mL/min)),
      y = "RER"
    ) +
    theme_cpet

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
        labels = c(expression(P[ET]*O[2]), expression(P[ET]*CO[2]))
      ) +
      ggplot2::labs(
        title = if (language == "fr") expression(P[ET]*O[2]~"/"~P[ET]*CO[2]) else expression(P[ET]*O[2]~"/"~P[ET]*CO[2]),
        x = expression(VO[2]~(mL/min)),
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
          title = if (language == "fr") expression(Pouls~O[2]) else expression(O[2]~Pulse),
          x = expression(VO[2]~(mL/min)),
          y = expression(VO[2]/HR~(mL/beat))
        ) +
        theme_cpet
    } else {
      p7 <- ggplot2::ggplot() +
        ggplot2::annotate("text", x = 0.5, y = 0.5,
                          label = if (language == "fr") "Donn\u00e9es non disponibles" else "Data not available") +
        ggplot2::theme_void()
    }
  }

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

  combined +
    patchwork::plot_annotation(
      title = sprintf("%s - %s",
                      data@participant@name,
                      as.character(data@metadata@test_date)),
      theme = ggplot2::theme(
        plot.title = ggplot2::element_text(size = 11, face = "bold", hjust = 0.5)
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
#' @export
plot_v_slope <- function(x,
                         thresholds = NULL,
                         show_identity = TRUE,
                         language = "en") {
  if (inherits(x, "CpetAnalysis") || grepl("CpetAnalysis$", class(x)[1])) {
    breaths <- x@data@breaths
    if (is.null(thresholds)) thresholds <- x@thresholds
  } else {
    breaths <- x@breaths
  }

  p <- ggplot2::ggplot(breaths, ggplot2::aes(x = vo2_ml, y = vco2_ml)) +
    ggplot2::geom_point(size = 1.5, alpha = 0.6, color = "#2E86AB") +
    ggplot2::labs(
      title = if (language == "fr") expression(V-Slope~(VCO[2]~vs~VO[2])) else expression(V-Slope~(VCO[2]~vs~VO[2])),
      x = expression(VO[2]~(mL/min)),
      y = expression(VCO[2]~(mL/min))
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
#' @export
plot_ventilatory_equivalents <- function(x,
                                          x_axis = "time",
                                          thresholds = NULL,
                                          language = "en") {
  if (inherits(x, "CpetAnalysis") || grepl("CpetAnalysis$", class(x)[1])) {
    breaths <- x@data@breaths
    if (is.null(thresholds)) thresholds <- x@thresholds
  } else {
    breaths <- x@breaths
  }

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
    x_label <- expression(VO[2]~(mL/min))
  } else {
    x_var <- "time_min"
    x_label <- if (language == "fr") "Temps (min)" else "Time (min)"
  }

  p <- ggplot2::ggplot(breaths_long, ggplot2::aes(x = .data[[x_var]], y = value, color = variable)) +
    ggplot2::geom_point(size = 1.2, alpha = 0.6) +
    ggplot2::scale_color_manual(
      values = c("VE/VO2" = "#2E86AB", "VE/VCO2" = "#E94F37"),
      name = NULL,
      labels = c(expression(VE/VO[2]), expression(VE/VCO[2]))
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
#' @export
plot_gas_exchange <- function(x,
                               variables = c("vo2", "vco2", "rer"),
                               smooth = FALSE,
                               normalize = FALSE,
                               language = "en") {
  if (inherits(x, "CpetAnalysis") || grepl("CpetAnalysis$", class(x)[1])) {
    breaths <- x@data@breaths
    weight_kg <- x@data@participant@weight_kg
  } else {
    breaths <- x@breaths
    weight_kg <- x@participant@weight_kg
  }

  breaths <- breaths |>
    dplyr::mutate(time_min = time_s / 60)

  if (normalize && "vo2" %in% variables) {
    breaths <- breaths |>
      dplyr::mutate(vo2_ml = vo2_ml / weight_kg)
  }

  # Build plot data
  plot_data <- breaths |>
    dplyr::select(time_min, dplyr::any_of(c(
      vo2 = "vo2_ml",
      vco2 = "vco2_ml",
      rer = "rer",
      ve = "ve_l"
    ))) |>
    dplyr::rename_with(~ gsub("_ml|_l", "", .x))

  # Filter to requested variables
  vars_present <- intersect(variables, names(plot_data))

  if (length(vars_present) == 0) {
    cli::cli_abort("None of the requested variables found in data")
  }

  plot_long <- plot_data |>
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

  p <- ggplot2::ggplot(plot_long, ggplot2::aes(x = time_min, y = value, color = variable)) +
    ggplot2::geom_point(size = 1, alpha = 0.5) +
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
#' @export
plot_heart_rate <- function(x,
                            x_axis = "time",
                            show_zones = FALSE,
                            language = "en") {
  if (inherits(x, "CpetAnalysis") || grepl("CpetAnalysis$", class(x)[1])) {
    breaths <- x@data@breaths
    age <- x@data@participant@age
  } else {
    breaths <- x@breaths
    age <- x@participant@age
  }

  if (!"hr_bpm" %in% names(breaths) || all(is.na(breaths$hr_bpm))) {
    cli::cli_abort("Heart rate data not available")
  }

  breaths <- breaths |>
    dplyr::mutate(time_min = time_s / 60)

  # Set x-axis
  if (x_axis == "vo2") {
    x_var <- "vo2_ml"
    x_label <- expression(VO[2]~(mL/min))
  } else {
    x_var <- "time_min"
    x_label <- if (language == "fr") "Temps (min)" else "Time (min)"
  }

  p <- ggplot2::ggplot(breaths, ggplot2::aes(x = .data[[x_var]], y = hr_bpm)) +
    ggplot2::geom_point(size = 1.2, alpha = 0.6, color = "#E94F37") +
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
#'
#' @param x A CpetData object
#' @param show_vo2 Overlay VO2 on secondary axis
#' @param language Language for labels
#'
#' @return A ggplot2 object
#'
#' @export
plot_power <- function(x,
                       show_vo2 = TRUE,
                       language = "en") {
  if (inherits(x, "CpetAnalysis") || grepl("CpetAnalysis$", class(x)[1])) {
    breaths <- x@data@breaths
    weight_kg <- x@data@participant@weight_kg
  } else {
    breaths <- x@breaths
    weight_kg <- x@participant@weight_kg
  }

  if (!"power_w" %in% names(breaths) || all(is.na(breaths$power_w))) {
    cli::cli_abort("Power data not available")
  }

  breaths <- breaths |>
    dplyr::mutate(time_min = time_s / 60)

  if (show_vo2) {
    # Calculate scaling factor for secondary axis
    power_range <- range(breaths$power_w, na.rm = TRUE)
    vo2_range <- range(breaths$vo2_ml, na.rm = TRUE)
    scale_factor <- diff(power_range) / diff(vo2_range)
    offset <- power_range[1] - vo2_range[1] * scale_factor

    p <- ggplot2::ggplot(breaths, ggplot2::aes(x = time_min)) +
      ggplot2::geom_point(ggplot2::aes(y = power_w), size = 1.2, alpha = 0.6, color = "#1B998B") +
      ggplot2::geom_point(ggplot2::aes(y = vo2_ml * scale_factor + offset),
                          size = 1, alpha = 0.4, color = "#2E86AB") +
      ggplot2::scale_y_continuous(
        name = "Power (W)",
        sec.axis = ggplot2::sec_axis(
          ~ (. - offset) / scale_factor,
          name = expression(VO[2]~(mL/min))
        )
      ) +
      ggplot2::labs(
        title = if (language == "fr") expression(Puissance~et~VO[2]) else expression(Power~and~VO[2]),
        x = if (language == "fr") "Temps (min)" else "Time (min)"
      ) +
      ggplot2::theme_minimal() +
      ggplot2::theme(
        plot.title = ggplot2::element_text(size = 14, face = "bold"),
        axis.title.y.left = ggplot2::element_text(color = "#1B998B"),
        axis.title.y.right = ggplot2::element_text(color = "#2E86AB")
      )
  } else {
    p <- ggplot2::ggplot(breaths, ggplot2::aes(x = time_min, y = power_w)) +
      ggplot2::geom_point(size = 1.2, alpha = 0.6, color = "#1B998B") +
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
                                      show_citation = TRUE) {

  if (!inherits(x, "CpetAnalysis") && !grepl("CpetAnalysis$", class(x)[1])) {
    cli::cli_abort("x must be a CpetAnalysis object")
  }

  peaks <- x@peaks
  participant <- x@data@participant

  if (is.null(peaks) || length(peaks@vo2_peak) == 0) {
    cli::cli_abort("No peak values available in analysis")
  }

  # Calculate predicted values
  predicted <- calculate_predicted_values(participant)

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
  labels_en <- expression(atop(VO[2]*max, (mL/kg/min)))
  labels_fr <- expression(atop(VO[2]*max, (mL/kg/min)))

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

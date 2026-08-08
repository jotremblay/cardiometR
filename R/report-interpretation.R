# Report interpretation helpers for cardiometR

#' Generate Visual Interpretation Data
#'
#' @description
#' Generates data for visual interpretation gauges in the report.
#' Returns colors, percentages, and ratings for each domain.
#'
#' @param analysis CpetAnalysis object
#' @param language Language code
#' @param prediction_source Prediction equation source: "jones" or "prefaut"
#' @return List with visual interpretation elements
#' @keywords internal
generate_visual_interpretation <- function(analysis, language = "en",
                                            prediction_source = "jones") {
  peaks <- analysis@peaks
  participant <- analysis@data@participant
  predicted <- calculate_predicted_values(participant, prediction_source = prediction_source)

  # Default values
  default_result <- list(
    aerobic_color = 'rgb("#9CA3AF")',
    aerobic_percent = 0,
    aerobic_rating = if (language == "fr") "Donn\u00e9es insuffisantes" else "Insufficient data",
    cardiovascular_color = 'rgb("#9CA3AF")',
    cardiovascular_percent = 0,
    cardiovascular_rating = if (language == "fr") "Donn\u00e9es insuffisantes" else "Insufficient data",
    ventilatory_color = 'rgb("#9CA3AF")',
    ventilatory_percent = 0,
    ventilatory_rating = if (language == "fr") "Donn\u00e9es insuffisantes" else "Insufficient data",
    interpretation_summary = if (language == "fr") "Donn\u00e9es insuffisantes pour l'interpr\u00e9tation." else "Insufficient data for interpretation."
  )

 if (is.null(peaks) || length(peaks@vo2_peak) == 0) {
    return(default_result)
  }

  # Helper function to get color based on percentage
  get_color <- function(pct, domain = "aerobic") {
    if (domain == "ventilatory") {
      # For RER, higher is better (maximal effort)
      if (pct >= 1.15) return('rgb("#10B981")')  # green - maximal
      if (pct >= 1.10) return('rgb("#F59E0B")')  # amber - near maximal
      if (pct >= 1.00) return('rgb("#EF4444")')  # red - submaximal
      return('rgb("#9CA3AF")')  # gray
    } else {
      # For VO2 and HR, percentage of predicted
      if (pct >= 100) return('rgb("#10B981")')   # green - excellent
      if (pct >= 85) return('rgb("#3B82F6")')    # blue - normal
      if (pct >= 70) return('rgb("#F59E0B")')    # amber - mildly reduced
      if (pct >= 50) return('rgb("#F97316")')    # orange - moderately reduced
      return('rgb("#EF4444")')                    # red - severely reduced
    }
  }

  # Aerobic capacity
  vo2_pct <- round(100 * peaks@vo2_peak / predicted$vo2_max, 0)
  aerobic_color <- get_color(vo2_pct, "aerobic")
  aerobic_bar_pct <- min(100, vo2_pct)

  aerobic_rating <- if (language == "fr") {
    if (vo2_pct >= 100) "Excellente"
    else if (vo2_pct >= 85) "Normale"
    else if (vo2_pct >= 70) "L\u00e9g\u00e8rement r\u00e9duite"
    else if (vo2_pct >= 50) "Mod\u00e9r\u00e9ment r\u00e9duite"
    else "S\u00e9v\u00e8rement r\u00e9duite"
  } else {
    if (vo2_pct >= 100) "Excellent"
    else if (vo2_pct >= 85) "Normal"
    else if (vo2_pct >= 70) "Mildly reduced"
    else if (vo2_pct >= 50) "Moderately reduced"
    else "Severely reduced"
  }

  # Cardiovascular response
  hr_pct <- 0
  cardiovascular_color <- 'rgb("#9CA3AF")'
  cardiovascular_rating <- if (language == "fr") "Non disponible" else "Not available"
  cardiovascular_bar_pct <- 0

  if (!is.null(peaks@hr_peak) && length(peaks@hr_peak) > 0) {
    hr_pct <- round(100 * peaks@hr_peak / predicted$hr_max, 0)
    cardiovascular_color <- get_color(hr_pct, "cardiovascular")
    cardiovascular_bar_pct <- min(100, hr_pct)

    cardiovascular_rating <- if (language == "fr") {
      if (hr_pct >= 95) "Maximale"
      else if (hr_pct >= 85) "Ad\u00e9quate"
      else "Sous-maximale"
    } else {
      if (hr_pct >= 95) "Maximal"
      else if (hr_pct >= 85) "Adequate"
      else "Submaximal"
    }
  }

  # Ventilatory response (RER)
  rer_val <- peaks@rer_peak
  ventilatory_color <- get_color(rer_val, "ventilatory")
  # Scale RER to percentage for bar (0.7-1.3 range -> 0-100%)
  ventilatory_bar_pct <- min(100, max(0, round((rer_val - 0.7) / 0.6 * 100, 0)))

  ventilatory_rating <- if (language == "fr") {
    if (rer_val >= 1.15) "Effort maximal"
    else if (rer_val >= 1.10) "Quasi-maximal"
    else if (rer_val >= 1.00) "Sous-maximal"
    else "Non concluant"
  } else {
    if (rer_val >= 1.15) "Maximal effort"
    else if (rer_val >= 1.10) "Near-maximal"
    else if (rer_val >= 1.00) "Submaximal"
    else "Inconclusive"
  }

  # Summary text
  summary_parts <- c()

  if (language == "fr") {
    summary_parts <- c(summary_parts, sprintf("VO#sub[2]max \u00e0 %d%% du pr\u00e9dit", vo2_pct))
    if (hr_pct > 0) {
      summary_parts <- c(summary_parts, sprintf("FC max \u00e0 %d%% du pr\u00e9dit", hr_pct))
    }
    if (rer_val >= 1.10) {
      summary_parts <- c(summary_parts, "effort maximal atteint (RER \u2265 1.10)")
    } else {
      summary_parts <- c(summary_parts, sprintf("RER pic = %.2f", rer_val))
    }
  } else {
    summary_parts <- c(summary_parts, sprintf("VO#sub[2]max at %d%% of predicted", vo2_pct))
    if (hr_pct > 0) {
      summary_parts <- c(summary_parts, sprintf("HR max at %d%% of predicted", hr_pct))
    }
    if (rer_val >= 1.10) {
      summary_parts <- c(summary_parts, "maximal effort achieved (RER \u2265 1.10)")
    } else {
      summary_parts <- c(summary_parts, sprintf("peak RER = %.2f", rer_val))
    }
  }

  interpretation_summary <- paste(summary_parts, collapse = "; ")
  interpretation_summary <- paste0(toupper(substr(interpretation_summary, 1, 1)),
                                   substr(interpretation_summary, 2, nchar(interpretation_summary)), ".")

  list(
    aerobic_color = aerobic_color,
    aerobic_percent = aerobic_bar_pct,
    aerobic_rating = aerobic_rating,
    cardiovascular_color = cardiovascular_color,
    cardiovascular_percent = cardiovascular_bar_pct,
    cardiovascular_rating = cardiovascular_rating,
    ventilatory_color = ventilatory_color,
    ventilatory_percent = ventilatory_bar_pct,
    ventilatory_rating = ventilatory_rating,
    interpretation_summary = interpretation_summary
  )
}


#' Generate Automatic Interpretation
#'
#' @description
#' Generates automatic interpretation text based on results.
#'
#' @param analysis CpetAnalysis object
#' @param language Language code
#' @param prediction_source Prediction equation source: "jones" or "prefaut"
#' @return List with interpretation sections
#' @keywords internal
generate_auto_interpretation <- function(analysis, language = "en",
                                          prediction_source = "jones") {
  peaks <- analysis@peaks
  participant <- analysis@data@participant
  predicted <- calculate_predicted_values(participant, prediction_source = prediction_source)

  if (is.null(peaks) || length(peaks@vo2_peak) == 0) {
    return(list(
      aerobic = if (language == "fr") "Donn\u00e9es insuffisantes pour l'interpr\u00e9tation." else "Insufficient data for interpretation.",
      cardiovascular = "",
      ventilatory = ""
    ))
  }

  vo2_percent <- 100 * peaks@vo2_peak / predicted$vo2_max

  # Aerobic capacity interpretation
  aerobic_text <- if (language == "fr") {
    if (vo2_percent >= 100) {
      "Capacit\u00e9 a\u00e9robie normale \u00e0 \u00e9lev\u00e9e."
    } else if (vo2_percent >= 85) {
      "Capacit\u00e9 a\u00e9robie dans les limites normales."
    } else if (vo2_percent >= 70) {
      "Capacit\u00e9 a\u00e9robie l\u00e9g\u00e8rement r\u00e9duite."
    } else if (vo2_percent >= 50) {
      "Capacit\u00e9 a\u00e9robie mod\u00e9r\u00e9ment r\u00e9duite."
    } else {
      "Capacit\u00e9 a\u00e9robie s\u00e9v\u00e8rement r\u00e9duite."
    }
  } else {
    if (vo2_percent >= 100) {
      "Normal to elevated aerobic capacity."
    } else if (vo2_percent >= 85) {
      "Aerobic capacity within normal limits."
    } else if (vo2_percent >= 70) {
      "Mildly reduced aerobic capacity."
    } else if (vo2_percent >= 50) {
      "Moderately reduced aerobic capacity."
    } else {
      "Severely reduced aerobic capacity."
    }
  }

  # Cardiovascular interpretation
  hr_text <- ""
  if (!is.null(peaks@hr_peak) && length(peaks@hr_peak) > 0) {
    hr_percent <- 100 * peaks@hr_peak / predicted$hr_max
    hr_text <- if (language == "fr") {
      if (hr_percent >= 95) {
        "R\u00e9ponse chronotrope maximale atteinte."
      } else if (hr_percent >= 85) {
        "R\u00e9ponse chronotrope ad\u00e9quate."
      } else {
        "R\u00e9ponse chronotrope sous-maximale."
      }
    } else {
      if (hr_percent >= 95) {
        "Maximal chronotropic response achieved."
      } else if (hr_percent >= 85) {
        "Adequate chronotropic response."
      } else {
        "Submaximal chronotropic response."
      }
    }
  }

  # Ventilatory interpretation
  rer_text <- if (language == "fr") {
    if (peaks@rer_peak >= 1.15) {
      "RER pic \u00e9lev\u00e9 (\u2265 1.15), compatible avec un effort maximal."
    } else if (peaks@rer_peak >= 1.10) {
      "RER pic \u00e9lev\u00e9, sugg\u00e9rant un effort quasi-maximal."
    } else {
      "RER pic mod\u00e9r\u00e9, effort possiblement sous-maximal."
    }
  } else {
    if (peaks@rer_peak >= 1.15) {
      "Peak RER elevated (\u2265 1.15), consistent with maximal effort."
    } else if (peaks@rer_peak >= 1.10) {
      "Peak RER elevated, suggesting near-maximal effort."
    } else {
      "Moderate peak RER, effort possibly submaximal."
    }
  }

  list(
    aerobic = aerobic_text,
    cardiovascular = hr_text,
    ventilatory = rer_text
  )
}



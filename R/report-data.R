# Report template data builders for cardiometR

#' Build Template Data
#'
#' @description
#' Constructs the data list for Typst template interpolation.
#'
#' @param analysis CpetAnalysis object
#' @param config ReportConfig object
#' @param labels List of text labels
#' @param clinical_notes Optional clinical notes text
#' @param interpretation Optional interpretation list
#' @param athlete_sport Sport for normative comparison (optional)
#' @param athlete_level Competitive level for normative comparison
#' @return Named list for template
#' @keywords internal
build_template_data <- function(analysis, config, labels, clinical_notes, interpretation,
                                athlete_sport = NULL, athlete_level = "recreational",
                                report_sections = NULL, signature_date = NULL) {
  language <- config@language
  prediction_source <- config@prediction_source
  data <- analysis@data
  participant <- data@participant
  metadata <- data@metadata
  peaks <- analysis@peaks
  thresholds <- analysis@thresholds

  # Calculate BMI
  bmi <- participant@weight_kg / (participant@height_cm / 100)^2

  # Calculate predicted values
  predicted <- calculate_predicted_values(participant, prediction_source = prediction_source)

  # Format sex
  sex_label <- if (participant@sex == "M") {
    labels$label_male
  } else if (participant@sex == "F") {
    labels$label_female
  } else {
    "Other"
  }

  institution_split <- split_header_text(config@institution %||% "", split_word = "et")
  lab_name_split <- split_header_text(config@lab_name %||% "", split_word = "et")

  # Build base data
  template_data <- c(
    labels,
    list(
      # Header info
      institution = config@institution %||% "",
      institution_line1 = institution_split$line1,
      institution_line2 = institution_split$line2,
      lab_name = config@lab_name %||% "",
      lab_name_line1 = lab_name_split$line1,
      lab_name_line2 = lab_name_split$line2,
      lab_url = config@lab_url %||% "",
      report_date = format(Sys.Date(), "%Y-%m-%d"),
      signature_date = format(signature_date %||% Sys.Date(), "%Y-%m-%d"),
      # The header carries the two institutions and the lab. A configured
      # logo replaces the institutional one; the others stay put.
      logo_path = {
        chosen <- config@logo_path
        if (!is.null(chosen) && length(chosen) > 0 && nzchar(chosen)) {
          chosen
        } else {
          system.file("assets", "Ec-_kinesiologie_-act_-phy_officiel-RVB.png",
                      package = "cardiometR")
        }
      },
      epic_logo_path = system.file("assets", "Centre_EPIC_ICM.jpg",
                                   package = "cardiometR"),
      lab_logo_path = system.file("assets", "lpeba_logo.svg",
                                  package = "cardiometR"),

      # Patient info (escape user data for Typst safety)
      patient_name = escape_typst(participant@name),
      patient_id = escape_typst(participant@id),
      patient_dob = if (!is.null(participant@date_of_birth) && length(participant@date_of_birth) > 0) {
        format(participant@date_of_birth, "%Y-%m-%d")
      } else "",
      patient_age = format_age(participant@age),
      patient_sex = sex_label,
      patient_height = participant@height_cm,
      patient_weight = participant@weight_kg,
      patient_bmi = round(bmi, 1),
      patient_sport = escape_typst({
        sport <- participant@sport %||% ""
        if (length(sport) > 0 && nchar(sport) > 0) {
          sport
        } else if (!is.null(analysis@protocol_config)) {
          format_modality(analysis@protocol_config@modality, language)
        } else {
          "-"
        }
      }),

      # Test info
      test_date = format(metadata@test_date, "%Y-%m-%d"),
      test_protocol = escape_typst(trimws(gsub("_", " ", metadata@protocol, fixed = TRUE))),
      test_device = escape_typst(metadata@device),
      test_technician = escape_typst(config@technician %||% metadata@technician %||% "-"),
      test_duration = format_duration(max(data@breaths$time_s)),
      test_reason = "-"
    )
  )

  # Running header, footer and title lines. Every page names the record it
  # belongs to, so a loose page can still be filed.
  record_id <- escape_typst(participant@id)
  test_date_str <- format(metadata@test_date, "%Y-%m-%d")
  template_data$record_line <- paste(
    test_date_str,
    paste(labels$label_record, record_id),
    sep = " \u00b7 "
  )
  template_data$running_header <- paste(
    labels$report_short_title, record_id, test_date_str,
    sep = " \u00b7 "
  )
  template_data$footer_left <- paste(
    record_id, escape_typst(participant@name), test_date_str,
    sep = " \u00b7 "
  )
  template_data$report_kicker <- labels$report_kicker
  template_data$test_summary_line <- paste(
    trimws(gsub("_", " ", metadata@protocol, fixed = TRUE)),
    escape_typst(metadata@device),
    sep = " \u00b7 "
  )

  # Add peak values if available
  if (!is.null(peaks) && length(peaks@vo2_peak) > 0) {
    vo2_percent <- round(100 * peaks@vo2_peak / predicted$vo2_max, 0)
    hr_percent <- if (!is.null(peaks@hr_peak) && length(peaks@hr_peak) > 0) {
      round(100 * peaks@hr_peak / predicted$hr_max, 0)
    } else NA

    # Determine modality for labels
    is_treadmill <- !is.null(analysis@protocol_config) &&
      analysis@protocol_config@modality == "treadmill"

    # Modality-aware peak card values
    if (is_treadmill && !is.null(peaks@speed_peak) && length(peaks@speed_peak) > 0) {
      power_card_value <- round(peaks@speed_peak, 1)
      power_card_wkg <- ""
      template_data$label_power_peak <- if (language == "fr") {
        "Vitesse pic (km/h)"
      } else {
        "Peak Velocity (km/h)"
      }
      template_data$label_power_peak_row <- if (language == "fr") {
        "Vitesse pic (km/h)"
      } else {
        "Speed peak (km/h)"
      }
    } else {
      power_card_value <- if (!is.null(peaks@power_peak)) round(peaks@power_peak, 0) else "-"
      power_card_wkg <- if (!is.null(peaks@power_peak)) {
        paste0(round(peaks@power_peak / participant@weight_kg, 1), " W/kg")
      } else "-"
    }

    template_data <- c(template_data, list(
      vo2_peak_value = round(peaks@vo2_kg_peak, 1),
      vo2_peak_percent = paste0(vo2_percent, "%"),
      hr_peak_value = if (!is.null(peaks@hr_peak)) round(peaks@hr_peak, 0) else "-",
      hr_peak_percent = if (!is.na(hr_percent)) paste0(hr_percent, "%") else "-",
      power_peak_value = power_card_value,
      power_peak_wkg = power_card_wkg,

      # Detailed results
      vo2_peak_abs = round(peaks@vo2_peak, 0),
      vo2_peak_rel = round(peaks@vo2_kg_peak, 1),
      vo2_predicted = round(predicted$vo2_max, 0),
      vo2_rel_predicted = round(predicted$vo2_max / participant@weight_kg, 1),
      vo2_percent = vo2_percent,
      vo2_rel_percent = vo2_percent,
      ve_peak = round(peaks@ve_peak, 1),
      ve_predicted = round(predicted$ve_max, 0),
      ve_percent = round(100 * peaks@ve_peak / predicted$ve_max, 0),
      hr_peak = if (!is.null(peaks@hr_peak)) round(peaks@hr_peak, 0) else "-",
      hr_predicted = round(predicted$hr_max, 0),
      hr_percent = if (!is.na(hr_percent)) hr_percent else "-",
      rer_peak = round(peaks@rer_peak, 2),
      power_peak = if (is_treadmill && !is.null(peaks@speed_peak) && length(peaks@speed_peak) > 0) {
        round(peaks@speed_peak, 1)
      } else if (!is.null(peaks@power_peak)) {
        round(peaks@power_peak, 0)
      } else "-",
      # Peak power/speed predictions from Jones/Wasserman formulas are not
      # validated for ramp-protocol peak values; suppress to avoid nonsense
      # percentages (e.g. 23 %Pred for a 358 W test vs 1576 W "predicted").
      power_predicted = "\u2014",
      power_percent = "\u2014",
      o2_pulse = if (!is.null(peaks@hr_peak) && peaks@hr_peak > 0) {
        round(peaks@vo2_peak / peaks@hr_peak, 1)
      } else "-",
      o2_pulse_predicted = round(predicted$o2_pulse, 1),
      o2_pulse_percent = if (!is.null(peaks@hr_peak) && peaks@hr_peak > 0) {
        round(100 * (peaks@vo2_peak / peaks@hr_peak) / predicted$o2_pulse, 0)
      } else "-"
    ))
  }

  # Add thresholds if available
  has_thresholds <- !is.null(thresholds) &&
                    length(thresholds@vt1_vo2) > 0 &&
                    !is.na(thresholds@vt1_vo2)

  template_data$thresholds_detected <- has_thresholds
  template_data$has_vt_table <- has_thresholds

  if (has_thresholds) {
    template_data <- c(template_data, list(
      vt1_vo2 = round(thresholds@vt1_vo2, 0),
      vt1_percent = if (!is.null(peaks)) round(100 * thresholds@vt1_vo2 / peaks@vo2_peak, 0) else "-",
      vt1_hr = if (!is.null(thresholds@vt1_hr)) round(thresholds@vt1_hr, 0) else "-",
      vt1_power = if (!is.null(thresholds@vt1_power)) round(thresholds@vt1_power, 0) else "-",
      vt2_vo2 = if (!is.null(thresholds@vt2_vo2)) round(thresholds@vt2_vo2, 0) else "-",
      vt2_percent = if (!is.null(thresholds@vt2_vo2) && !is.null(peaks)) {
        round(100 * thresholds@vt2_vo2 / peaks@vo2_peak, 0)
      } else "-",
      vt2_hr = if (!is.null(thresholds@vt2_hr)) round(thresholds@vt2_hr, 0) else "-",
      vt2_power = if (!is.null(thresholds@vt2_power)) round(thresholds@vt2_power, 0) else "-",
      # A hand-corrected threshold says so on the printed report, so the
      # reader knows the value did not come from the detector.
      threshold_method = if (identical(thresholds@vt1_method, "manual")) {
        tr("threshold_method_manual", config@language)
      } else {
        thresholds@vt1_method %||% "V-slope"
      },
      threshold_confidence = thresholds@confidence %||% "moderate"
    ))
  }

  # Pre-test conditions if available
  if (!is.null(analysis@pre_test_conditions)) {
    ptc <- analysis@pre_test_conditions
    template_data <- c(template_data, list(
      has_pretest_conditions = TRUE,
      nutritional_state = format_nutritional_state(ptc@nutritional_state, language),
      last_meal_hours = ptc@last_meal_hours,
      fatigue_state = format_fatigue_state(ptc@fatigue_state, language),
      medications_taken = ptc@medications_taken,
      medication_list = escape_typst(if (!is.null(ptc@medication_names) && length(ptc@medication_names) > 0) {
        paste(ptc@medication_names, collapse = ", ")
      } else {
        if (language == "fr") "Aucun" else "None"
      }),
      caffeine_intake = ptc@caffeine_intake %||% FALSE,
      caffeine_mg = ptc@caffeine_mg
    ))
  } else {
    template_data$has_pretest_conditions <- FALSE
  }

  protocol_for_report <- analysis@protocol_config
  if (is.null(protocol_for_report)) {
    protocol_for_report <- tryCatch(
      detect_protocol_config(data),
      error = function(e) NULL
    )
  }

  # Protocol details if available
  if (!is.null(protocol_for_report)) {
    pc <- protocol_for_report
    intensity_unit <- if (pc@modality == "treadmill") "km/h" else "W"
    value_digits <- if (pc@modality == "treadmill") 1 else 0

    format_with_unit <- function(value, unit, digits = 0) {
      if (is.null(value) || length(value) == 0 || anyNA(value)) {
        return("-")
      }
      paste0(format(round(value, digits), trim = TRUE), " ", unit)
    }

    # Modality-aware intensity labels
    label_start <- if (pc@modality == "treadmill") {
      tr("starting_speed", language)
    } else {
      tr("starting_power", language)
    }
    label_incr <- if (pc@modality == "treadmill") {
      tr("speed_increment", language)
    } else {
      tr("power_increment", language)
    }

    is_blank_text <- function(x) {
      is.null(x) ||
        length(x) == 0 ||
        all(is.na(x)) ||
        !nzchar(trimws(as.character(x[[1]])))
    }

    # Equipment: use modality-aware display name
    equipment_model <- format_modality(pc@modality, language)

    # Analyzer: default to "COSMED Quark CPET" when available
    analyzer_model <- pc@analyzer_model
    if (!is_blank_text(analyzer_model)) {
      analyzer_model <- escape_typst(as.character(analyzer_model[[1]]))
    } else {
      # Fall back to device metadata or standard COSMED name
      if (!is_blank_text(metadata@device)) {
        analyzer_model <- escape_typst(as.character(metadata@device))
      } else {
        analyzer_model <- "COSMED Quark CPET"
      }
    }

    template_data <- c(template_data, list(
      has_protocol_details = TRUE,
      protocol_modality = pc@modality,
      protocol_modality_label = format_modality(pc@modality, language),
      label_starting_intensity = label_start,
      label_increment = label_incr,
      starting_intensity = pc@starting_intensity,
      intensity_unit = intensity_unit,
      increment_size = pc@increment_size,
      stage_duration_s = pc@stage_duration_s,
      starting_intensity_display = format_with_unit(pc@starting_intensity, intensity_unit, value_digits),
      increment_size_display = format_with_unit(pc@increment_size, intensity_unit, value_digits),
      stage_duration_display = if (!is.null(pc@stage_duration_s) &&
        length(pc@stage_duration_s) > 0 &&
        !anyNA(pc@stage_duration_s)) {
        paste0(round(pc@stage_duration_s, 0), " s")
      } else {
        "-"
      },
      starting_grade = pc@starting_grade,
      grade_increment = pc@grade_increment,
      equipment_model = equipment_model,
      analyzer_model = analyzer_model
    ))
  } else {
    template_data$has_protocol_details <- FALSE
  }

  # Stage-by-stage summary table
  if (!is.null(analysis@stage_summary) && nrow(analysis@stage_summary) > 0) {
    template_data$has_stage_table <- TRUE
    modality <- if (!is.null(protocol_for_report)) {
      protocol_for_report@modality
    } else {
      NULL
    }
    template_data$stage_table <- format_stage_table_typst(
      analysis@stage_summary,
      language,
      modality = modality
    )
  } else {
    template_data$has_stage_table <- FALSE
  }

  # Economy/efficiency metrics
  if (!is.null(analysis@economy_metrics)) {
    em <- analysis@economy_metrics
    # Set section title based on modality
    economy_title <- if (em@modality == "cycling") {
      if (language == "fr") "Efficacit\u00e9 m\u00e9canique" else "Cycling Efficiency"
    } else {
      if (language == "fr") "\u00c9conomie de course" else "Running Economy"
    }
    has_any_economy <- any(vapply(
      list(em@gross_efficiency, em@running_economy),
      function(v) !is.null(v) && length(v) > 0 && all(is.finite(v)),
      logical(1)
    ))
    template_data <- c(template_data, list(
      has_economy_metrics = has_any_economy,
      section_economy = economy_title,
      economy_modality = em@modality,
      gross_efficiency = em@gross_efficiency,
      running_economy = em@running_economy,
      economy_reference_stage = em@reference_stage,
      economy_reference_power = em@reference_power,
      economy_reference_speed = em@reference_speed
    ))
  } else {
    template_data$has_economy_metrics <- FALSE
  }

  # Data type (breath-by-breath vs time-averaged)
  data <- analysis@data
  if (data@is_averaged && !is.null(data@averaging_window)) {
    template_data$data_type <- sprintf("%s (%ds)",
      tr("time_averaged", language), data@averaging_window)
  } else {
    template_data$data_type <- tr("breath_by_breath", language)
  }
  template_data$label_data_type <- tr("data_type", language)

  # Predicted values citation note with population description
  sex_desc <- if (participant@sex == "M") {
    if (language == "fr") "hommes" else "males"
  } else {
    if (language == "fr") "femmes" else "females"
  }

  # Use citation from predicted values (adapts to prediction source)
  if (language == "fr") {
    template_data$predicted_values_note <- sprintf(
      "Valeurs pr\u00e9dites pour %s, %s ans. R\u00e9f. : %s.",
      sex_desc, format_age(participant@age), predicted$citation_short
    )
  } else {
    template_data$predicted_values_note <- sprintf(
      "Predicted values for %s, age %s. Ref: %s.",
      sex_desc, format_age(participant@age), predicted$citation_short
    )
  }

  # Section toggle flags (override data-based detection when report_sections is specified)
  if (!is.null(report_sections)) {
    if (!"pretest" %in% report_sections) {
      template_data$has_pretest_conditions <- FALSE
    }
    if (!"protocol_details" %in% report_sections) {
      template_data$has_protocol_details <- FALSE
    }
    if (!"stage_table" %in% report_sections) {
      template_data$has_stage_table <- FALSE
    }
    if (!"economy" %in% report_sections) {
      template_data$has_economy_metrics <- FALSE
    }
    if (!"thresholds" %in% report_sections) {
      template_data$thresholds_detected <- FALSE
      template_data$has_vt_table <- FALSE
    }
    if (!"clinical_notes" %in% report_sections) {
      template_data$has_clinical_notes <- FALSE
    }
  }

  # Whether the effort was maximal, stated up front. A reader who sees a
  # submaximal test should stop reading the peak values as peaks.
  template_data <- c(template_data,
                     build_validity_block(analysis, labels, language))

  # What produced the numbers, printed so a second reader can reproduce
  # the analysis.
  template_data <- c(template_data,
                     build_analysis_params_block(analysis, config, labels,
                                                 language))

  # Add visual interpretation data
  visual_interp <- generate_visual_interpretation(analysis, config@language,
                                                   prediction_source = prediction_source)
  template_data <- c(template_data, visual_interp)

  # Phase 7: athlete profile / estimates & caveats / longitudinal section data
  phase7 <- build_phase7_template_data(analysis, language, report_sections,
                                       athlete_sport = athlete_sport,
                                       athlete_level = athlete_level)
  template_data <- c(template_data, phase7)

  # Add clinical notes
  # Add clinical notes if provided
  template_data$clinical_notes <- escape_typst(clinical_notes %||% "")
  template_data$has_clinical_notes <- !is.null(clinical_notes) && nchar(clinical_notes) > 0

  # Build bibliography from cited sources
  bibliography_entries <- list()

  # Always add predicted values citation
  bibliography_entries$predicted <- predicted$citation

  # Add sport-specific normative citations if applicable
  if (!is.null(athlete_sport)) {
    norms <- get_normative_data(athlete_sport, athlete_level, participant@sex, participant@age)
    bibliography_entries$vo2max_norms <- norms$citation

    # Add efficiency/economy citation based on sport
    if (athlete_sport == "cycling" && !is.null(norms$efficiency_citation)) {
      bibliography_entries$efficiency <- norms$efficiency_citation
    } else if (athlete_sport == "running" && !is.null(norms$economy_citation)) {
      bibliography_entries$economy <- norms$economy_citation
    }
  }

  # Format bibliography as numbered list
  bib_text <- paste(
    seq_along(bibliography_entries),
    ". ",
    unlist(bibliography_entries),
    sep = "",
    collapse = "\n\n"
  )

  template_data$bibliography <- if (length(bibliography_entries) > 0) bib_text else ""

  # Add Pr\u00e9faut-specific protocol fields if available
  if (prediction_source == "prefaut" && !is.null(predicted$pmt)) {
    template_data$prefaut_pmt <- round(predicted$pmt, 0)
    template_data$prefaut_warmup <- round(predicted$warmup_watts, 0)
    template_data$prefaut_step <- round(predicted$step_watts, 0)
    template_data$prefaut_population <- predicted$population
  }

  template_data
}

# Row index of the stage_summary entry nearest 70% of VO2peak, filtered so
# `required` column (e.g. "power_w" or "speed_kmh") is positive and finite.
# Returns NA_integer_ when inputs are unusable.
submax_stage_idx <- function(analysis, peaks, required) {
  ss <- tryCatch(analysis@stage_summary, error = function(e) NULL)
  vo2_peak_ml <- tryCatch(peaks@vo2_peak, error = function(e) NA_real_)
  if (!is.data.frame(ss) || nrow(ss) == 0 ||
      !all(c("vo2_ml", required) %in% names(ss)) ||
      !is.numeric(vo2_peak_ml) || !is.finite(vo2_peak_ml)) {
    return(NA_integer_)
  }
  valid <- is.finite(ss$vo2_ml) & is.finite(ss[[required]]) & ss[[required]] > 0
  if (!any(valid)) return(NA_integer_)
  which(valid)[which.min(abs(ss$vo2_ml[valid] - 0.70 * vo2_peak_ml))]
}


# Split long header labels into two lines (e.g., before "et" in French names)
split_header_text <- function(text, split_word = "et") {
  raw_value <- as.character(text %||% "")
  if (length(raw_value) == 0 || is.na(raw_value[1])) {
    raw_value <- ""
  } else {
    raw_value <- raw_value[1]
  }

  value <- gsub("\\s+", " ", trimws(raw_value))
  if (!nzchar(value)) {
    return(list(line1 = "", line2 = ""))
  }


  pattern <- sprintf("^(.+?)\\s+(%s\\b.*)$", split_word)
  match <- regexec(pattern, value, ignore.case = TRUE, perl = TRUE)
  parts <- regmatches(value, match)[[1]]

  if (length(parts) >= 3) {
    return(list(
      line1 = escape_typst(trimws(parts[2])),
      line2 = escape_typst(trimws(parts[3]))
    ))
  }

  list(line1 = escape_typst(value), line2 = "")
}


#' Calculate Predicted Values
#'
#' @description
#' Calculates age/sex-specific predicted maximal values.
#' Supports Jones et al. (1997) and Prefaut et al. prediction equations.
#'
#' @param participant Participant object
#' @param prediction_source Prediction equation source: "jones" or "prefaut"
#' @return Named list of predicted values
#' @keywords internal
calculate_predicted_values <- function(participant, prediction_source = "jones") {
  age <- participant@age
  sex <- participant@sex
  weight <- participant@weight_kg
  height <- participant@height_cm

  if (prediction_source == "prefaut") {
    # --- Pr\u00e9faut prediction equations ---

    # Determine population category and VO2max predicted (mL/min)
    if (age < 16) {
      # Pediatric equations
      if (sex == "M") {
        population <- "boy"
        vo2_max <- (52.8 * weight) - 303.4
      } else {
        population <- "girl"
        vo2_max <- (28.5 * weight) + 288
      }
    } else if (sex == "M") {
      # Adult male - check obesity threshold
      ideal_weight <- (0.79 * height) - 60.7
      if (weight > ideal_weight) {
        population <- "obese_male"
        vo2_max <- ideal_weight * (50.72 - 0.372 * age)
      } else {
        population <- "male"
        vo2_max <- weight * (50.72 - 0.372 * age)
      }
    } else {
      # Adult female - check obesity threshold
      ideal_weight <- (0.65 * height) - 42.8
      if (weight > ideal_weight) {
        population <- "obese_female"
        vo2_max <- height * (14.81 - 0.11 * age)
      } else {
        population <- "female"
        vo2_max <- (42.8 + weight) * (22.8 - 0.17 * age)
      }
    }

    vo2_max_rel <- vo2_max / weight

    # HR max (Pr\u00e9faut): 210 - 0.65 * age (all populations)
    hr_max <- 210 - 0.65 * age

    # PMT (maximal theoretical power, watts)
    pmt <- (vo2_max - 300) / 10.3

    # Warmup watts
    warmup_watts <- pmt / 5

    # Step increment (10 stages from warmup to PMT)
    step_watts <- round((pmt - warmup_watts) / 10)

    # VE max approximation (same as Jones)
    ve_max <- 25 * (vo2_max / 1000)

    # Power max (use PMT as predicted power)
    power_max <- max(pmt, 50)

    # O2 pulse predicted
    o2_pulse <- vo2_max / hr_max

    list(
      hr_max = hr_max,
      vo2_max = vo2_max,
      vo2_max_rel = vo2_max_rel,
      ve_max = ve_max,
      power_max = power_max,
      o2_pulse = o2_pulse,
      pmt = pmt,
      warmup_watts = warmup_watts,
      step_watts = step_watts,
      population = population,
      citation = "Pr\u00e9faut C, et al. Exercise and Sport Sciences Reviews. Prediction equations for VO2max.",
      citation_short = "Pr\u00e9faut et al."
    )
  } else {
    # --- Jones et al. (1997) prediction equations (default) ---

    # Predicted HR max (Tanaka et al., 2001)
    hr_max <- 208 - 0.7 * age

    # Predicted VO2max (ml/min) - Jones et al. equations
    if (sex == "M") {
      # Males: VO2max = (60 - 0.55 * age) * weight
      vo2_max_rel <- 60 - 0.55 * age
      vo2_max <- vo2_max_rel * weight
    } else {
      # Females: VO2max = (48 - 0.37 * age) * weight
      vo2_max_rel <- 48 - 0.37 * age
      vo2_max <- vo2_max_rel * weight
    }

    # Predicted VE max (approximation: 35 * FEV1, using estimated FEV1)
    # Simplified: VE_max ~ 25-30 * VO2max(L/min)
    ve_max <- 25 * (vo2_max / 1000)

    # Predicted power (Wasserman equation approximation)
    if (sex == "M") {
      power_max <- (height - 60) * 20 - age * 2
    } else {
      power_max <- (height - 60) * 14 - age * 2
    }
    power_max <- max(power_max, 50)

    # O2 pulse predicted
    o2_pulse <- vo2_max / hr_max

    list(
      hr_max = hr_max,
      vo2_max = vo2_max,
      vo2_max_rel = vo2_max_rel,
      ve_max = ve_max,
      power_max = power_max,
      o2_pulse = o2_pulse,
      citation = "Jones NL, et al. Clinical Exercise Testing. 4th ed. Saunders; 1997. Tanaka H, et al. J Am Coll Cardiol. 2001;37(1):153-156.",
      citation_short = "Jones et al., 1997; Tanaka et al., 2001"
    )
  }
}



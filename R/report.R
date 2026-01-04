# Report Generation Functions for cardiometR
# Generates bilingual PDF reports using Typst

#' Generate CPET Report
#'
#' @description
#' Creates a professional PDF report from CPET analysis results using Typst.
#' Supports English and French languages with customizable templates.
#' Can include sport-specific normative data comparisons with citations.
#'
#' @param analysis A CpetAnalysis object containing processed data and results
#' @param output_file Path for the output PDF file
#' @param config A ReportConfig object with report settings
#' @param include_graphs Logical; include visualization plots (default TRUE)
#' @param athlete_sport Sport for normative comparison: "cycling", "running",
#'   "triathlon", or NULL for general population only (default NULL)
#' @param athlete_level Competitive level: "elite", "competitive", "recreational"
#'   (default "recreational")
#' @param clinical_notes Optional character string with clinical notes
#' @param interpretation Optional list with interpretation sections
#'
#' @return Invisibly returns the output file path
#'
#' @examples
#' \dontrun{
#' analysis <- analyze_cpet(data)
#' config <- ReportConfig(language = "fr", institution = "UCLouvain")
#'
#' # Standard report with general population comparison
#' generate_report(analysis, "patient_report.pdf", config)
#'
#' # Report with elite cyclist normative comparison
#' generate_report(analysis, "cyclist_report.pdf", config,
#'                 athlete_sport = "cycling", athlete_level = "elite")
#'
#' # Report for recreational triathlete
#' generate_report(analysis, "triathlete_report.pdf", config,
#'                 athlete_sport = "triathlon", athlete_level = "recreational")
#' }
#'
#' @export
generate_report <- function(analysis,
                            output_file = NULL,
                            config = ReportConfig(),
                            include_graphs = TRUE,
                            athlete_sport = NULL,
                            athlete_level = "recreational",
                            clinical_notes = NULL,
                            interpretation = NULL) {

  # Validate inputs
  if (!inherits(analysis, "CpetAnalysis") && !grepl("CpetAnalysis$", class(analysis)[1])) {
    cli::cli_abort("analysis must be a CpetAnalysis object")
  }

  # Set default output file
  if (is.null(output_file)) {
    patient_id <- analysis@data@participant@id
    date_str <- format(analysis@data@metadata@test_date, "%Y%m%d")
    output_file <- sprintf("cpet_report_%s_%s.pdf", patient_id, date_str)
  }

  language <- config@language

  # Get labels for the report
  labels <- get_report_labels(language)

  # Build template data
  template_data <- build_template_data(
    analysis = analysis,
    config = config,
    labels = labels,
    clinical_notes = clinical_notes,
    interpretation = interpretation,
    athlete_sport = athlete_sport,
    athlete_level = athlete_level
  )

  # Generate graphs if requested
  graph_files <- list()
  if (include_graphs) {
    graph_files <- generate_report_graphs(analysis, language, athlete_sport, athlete_level)
    template_data <- c(template_data, graph_files)
  }
  template_data$has_graphs <- include_graphs

  # Get template path
  template_path <- get_template_path(config@template)

  # Render with Typst via typr
  render_typst_report(
    template_path = template_path,
    data = template_data,
    output_file = output_file
  )

  # Clean up temporary graph files
  if (include_graphs) {
    cleanup_temp_files(graph_files)
  }

  cli::cli_alert_success("Report generated: {.file {output_file}}")
  invisible(output_file)
}


#' Get Report Labels
#'
#' @description
#' Returns all text labels for the report in the specified language.
#'
#' @param language Language code ("en" or "fr")
#' @return Named list of labels
#' @keywords internal
get_report_labels <- function(language = "en") {
  labels_en <- list(
    # Title and headers
    title = "Cardiopulmonary Exercise Test Report",
    subtitle = "Clinical Assessment",

    # Section titles
    section_patient = "Patient Information",
    section_test = "Test Information",
    section_peak_values = "Peak Values",
    section_detailed_results = "Detailed Results",
    section_thresholds = "Ventilatory Thresholds",
    section_graphs = "Graphical Analysis",
    section_interpretation = "Interpretation",
    section_aerobic_capacity = "Aerobic Capacity",
    section_cardiovascular = "Cardiovascular Response",
    section_ventilatory = "Ventilatory Response",
    section_clinical_notes = "Clinical Notes",
    section_references = "References",

    # Patient labels
    label_name = "Name",
    label_id = "Patient ID",
    label_dob = "Date of Birth",
    label_age = "Age",
    label_years = "years",
    label_sex = "Sex",
    label_height = "Height",
    label_weight = "Weight",
    label_bmi = "BMI",
    label_sport = "Sport/Activity",
    label_male = "Male",
    label_female = "Female",

    # Test labels
    label_test_date = "Test Date",
    label_protocol = "Protocol",
    label_device = "Device",
    label_technician = "Technician",
    label_duration = "Duration",
    label_reason = "Indication",

    # Results labels
    label_parameter = "Parameter",
    label_value = "Value",
    label_predicted = "Predicted",
    label_vo2_peak = "VO#sub[2] peak (mL/kg/min)",
    label_hr_peak = "HR peak (bpm)",
    label_power_peak = "Peak Power (W)",
    label_power = "Power",
    label_threshold = "Threshold",
    label_aerobic = "Aerobic Threshold",
    label_anaerobic = "Respiratory Compensation",
    label_detection_method = "Detection method",
    label_confidence = "Confidence",

    # Table row labels (detailed results)
    label_vo2_peak_abs = "VO#sub[2] peak (mL/min)",
    label_vo2_peak_rel = "VO#sub[2] peak (mL/kg/min)",
    label_ve_peak = "VE peak (L/min)",
    label_hr_peak_row = "HR peak (bpm)",
    label_rer_peak = "RER peak",
    label_power_peak_row = "Power peak (W)",
    label_o2_pulse = "O#sub[2] pulse (mL/beat)",
    label_hr_unit = "HR (bpm)",

    # Signatures
    label_technician_signature = "Technician Signature",
    label_physician_signature = "Physician Signature",

    # Captions
    caption_panel = "9-Panel CPET Display",
    caption_vslope = "V-Slope Analysis for Threshold Detection",
    caption_predicted = "Measured vs Predicted Values for Age/Sex",

    # Messages
    message_no_thresholds = "Ventilatory thresholds could not be reliably determined.",
    footer_left = "Confidential Medical Document",
    footer_disclaimer = "This report is generated automatically and should be reviewed by a qualified healthcare professional.",

    # Pre-test conditions
    section_pretest = "Pre-Test Conditions",
    label_nutritional_state = "Nutritional State",
    label_hours_ago = "h ago",
    label_fatigue_state = "Fatigue State",
    label_medications = "Medications",
    label_caffeine = "Caffeine",

    # Protocol details
    section_protocol_details = "Protocol Details",
    label_modality = "Modality",
    label_starting_intensity = "Starting Intensity",
    label_increment = "Increment",
    label_stage_duration = "Stage Duration",
    label_equipment = "Equipment",
    label_analyzer = "Analyzer",

    # Stage table
    section_stage_table = "Stage-by-Stage Results",

    # Economy metrics
    section_economy = "Movement Economy",
    label_gross_efficiency = "Gross Efficiency",
    label_running_economy = "Running Economy",
    label_at_stage = "at stage",
    unit_ml_kg_km = "mL/kg/km"
  )

  labels_fr <- list(
    # Title and headers
    title = "Rapport d'\u00e9preuve d'effort cardiorespiratoire",
    subtitle = "\u00c9valuation clinique",

    # Section titles
    section_patient = "Informations du patient",
    section_test = "Informations du test",
    section_peak_values = "Valeurs maximales",
    section_detailed_results = "R\u00e9sultats d\u00e9taill\u00e9s",
    section_thresholds = "Seuils ventilatoires",
    section_graphs = "Analyse graphique",
    section_interpretation = "Interpr\u00e9tation",
    section_aerobic_capacity = "Capacit\u00e9 a\u00e9robie",
    section_cardiovascular = "R\u00e9ponse cardiovasculaire",
    section_ventilatory = "R\u00e9ponse ventilatoire",
    section_clinical_notes = "Notes cliniques",
    section_references = "R\u00e9f\u00e9rences",

    # Patient labels
    label_name = "Nom",
    label_id = "ID Patient",
    label_dob = "Date de Naissance",
    label_age = "\u00c2ge",
    label_years = "ans",
    label_sex = "Sexe",
    label_height = "Taille",
    label_weight = "Poids",
    label_bmi = "IMC",
    label_sport = "Sport/Activit\u00e9",
    label_male = "Homme",
    label_female = "Femme",

    # Test labels
    label_test_date = "Date du Test",
    label_protocol = "Protocole",
    label_device = "Appareil",
    label_technician = "Technicien",
    label_duration = "Dur\u00e9e",
    label_reason = "Indication",

    # Results labels
    label_parameter = "Param\u00e8tre",
    label_value = "Valeur",
    label_predicted = "Pr\u00e9dit",
    label_vo2_peak = "VO#sub[2] pic (mL/kg/min)",
    label_hr_peak = "FC pic (bpm)",
    label_power_peak = "Puissance pic (W)",
    label_power = "Puissance",
    label_threshold = "Seuil",
    label_aerobic = "Seuil a\u00e9robie",
    label_anaerobic = "Point de compensation respiratoire",
    label_detection_method = "M\u00e9thode de d\u00e9tection",
    label_confidence = "Confiance",

    # Table row labels (detailed results) - French translations
    label_vo2_peak_abs = "VO#sub[2] pic (mL/min)",
    label_vo2_peak_rel = "VO#sub[2] pic (mL/kg/min)",
    label_ve_peak = "VE pic (L/min)",
    label_hr_peak_row = "FC pic (bpm)",
    label_rer_peak = "QR pic",
    label_power_peak_row = "Puissance pic (W)",
    label_o2_pulse = "Pouls O#sub[2] (mL/battement)",
    label_hr_unit = "FC (bpm)",

    # Signatures
    label_technician_signature = "Signature du technicien",
    label_physician_signature = "Signature du m\u00e9decin",

    # Captions
    caption_panel = "Affichage CPET 9 panneaux",
    caption_vslope = "Analyse V-slope pour d\u00e9tection des seuils",
    caption_predicted = "Valeurs mesur\u00e9es vs pr\u00e9dites pour \u00e2ge/sexe",

    # Messages
    message_no_thresholds = "Les seuils ventilatoires n'ont pas pu \u00eatre d\u00e9termin\u00e9s de mani\u00e8re fiable.",
    footer_left = "Document m\u00e9dical confidentiel",
    footer_disclaimer = "Ce rapport est g\u00e9n\u00e9r\u00e9 automatiquement et doit \u00eatre revu par un professionnel de sant\u00e9 qualifi\u00e9.",

    # Pre-test conditions
    section_pretest = "Conditions pr\u00e9-test",
    label_nutritional_state = "\u00c9tat nutritionnel",
    label_hours_ago = "h",
    label_fatigue_state = "\u00c9tat de fatigue",
    label_medications = "M\u00e9dicaments",
    label_caffeine = "Caf\u00e9ine",

    # Protocol details
    section_protocol_details = "D\u00e9tails du protocole",
    label_modality = "Modalit\u00e9",
    label_starting_intensity = "Intensit\u00e9 initiale",
    label_increment = "Incr\u00e9ment",
    label_stage_duration = "Dur\u00e9e du palier",
    label_equipment = "\u00c9quipement",
    label_analyzer = "Analyseur",

    # Stage table
    section_stage_table = "R\u00e9sultats par palier",

    # Economy metrics
    section_economy = "\u00c9conomie de mouvement",
    label_gross_efficiency = "Efficacit\u00e9 brute",
    label_running_economy = "\u00c9conomie de course",
    label_at_stage = "au palier",
    unit_ml_kg_km = "mL/kg/km"
  )

  if (language == "fr") labels_fr else labels_en
}


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
                                athlete_sport = NULL, athlete_level = "recreational") {
  language <- config@language
  data <- analysis@data
  participant <- data@participant
  metadata <- data@metadata
  peaks <- analysis@peaks
  thresholds <- analysis@thresholds

  # Calculate BMI
  bmi <- participant@weight_kg / (participant@height_cm / 100)^2

  # Calculate predicted values
  predicted <- calculate_predicted_values(participant)

  # Format sex
  sex_label <- if (participant@sex == "M") {
    labels$label_male
  } else if (participant@sex == "F") {
    labels$label_female
  } else {
    "Other"
  }

  # Build base data
  template_data <- c(
    labels,
    list(
      # Header info
      institution = config@institution %||% "",
      lab_name = config@lab_name %||% "",
      report_date = format(Sys.Date(), "%Y-%m-%d"),
      logo_path = config@logo_path,

      # Patient info
      patient_name = participant@name,
      patient_id = participant@id,
      patient_dob = if (!is.null(participant@date_of_birth) && length(participant@date_of_birth) > 0) {
        format(participant@date_of_birth, "%Y-%m-%d")
      } else "",
      patient_age = participant@age,
      patient_sex = sex_label,
      patient_height = participant@height_cm,
      patient_weight = participant@weight_kg,
      patient_bmi = round(bmi, 1),
      patient_sport = participant@sport %||% "-",

      # Test info
      test_date = format(metadata@test_date, "%Y-%m-%d"),
      test_protocol = metadata@protocol,
      test_device = metadata@device,
      test_technician = config@technician %||% metadata@technician %||% "-",
      test_duration = format_duration(max(data@breaths$time_s)),
      test_reason = "-"
    )
  )

  # Add peak values if available
  if (!is.null(peaks) && length(peaks@vo2_peak) > 0) {
    vo2_percent <- round(100 * peaks@vo2_peak / predicted$vo2_max, 0)
    hr_percent <- if (!is.null(peaks@hr_peak) && length(peaks@hr_peak) > 0) {
      round(100 * peaks@hr_peak / predicted$hr_max, 0)
    } else NA

    template_data <- c(template_data, list(
      vo2_peak_value = round(peaks@vo2_kg_peak, 1),
      vo2_peak_percent = paste0(vo2_percent, "%"),
      hr_peak_value = if (!is.null(peaks@hr_peak)) round(peaks@hr_peak, 0) else "-",
      hr_peak_percent = if (!is.na(hr_percent)) paste0(hr_percent, "%") else "-",
      power_peak_value = if (!is.null(peaks@power_peak)) round(peaks@power_peak, 0) else "-",
      power_peak_wkg = if (!is.null(peaks@power_peak)) {
        round(peaks@power_peak / participant@weight_kg, 1)
      } else "-",

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
      power_peak = if (!is.null(peaks@power_peak)) round(peaks@power_peak, 0) else "-",
      power_predicted = round(predicted$power_max, 0),
      power_percent = if (!is.null(peaks@power_peak)) {
        round(100 * peaks@power_peak / predicted$power_max, 0)
      } else "-",
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
      threshold_method = thresholds@vt1_method %||% "V-slope",
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
      medication_list = if (!is.null(ptc@medication_names) && length(ptc@medication_names) > 0) {
        paste(ptc@medication_names, collapse = ", ")
      } else {
        if (language == "fr") "Aucun" else "None"
      },
      caffeine_intake = ptc@caffeine_intake %||% FALSE,
      caffeine_mg = ptc@caffeine_mg
    ))
  } else {
    template_data$has_pretest_conditions <- FALSE
  }

  # Protocol details if available
  if (!is.null(analysis@protocol_config)) {
    pc <- analysis@protocol_config
    intensity_unit <- if (pc@modality == "treadmill") "km/h" else "W"
    template_data <- c(template_data, list(
      has_protocol_details = TRUE,
      protocol_modality = pc@modality,
      protocol_modality_label = format_modality(pc@modality, language),
      starting_intensity = pc@starting_intensity,
      intensity_unit = intensity_unit,
      increment_size = pc@increment_size,
      stage_duration_s = pc@stage_duration_s,
      starting_grade = pc@starting_grade,
      grade_increment = pc@grade_increment,
      equipment_model = pc@equipment_model %||% "-",
      analyzer_model = pc@analyzer_model %||% "-"
    ))
  } else {
    template_data$has_protocol_details <- FALSE
  }

  # Stage-by-stage summary table
  if (!is.null(analysis@stage_summary) && nrow(analysis@stage_summary) > 0) {
    template_data$has_stage_table <- TRUE
    template_data$stage_table <- format_stage_table_typst(
      analysis@stage_summary,
      language
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
    template_data <- c(template_data, list(
      has_economy_metrics = TRUE,
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

  # Add visual interpretation data
  visual_interp <- generate_visual_interpretation(analysis, config@language)
  template_data <- c(template_data, visual_interp)

  # Add clinical notes
  # Add clinical notes if provided
  template_data$clinical_notes <- clinical_notes %||% ""
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

  template_data
}


#' Calculate Predicted Values
#'
#' @description
#' Calculates age/sex-specific predicted maximal values.
#' Uses standard prediction equations.
#'
#' @param participant Participant object
#' @return Named list of predicted values
#' @keywords internal
calculate_predicted_values <- function(participant) {
  age <- participant@age
  sex <- participant@sex
  weight <- participant@weight_kg
  height <- participant@height_cm

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


#' Generate Visual Interpretation Data
#'
#' @description
#' Generates data for visual interpretation gauges in the report.
#' Returns colors, percentages, and ratings for each domain.
#'
#' @param analysis CpetAnalysis object
#' @param language Language code
#' @return List with visual interpretation elements
#' @keywords internal
generate_visual_interpretation <- function(analysis, language = "en") {
  peaks <- analysis@peaks
  participant <- analysis@data@participant
  predicted <- calculate_predicted_values(participant)

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
    summary_parts <- c(summary_parts, sprintf("VO2max \u00e0 %d%% du pr\u00e9dit", vo2_pct))
    if (hr_pct > 0) {
      summary_parts <- c(summary_parts, sprintf("FC max \u00e0 %d%% du pr\u00e9dit", hr_pct))
    }
    if (rer_val >= 1.10) {
      summary_parts <- c(summary_parts, "effort maximal atteint (RER \u2265 1.10)")
    } else {
      summary_parts <- c(summary_parts, sprintf("RER pic = %.2f", rer_val))
    }
  } else {
    summary_parts <- c(summary_parts, sprintf("VO2max at %d%% of predicted", vo2_pct))
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
#' @return List with interpretation sections
#' @keywords internal
generate_auto_interpretation <- function(analysis, language = "en") {
  peaks <- analysis@peaks
  participant <- analysis@data@participant
  predicted <- calculate_predicted_values(participant)

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


#' Generate Report Graphs
#'
#' @description
#' Creates temporary graph files for inclusion in the report.
#'
#' @param analysis CpetAnalysis object
#' @param language Language code
#' @param athlete_sport Sport for normative comparison (optional)
#' @param athlete_level Competitive level
#' @return List with graph file paths
#' @keywords internal
generate_report_graphs <- function(analysis, language = "en",
                                   athlete_sport = NULL, athlete_level = "recreational") {
  temp_dir <- tempdir()

  # Generate 9-panel plot
  panel_file <- file.path(temp_dir, "cpet_panel.png")
  p_panel <- plot_cpet_panel(analysis, language = language)
  ggplot2::ggsave(panel_file, p_panel, width = 10, height = 10, dpi = 150)

  # Generate V-slope plot
  vslope_file <- file.path(temp_dir, "vslope.png")
  p_vslope <- plot_v_slope(analysis, language = language)
  ggplot2::ggsave(vslope_file, p_vslope, width = 6, height = 5, dpi = 150)

  # Generate predicted comparison plot (with optional athlete norms)
  predicted_file <- file.path(temp_dir, "predicted_comparison.png")
  p_predicted <- plot_predicted_comparison(
    analysis,
    sport = athlete_sport,
    level = athlete_level,
    language = language,
    show_citation = TRUE
  )
  ggplot2::ggsave(predicted_file, p_predicted, width = 10, height = 5, dpi = 150)

  list(
    graph_panel = panel_file,
    graph_vslope = vslope_file,
    graph_predicted = predicted_file
  )
}


#' Clean Up Temporary Files
#'
#' @param files List of file paths to remove
#' @keywords internal
cleanup_temp_files <- function(files) {
  for (f in files) {
    if (is.character(f) && file.exists(f)) {
      unlink(f)
    }
  }
}


#' Get Template Path
#'
#' @param custom_template Optional custom template path
#' @return Path to Typst template
#' @keywords internal
get_template_path <- function(custom_template = NULL) {
  if (!is.null(custom_template) && length(custom_template) > 0 && file.exists(custom_template)) {
    return(custom_template)
  }

  system.file("templates", "cpet_report.typ", package = "cardiometR")
}


#' Process Template Conditionals
#'
#' @description
#' Recursively processes mustache-style conditionals in template content.
#' Handles nested conditionals correctly by finding matching pairs.
#'
#' @param content Template content string
#' @param data Named list of template data
#' @return Processed template content
#' @keywords internal
process_conditionals <- function(content, data) {
  # Find the first {{#if ...}} tag
  if_start <- regexpr("\\{\\{#if ([^}]+)\\}\\}", content, perl = TRUE)
  if (if_start == -1) return(content)

  # Extract variable name
  match_len <- attr(if_start, "match.length")
  tag <- substr(content, if_start, if_start + match_len - 1)
  var_name <- trimws(sub("\\{\\{#if ([^}]+)\\}\\}", "\\1", tag))


  # Find the matching {{/if}} by counting nesting levels
  pos <- if_start + match_len
  depth <- 1
  else_pos <- NA
  end_pos <- NA

  while (pos <= nchar(content) && depth > 0) {
    # Check for {{#if at current position
    if (substr(content, pos, pos + 5) == "{{#if ") {
      depth <- depth + 1
      pos <- pos + 6
    # Check for {{/if}} at current position
    } else if (substr(content, pos, pos + 6) == "{{/if}}") {
      depth <- depth - 1
      if (depth == 0) {
        end_pos <- pos
      }
      pos <- pos + 7
    # Check for {{else}} at current position (only at depth 1)
    } else if (depth == 1 && substr(content, pos, pos + 7) == "{{else}}") {
      else_pos <- pos
      pos <- pos + 8
    } else {
      pos <- pos + 1
    }
  }

  # If no matching end found, return content unchanged

  if (is.na(end_pos)) return(content)

  # Extract if-content and else-content
  if_content_start <- if_start + match_len
  if (!is.na(else_pos)) {
    if_content <- substr(content, if_content_start, else_pos - 1)
    else_content <- substr(content, else_pos + 8, end_pos - 1)
  } else {
    if_content <- substr(content, if_content_start, end_pos - 1)
    else_content <- ""
  }

  # Check if variable is truthy
  var_value <- data[[var_name]]
  is_truthy <- !is.null(var_value) && length(var_value) > 0 &&
    !identical(var_value, FALSE) && !identical(var_value, "")

  # Choose replacement and recursively process it
  replacement <- if (is_truthy) if_content else else_content
  replacement <- process_conditionals(replacement, data)

  # Build result
  before <- if (if_start > 1) substr(content, 1, if_start - 1) else ""
  after <- if (end_pos + 7 <= nchar(content)) substr(content, end_pos + 7, nchar(content)) else ""

  # Recursively process the rest
  result <- paste0(before, replacement, after)
  process_conditionals(result, data)
}


#' Render Typst Report
#'
#' @description
#' Renders the Typst template with data interpolation.
#'
#' @param template_path Path to Typst template
#' @param data Named list of template data
#' @param output_file Output PDF path
#' @keywords internal
render_typst_report <- function(template_path, data, output_file) {
  # Read template
  template_content <- paste(readLines(template_path, warn = FALSE), collapse = "\n")

  # Process conditionals recursively to handle nesting properly
  template_content <- process_conditionals(template_content, data)

  # Simple mustache-style variable interpolation
  for (name in names(data)) {
    value <- data[[name]]
    if (is.null(value) || length(value) == 0) {
      value <- ""
    } else if (is.logical(value)) {
      # Skip logical values (used for conditionals)
      next
    } else {
      value <- as.character(value)
    }
    pattern <- paste0("\\{\\{", name, "\\}\\}")
    template_content <- gsub(pattern, value, template_content)
  }

  # Create temp directory for template and images
  temp_dir <- tempfile(pattern = "typst_")
  dir.create(temp_dir)

  # Copy any image files to the temp directory and update paths
  image_vars <- c("graph_panel", "graph_vslope", "graph_predicted", "logo_path")
  for (var in image_vars) {
    val <- data[[var]]
    if (!is.null(val) && length(val) > 0 && !anyNA(val) && nchar(val) > 0 && file.exists(val)) {
      old_path <- data[[var]]
      new_name <- basename(old_path)
      new_path <- file.path(temp_dir, new_name)
      file.copy(old_path, new_path)
      # Update the path in template_content to use just the filename
      template_content <- gsub(old_path, new_name, template_content, fixed = TRUE)
    }
  }

  # Write interpolated template
  temp_typ <- file.path(temp_dir, "report.typ")
  writeLines(template_content, temp_typ)

  # Render with typr
  if (requireNamespace("typr", quietly = TRUE)) {
    typr::typr_compile(input = temp_typ, output_file = output_file, output_format = "pdf")
  } else {
    # Fallback: try system typst
    result <- system2("typst", args = c("compile", temp_typ, output_file),
                      stdout = TRUE, stderr = TRUE)
    if (!file.exists(output_file)) {
      cli::cli_abort(c(
        "Failed to render Typst template",
        "i" = "Install the {.pkg typr} package or ensure Typst is installed",
        "x" = paste(result, collapse = "\n")
      ))
    }
  }

  # Clean up temp directory
  unlink(temp_dir, recursive = TRUE)
}


#' Format Duration
#'
#' @param seconds Duration in seconds
#' @return Formatted string (MM:SS)
#' @keywords internal
format_duration <- function(seconds) {
  mins <- floor(seconds / 60)
  secs <- round(seconds %% 60)
  sprintf("%d:%02d", mins, secs)
}


#' Format Nutritional State
#'
#' @param state Character: "fed" or "fasted"
#' @param language Language code ("en" or "fr")
#' @return Formatted string
#' @keywords internal
format_nutritional_state <- function(state, language = "en") {
  if (is.null(state) || length(state) == 0) return("-")
  if (language == "fr") {
    switch(state,
      fed = "Nourri",
      fasted = "\u00c0 jeun",
      state
    )
  } else {
    switch(state,
      fed = "Fed",
      fasted = "Fasted",
      state
    )
  }
}


#' Format Fatigue State
#'
#' @param state Character: "rested" or "fatigued"
#' @param language Language code ("en" or "fr")
#' @return Formatted string
#' @keywords internal
format_fatigue_state <- function(state, language = "en") {
  if (is.null(state) || length(state) == 0) return("-")
  if (language == "fr") {
    switch(state,
      rested = "Repos\u00e9",
      fatigued = "Fatigu\u00e9",
      state
    )
  } else {
    switch(state,
      rested = "Rested",
      fatigued = "Fatigued",
      state
    )
  }
}


#' Format Modality
#'
#' @param modality Character: "cycling" or "treadmill"
#' @param language Language code ("en" or "fr")
#' @return Formatted string
#' @keywords internal
format_modality <- function(modality, language = "en") {
  if (is.null(modality) || length(modality) == 0) return("-")
  if (language == "fr") {
    switch(modality,
      cycling = "Ergocycle",
      treadmill = "Tapis roulant",
      other = "Autre",
      modality
    )
  } else {
    switch(modality,
      cycling = "Cycle Ergometer",
      treadmill = "Treadmill",
      other = "Other",
      modality
    )
  }
}


#' Format Stage Table for Typst
#'
#' @description
#' Converts stage summary data frame to Typst table row syntax.
#'
#' @param stage_summary Data frame with stage summary data
#' @param language Language code ("en" or "fr")
#' @return Character string with complete Typst table
#' @keywords internal
format_stage_table_typst <- function(stage_summary, language = "en") {
  n_stages <- nrow(stage_summary)


  # Check if lactate data exists

has_lactate <- "lactate_mmol" %in% names(stage_summary) &&
    any(!is.na(stage_summary$lactate_mmol))

  # Define headers based on language
  if (language == "fr") {
    headers <- c("Palier", "Dur\u00e9e", "Intensit\u00e9", "FC", "VE", "VO2", "RER")
    if (has_lactate) headers <- c(headers, "Lactate")
  } else {
    headers <- c("Stage", "Duration", "Intensity", "HR", "VE", "VO2", "RER")
    if (has_lactate) headers <- c(headers, "Lactate")
  }

  # Build header row
  header_row <- paste0(
    "[*", headers, "*]",
    collapse = ", "
  )

  # Build data rows
  rows <- purrr::map_chr(seq_len(n_stages), function(i) {
    row <- stage_summary[i, ]

    # Get stage number (use row index if not in data)
    stage_num <- if ("stage" %in% names(row)) row$stage else i

    # Get duration
    duration_str <- if ("duration_s" %in% names(row) && !is.na(row$duration_s)) {
      format_duration(row$duration_s)
    } else {
      "-"
    }

    # Get intensity (power for cycling, speed for treadmill)
    intensity <- if ("power_w" %in% names(row) && !is.na(row$power_w) && row$power_w > 0) {
      sprintf("%.0f W", row$power_w)
    } else if ("speed_kmh" %in% names(row) && !is.na(row$speed_kmh)) {
      sprintf("%.1f km/h", row$speed_kmh)
    } else {
      "-"
    }

    # Get HR
    hr <- if ("hr_bpm" %in% names(row) && !is.na(row$hr_bpm)) {
      sprintf("%.0f", row$hr_bpm)
    } else {
      "-"
    }

    # Get VE
    ve <- if ("ve_l" %in% names(row) && !is.na(row$ve_l)) {
      sprintf("%.1f", row$ve_l)
    } else {
      "-"
    }

    # Get VO2
    vo2 <- if ("vo2_ml" %in% names(row) && !is.na(row$vo2_ml)) {
      sprintf("%.0f", row$vo2_ml)
    } else {
      "-"
    }

    # Get VCO2
    vco2 <- if ("vco2_ml" %in% names(row) && !is.na(row$vco2_ml)) {
      sprintf("%.0f", row$vco2_ml)
    } else {
      "-"
    }

    # Get RER
    rer <- if ("rer" %in% names(row) && !is.na(row$rer)) {
      sprintf("%.2f", row$rer)
    } else {
      "-"
    }

    # Build row - 7 columns standard, 8 with lactate
    if (has_lactate) {
      lactate <- if ("lactate_mmol" %in% names(row) && !is.na(row$lactate_mmol)) {
        sprintf("%.1f", row$lactate_mmol)
      } else {
        "-"
      }
      sprintf(
        "  [%s], [%s], [%s], [%s], [%s], [%s], [%s], [%s]",
        stage_num, duration_str, intensity, hr, ve, vo2, rer, lactate
      )
    } else {
      sprintf(
        "  [%s], [%s], [%s], [%s], [%s], [%s], [%s]",
        stage_num, duration_str, intensity, hr, ve, vo2, rer
      )
    }
  })

  # Build complete Typst table
  n_cols <- if (has_lactate) 8 else 7
  col_spec <- paste(rep("1fr", n_cols), collapse = ", ")

  sprintf(
    '#table(
  columns: (%s),
  align: center,
  stroke: 0.5pt + luma(200),
  inset: 6pt,
  fill: (_, row) => if row == 0 { luma(240) } else { none },
  %s,
%s
)',
    col_spec,
    header_row,
    paste(rows, collapse = ",\n")
  )
}


#' Get Institution Logo Path
#'
#' @description
#' Returns the path to a built-in institution logo for use in reports.
#' Available logos: "udem" (UdeM - Ecole de kinesiologie),
#' "epic" (Centre EPIC - Institut de Cardiologie de Montreal).
#'
#' @param institution Institution identifier: "udem" or "epic"
#'
#' @return Character string with the full path to the logo file
#'
#' @examples
#' # Get UdeM logo
#' logo <- get_logo("udem")
#'
#' # Use in report config
#' \dontrun{
#' config <- ReportConfig(
#'   institution = "Ecole de kinesiologie, UdeM",
#'   logo_path = get_logo("udem")
#' )
#' }
#'
#' @export
get_logo <- function(institution = c("udem", "epic")) {
  institution <- match.arg(institution)

  logo_file <- switch(institution,
    "udem" = "Ec-_kinesiologie_-act_-phy_officiel-RVB.png",
    "epic" = "Centre_EPIC_ICM.jpg"
  )

  logo_path <- system.file("assets", logo_file, package = "cardiometR")

  if (!file.exists(logo_path)) {
    cli::cli_warn("Logo file not found: {.file {logo_file}}")
    return(NULL)
  }

  logo_path
}


#' Create Summary Table
#'
#' @description
#' Creates a gt summary table for CPET results.
#'
#' @param analysis CpetAnalysis object
#' @param language Language code
#'
#' @return A gt table object
#'
#' @export
create_summary_table <- function(analysis, language = "en") {
  if (!requireNamespace("gt", quietly = TRUE)) {
    cli::cli_abort("Package {.pkg gt} is required for tables")
  }

  peaks <- analysis@peaks
  participant <- analysis@data@participant
  predicted <- calculate_predicted_values(participant)

  if (is.null(peaks) || length(peaks@vo2_peak) == 0) {
    cli::cli_abort("No peak values available in analysis")
  }

  # Build summary data
  summary_data <- tibble::tibble(
    Parameter = c(
      "VO2 peak (mL/min)",
      "VO2 peak (mL/kg/min)",
      "VE peak (L/min)",
      "HR peak (bpm)",
      "RER peak",
      "Power peak (W)"
    ),
    Value = c(
      round(peaks@vo2_peak, 0),
      round(peaks@vo2_kg_peak, 1),
      round(peaks@ve_peak, 1),
      if (!is.null(peaks@hr_peak)) round(peaks@hr_peak, 0) else NA,
      round(peaks@rer_peak, 2),
      if (!is.null(peaks@power_peak)) round(peaks@power_peak, 0) else NA
    ),
    Predicted = c(
      round(predicted$vo2_max, 0),
      round(predicted$vo2_max / participant@weight_kg, 1),
      round(predicted$ve_max, 0),
      round(predicted$hr_max, 0),
      NA,
      round(predicted$power_max, 0)
    )
  )

  summary_data <- summary_data |>
    dplyr::mutate(
      `% Predicted` = dplyr::if_else(
        !is.na(Predicted) & !is.na(Value),
        round(100 * Value / Predicted, 0),
        NA_real_
      )
    )

  # Create gt table
  tbl <- gt::gt(summary_data) |>
    gt::tab_header(
      title = if (language == "fr") "R\u00e9sultats Maximaux" else "Peak Results"
    ) |>
    gt::fmt_number(columns = c(Value, Predicted), decimals = 0, use_seps = TRUE) |>
    gt::fmt_number(columns = `% Predicted`, decimals = 0, pattern = "{x}%") |>
    gt::sub_missing(missing_text = "-") |>
    gt::tab_style(
      style = gt::cell_fill(color = "#2E86AB20"),
      locations = gt::cells_body(rows = 1:2)
    ) |>
    gt::tab_options(
      table.font.size = gt::px(12),
      heading.title.font.size = gt::px(14),
      heading.title.font.weight = "bold"
    )

  tbl
}

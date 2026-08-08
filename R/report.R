# Report Generation Functions for cardiometR
# Generates bilingual PDF reports using Typst

report_graph_cache <- new.env(parent = emptyenv())

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
#' @param report_sections Optional character vector of sections to include
#'   (e.g., c("protocol_details", "stage_table", "thresholds", "graphs")).
#'   When NULL, all sections with data are included.
#' @param signature_date Optional Date stamped next to the physician signature
#'   (defaults to `Sys.Date()`).
#'
#' @return Invisibly returns the output file path
#'
#' @examples
#' \dontrun{
#' analysis <- analyze_cpet(data)
#' config <- ReportConfig(language = "fr", institution = "Universite de Montreal")
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
                            interpretation = NULL,
                            report_sections = NULL,
                            signature_date = NULL) {

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
  prediction_source <- config@prediction_source

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
    athlete_level = athlete_level,
    report_sections = report_sections,
    signature_date = signature_date
  )

  # Generate graphs if requested (and section is enabled)
  graph_files <- list()
  has_graphs <- FALSE
  graphs_missing <- FALSE
  graphs_enabled <- is.null(report_sections) || "graphs" %in% report_sections
  if (include_graphs && graphs_enabled) {
    graph_files <- tryCatch(
      generate_report_graphs(analysis, language, athlete_sport, athlete_level,
                             prediction_source = prediction_source),
      error = function(e) {
        cli::cli_warn(c(
          "Graph generation failed; report will show a missing-graphs banner.",
          "i" = e$message
        ))
        list()
      }
    )
    if (length(graph_files) > 0) {
      on.exit(cleanup_temp_files(graph_files), add = TRUE)
      template_data <- c(template_data, graph_files)
      has_graphs <- TRUE
    } else {
      graphs_missing <- TRUE
      cli::cli_warn(c(
        "Critical physiology graphs are missing from the report.",
        "i" = "A banner is written into the PDF so the gap is not silent."
      ))
    }
  }
  template_data$has_graphs <- has_graphs
  template_data$graphs_missing <- graphs_missing
  template_data$graphs_missing_banner <- if (graphs_missing) {
    escape_typst(tr("graphs_missing_banner", language))
  } else {
    ""
  }

  # Get template path
  template_path <- get_template_path(config@template)

  # Render with Typst via typr
  render_typst_report(
    template_path = template_path,
    data = template_data,
    output_file = output_file
  )

  # Temporary graph files are cleaned via on.exit

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
    subtitle = "",

    # Section titles
    section_patient = "Patient Information",
    section_test = "Test Information",
    section_peak_values = "Peak Values",
    section_detailed_results = "Detailed Results",
    section_athlete_profile = "Athlete Profile",
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
    label_rer_short = "RER",

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
    report_kicker = "Clinical report \u2014 cardiopulmonary exercise test",
    report_short_title = "CPET report",
    label_record = "record",
    section_validity = "Test validity",
    section_analysis_params = "Analysis parameters",
    validity_confirmed = "Maximal effort confirmed",
    validity_not_confirmed = "Maximal effort not confirmed",
    label_criteria_of = "of the ACSM mandatory criteria",
    param_software = "Software",
    param_averaging = "Averaging",
    param_peak_rule = "Peak VO2",
    param_peak_rule_value = "highest %d s average",
    param_thresholds = "Thresholds",
    param_predicted = "Predicted values",
    param_exclusion = "Exclusion rule",
    param_exclusion_value = "+/- 3 SD (Lamarra)",
    param_render = "Render",
    param_render_value = "Typst - PDF",
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
    subtitle = "",

    # Section titles
    section_patient = "Informations du patient",
    section_test = "Informations du test",
    section_peak_values = "Valeurs maximales",
    section_detailed_results = "R\u00e9sultats d\u00e9taill\u00e9s",
    section_athlete_profile = "Profil de l'athl\u00e8te",
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
    label_rer_short = "QR",

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
    report_kicker = "Rapport clinique \u2014 \u00e9preuve d'effort cardiorespiratoire",
    report_short_title = "Rapport EECR",
    label_record = "dossier",
    section_validity = "Validit\u00e9 du test",
    section_analysis_params = "Param\u00e8tres de l'analyse",
    validity_confirmed = "Effort maximal confirm\u00e9",
    validity_not_confirmed = "Effort maximal non confirm\u00e9",
    label_criteria_of = "des crit\u00e8res obligatoires de l'ACSM",
    param_software = "Logiciel",
    param_averaging = "Moyennage",
    param_peak_rule = "VO2 de pointe",
    param_peak_rule_value = "moyenne %d s la plus \u00e9lev\u00e9e",
    param_thresholds = "Seuils",
    param_predicted = "Valeurs pr\u00e9dites",
    param_exclusion = "R\u00e8gle d'exclusion",
    param_exclusion_value = "+/- 3 ET (Lamarra)",
    param_render = "Rendu",
    param_render_value = "Typst - PDF",
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



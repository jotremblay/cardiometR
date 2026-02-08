#' Main Application UI
#'
#' @description
#' Builds the main Shiny UI using bslib page_navbar with five tabs:
#' Upload, Configure, Results, Quality, and Report.
#'
#' @return A Shiny UI definition.
#'
#' @keywords internal
app_ui <- function() {
  lang <- getOption("cardiometR.language", "fr")

  bslib::page_navbar(
    id = "main_navbar",
    title = shiny::tagList(
      shiny::img(
        src = "cardiometR/Ec-_kinesiologie_-act_-phy_officiel-RVB.png",
        alt = "UdeM Logo",
        height = "35px",
        class = "me-2"
      ),
      tr("app_title", lang)
    ),
    theme = bslib::bs_theme(
      version = 5,
      bootswatch = "flatly",
      primary = "#2E86AB",
      secondary = "#6c757d"
    ),
    fillable = TRUE,

    # Header with language toggle
    header = shiny::tagList(
      shiny::div(
        class = "container-fluid d-flex justify-content-end py-2",
        shiny::actionButton(
          "lang_switch",
          label = if (lang == "en") "FR" else "EN",
          class = "btn-outline-secondary btn-sm"
        )
      ),
      shiny::tags$script(shiny::HTML("
        Shiny.addCustomMessageHandler('update_nav_labels', function(labels) {
          document.querySelectorAll('.nav-link[data-value]').forEach(function(el) {
            var val = el.getAttribute('data-value');
            if (labels[val]) {
              var icon = el.querySelector('i, svg');
              el.textContent = ' ' + labels[val];
              if (icon) el.prepend(icon);
            }
          });
        });
        Shiny.addCustomMessageHandler('update_settings_badges', function(changed) {
          Object.keys(changed).forEach(function(key) {
            var btn = document.querySelector('.accordion-button[aria-controls*=\"' + key + '\"]');
            if (!btn) return;
            var badge = btn.querySelector('.settings-badge');
            if (changed[key]) {
              if (!badge) {
                badge = document.createElement('span');
                badge.className = 'settings-badge badge bg-primary ms-2';
                badge.style.fontSize = '0.65em';
                badge.textContent = '\\u2022';
                btn.appendChild(badge);
              }
            } else if (badge) {
              badge.remove();
            }
          });
        });
      "))
    ),

    # Tab: Upload
    bslib::nav_panel(
      title = tr("nav_upload", lang),
      value = "upload",
      icon = shiny::icon("upload"),
      mod_upload_ui("upload", lang)
    ),

    # Tab: Configure
    bslib::nav_panel(
      title = tr("nav_configure", lang),
      value = "configure",
      icon = shiny::icon("sliders"),
      shiny::actionButton(
        "back_to_upload",
        label = tr("back_to_upload", lang),
        icon = shiny::icon("arrow-left"),
        class = "btn-outline-secondary btn-sm mb-3"
      ),
      bslib::layout_columns(
        col_widths = c(5, 7),
        mod_participant_ui("participant", lang),
        mod_settings_ui("settings", lang)
      )
    ),

    # Tab: Results
    bslib::nav_panel(
      title = tr("nav_results", lang),
      value = "results",
      icon = shiny::icon("chart-line"),
      bslib::layout_columns(
        col_widths = c(4, 8),
        mod_results_ui("results", lang),
        mod_plots_ui("plots", lang)
      )
    ),

    # Tab: Quality
    bslib::nav_panel(
      title = tr("nav_quality", lang),
      value = "quality",
      icon = shiny::icon("clipboard-check"),
      mod_quality_ui("quality", lang)
    ),

    # Tab: Report
    bslib::nav_panel(
      title = tr("nav_report", lang),
      value = "report",
      icon = shiny::icon("file-pdf"),
      mod_report_ui("report", lang)
    )
  )
}

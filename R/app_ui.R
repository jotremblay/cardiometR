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
  app_page <- function(...) shiny::div(class = "app-page", ...)

  brand_path <- system.file("app/_brand.yml", package = "cardiometR")
  use_brand <- utils::packageVersion("bslib") >= "0.7.0" &&
    nzchar(brand_path) &&
    requireNamespace("brand.yml", quietly = TRUE)
  theme <- if (use_brand) {
    bslib::bs_theme(version = 5, brand = brand_path)
  } else {
    bslib::bs_theme(
      version = 5,
      bootswatch = "flatly",
      primary = "#2E86AB",
      secondary = "#6c757d"
    )
  }

  css_dir <- system.file("app/www", package = "cardiometR")
  app_css_dep <- htmltools::htmlDependency(
    name = "cardiometr-app",
    version = as.character(utils::packageVersion("cardiometR")),
    src = c(file = css_dir),
    stylesheet = "app.css"
  )

  bslib::page_navbar(
    id = "main_navbar",
    title = shiny::div(
      class = "app-brand",
      shiny::img(
        src = "cardiometR/Ec-_kinesiologie_-act_-phy_officiel-RVB.png",
        alt = "UdeM Logo",
        class = "app-logo app-logo-udem"
      ),
      shiny::img(
        src = "cardiometR/lpeba_logo.svg",
        alt = "LPEBA Logo",
        class = "app-logo app-logo-lab"
      ),
      shiny::span(class = "app-brand-title", tr("app_title", lang))
    ),
    theme = theme,
    fillable = TRUE,

    # Global app styles/scripts
    header = shiny::tagList(
      app_css_dep,
      shiny::tags$link(rel = "shortcut icon", type = "image/png",
                       href = "cardiometr_www/favicon.png"),
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
        Shiny.addCustomMessageHandler('update_text', function(data) {
          Object.keys(data).forEach(function(id) {
            var el = document.getElementById(id);
            if (el) el.textContent = data[id];
          });
        });
        Shiny.addCustomMessageHandler('update_input_label', function(data) {
          var label = document.querySelector('label[for=\"' + data.id + '\"]');
          if (label) label.textContent = data.label;
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
      app_page(mod_upload_ui("upload", lang))
    ),

    # Tab: Configure
    bslib::nav_panel(
      title = tr("nav_configure", lang),
      value = "configure",
      icon = shiny::icon("sliders"),
      app_page(
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
      )
    ),

    # Tab: Results
    bslib::nav_panel(
      title = tr("nav_results", lang),
      value = "results",
      icon = shiny::icon("chart-line"),
      app_page(
        bslib::layout_columns(
          col_widths = c(4, 8),
          mod_results_ui("results", lang),
          mod_plots_ui("plots", lang)
        )
      )
    ),

    # Tab: Quality
    bslib::nav_panel(
      title = tr("nav_quality", lang),
      value = "quality",
      icon = shiny::icon("clipboard-check"),
      app_page(mod_quality_ui("quality", lang))
    ),

    # Tab: Report
    bslib::nav_panel(
      title = tr("nav_report", lang),
      value = "report",
      icon = shiny::icon("file-pdf"),
      app_page(mod_report_ui("report", lang))
    ),
    bslib::nav_spacer(),
    bslib::nav_item(
      shiny::div(
        class = "app-toolbar",
        bslib::input_dark_mode(id = "dark_mode"),
        shiny::actionButton(
          "lang_switch",
          label = if (lang == "en") "FR" else "EN",
          class = "btn-outline-light btn-sm"
        )
      )
    )
  )
}

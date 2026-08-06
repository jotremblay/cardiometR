#' Brand Subtitle Shown Under the App Name
#'
#' @param lang Language code.
#' @return A character string, e.g. `"Analyse EECR - LPEBA - v0.6.0"`.
#'
#' @keywords internal
app_brand_subtitle <- function(lang) {
  paste(
    tr("app_subtitle", lang),
    "LPEBA",
    paste0("v", utils::packageVersion("cardiometR")),
    sep = " \u00b7 "
  )
}


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
      shiny::span(
        class = "app-logo-plate",
        shiny::img(
          src = "cardiometR/Ec-_kinesiologie_-act_-phy_officiel-RVB.png",
          alt = "Universite de Montreal - kinesiologie",
          class = "app-logo app-logo-udem"
        ),
        shiny::span(class = "app-logo-divider"),
        shiny::img(
          src = "cardiometR/Centre_EPIC_ICM.jpg",
          alt = "Centre EPIC - Institut de Cardiologie de Montreal",
          class = "app-logo app-logo-epic"
        ),
        shiny::span(class = "app-logo-divider"),
        shiny::img(
          src = "cardiometR/lpeba_logo.svg",
          alt = "LPEBA",
          class = "app-logo app-logo-lab"
        )
      ),
      shiny::div(
        class = "app-brand-text",
        shiny::span(class = "app-brand-title", "cardiometR"),
        shiny::span(
          class = "app-brand-subtitle",
          id = "app_brand_subtitle",
          app_brand_subtitle(lang)
        )
      )
    ),
    theme = theme,
    fillable = TRUE,
    navbar_options = bslib::navbar_options(bg = "#1f2d3d", theme = "dark"),

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
        Shiny.addCustomMessageHandler('update_button_label', function(data) {
          var el = document.querySelector(data.selector);
          if (!el) return;
          // Preserve child nodes (e.g. Shiny's hidden <input type=\"file\">)
          // by replacing only the first text node rather than textContent.
          var replaced = false;
          for (var i = 0; i < el.childNodes.length; i++) {
            var n = el.childNodes[i];
            if (n.nodeType === Node.TEXT_NODE && n.nodeValue.trim().length) {
              n.nodeValue = data.label;
              replaced = true;
              break;
            }
          }
          if (!replaced) el.insertBefore(document.createTextNode(data.label), el.firstChild);
        });
        Shiny.addCustomMessageHandler('localize_upload_progress', function(data) {
          var el = document.getElementById(data.id);
          if (!el) return;
          var container = el.closest('.form-group, .shiny-input-container') || el.parentNode;
          if (!container) return;
          var observer = new MutationObserver(function() {
            var bar = container.querySelector('.progress-bar');
            if (bar && bar.textContent && bar.textContent.match(/Upload complete/i)) {
              bar.textContent = data.text;
            }
          });
          observer.observe(container, { childList: true, subtree: true, characterData: true });
          setTimeout(function() { observer.disconnect(); }, 10000);
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
          col_widths = c(5, 7),
          mod_results_ui("results", lang),
          mod_plots_ui("plots", lang, secondary_id = "results",
                       thresholds_id = "thresholds")
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

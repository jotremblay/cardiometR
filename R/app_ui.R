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
    theme = bslib::bs_theme(
      version = 5,
      bootswatch = "flatly",
      primary = "#2E86AB",
      secondary = "#6c757d"
    ),
    fillable = TRUE,

    # Global app styles/scripts
    header = shiny::tagList(
      shiny::tags$style(shiny::HTML("
        :root {
          --app-content-max-width: 1520px;
          --app-content-xpad: clamp(0.65rem, 1.2vw, 1.25rem);
          --app-content-ypad: clamp(0.8rem, 1.2vw, 1.4rem);
          --app-card-border: #e6edf5;
        }
        .bslib-page-navbar > .container-fluid.html-fill-item {
          padding-left: 0;
          padding-right: 0;
        }
        .navbar {
          box-shadow: 0 1px 6px rgba(17, 35, 58, 0.08);
          border-bottom: 1px solid rgba(255, 255, 255, 0.2);
        }
        .navbar > .container-fluid {
          display: flex;
          align-items: center;
          gap: 1rem;
        }
        .navbar .navbar-header {
          display: flex;
          align-items: center;
          margin-right: 0.5rem;
          flex-shrink: 0;
        }
        .navbar .navbar-brand {
          display: flex;
          align-items: center;
          margin-right: 0;
          padding-top: 0;
          padding-bottom: 0;
          min-height: 64px;
          line-height: 1;
        }
        .app-brand {
          display: flex;
          align-items: center;
          gap: 0.65rem;
          min-height: 64px;
        }
        .app-brand .app-logo {
          display: block;
          width: auto;
          object-fit: contain;
          flex-shrink: 0;
        }
        .app-brand .app-logo-udem {
          height: 52px;
        }
        .app-brand .app-logo-lab {
          height: 66px;
          max-width: none;
          transform: translateY(4px);
        }
        .app-brand-title {
          font-weight: 700;
          font-size: clamp(1.45rem, 1.2rem + 0.7vw, 2rem);
          line-height: 1;
          color: #1f2d3d;
          white-space: nowrap;
        }
        .navbar .navbar-nav {
          display: flex;
          align-items: center;
          gap: 0.12rem;
        }
        .navbar .nav-item {
          display: flex;
          align-items: center;
        }
        .navbar .nav-link {
          display: inline-flex;
          align-items: center;
          gap: 0.4rem;
          line-height: 1.05;
          padding-top: 0.85rem;
          padding-bottom: 0.85rem;
          padding-left: 0.75rem;
          padding-right: 0.75rem;
        }
        .navbar .nav-link i,
        .navbar .nav-link svg {
          line-height: 1;
          vertical-align: middle;
        }
        .navbar .nav-link.active {
          font-weight: 600;
        }
        .navbar .app-toolbar {
          display: flex;
          align-items: center;
          padding-left: 0.5rem;
        }
        .navbar .app-toolbar .btn {
          border-width: 1px;
          min-width: 3rem;
        }
        .bslib-page-navbar .tab-content {
          padding-top: 0.25rem;
        }
        .bslib-page-navbar .tab-pane {
          padding-left: var(--app-content-xpad);
          padding-right: var(--app-content-xpad);
        }
        .app-page {
          width: min(100%, var(--app-content-max-width));
          margin-left: auto;
          margin-right: auto;
          padding-top: var(--app-content-ypad);
          padding-bottom: var(--app-content-ypad);
        }
        .app-page > .bslib-grid,
        .app-page > .card {
          margin-bottom: 0;
        }
        .card {
          border: 1px solid var(--app-card-border);
          box-shadow: 0 1px 3px rgba(17, 35, 58, 0.05);
        }
        @media (max-width: 991.98px) {
          .navbar > .container-fluid {
            align-items: flex-start;
          }
          .navbar .navbar-brand {
            min-height: 44px;
          }
          .app-brand {
            min-height: 44px;
            gap: 0.45rem;
          }
          .app-brand .app-logo-udem {
            height: 38px;
          }
          .app-brand .app-logo-lab {
            height: 48px;
            transform: translateY(2px);
          }
          .app-brand-title {
            font-size: 1.2rem;
            white-space: normal;
            max-width: 13ch;
          }
          .bslib-page-navbar .tab-pane {
            padding-left: 0.55rem;
            padding-right: 0.55rem;
          }
          .app-page {
            padding-top: 0.7rem;
            padding-bottom: 0.9rem;
          }
        }
        @media (min-width: 992px) {
          .navbar .navbar-collapse.collapse {
            display: flex !important;
            align-items: center;
            justify-content: space-between;
          }
        }
        .plot-container { position: relative; }
        .plot-container .recalculating { opacity: 0.3; }
        .plot-container .recalculating::after {
          content: ''; position: absolute; top: 50%; left: 50%;
          width: 40px; height: 40px; margin: -20px;
          border: 4px solid #e0e0e0; border-top-color: #2E86AB;
          border-radius: 50%; animation: spin 0.8s linear infinite;
        }
        @keyframes spin { to { transform: rotate(360deg); } }

        /* Value box icon sizing refinement */
        .bslib-value-box .value-box-showcase .fa,
        .bslib-value-box .value-box-showcase .svg-inline--fa {
          font-size: 2.2rem;
          opacity: 0.85;
        }

        /* Upload dropzone styling */
        .upload-dropzone {
          border: 2px dashed var(--app-card-border);
          border-radius: 0.5rem;
          padding: 2rem;
          text-align: center;
          transition: border-color 0.2s, background-color 0.2s;
          background: #fafbfd;
        }
        .upload-dropzone:hover {
          border-color: #2E86AB;
          background: #f0f7fb;
        }
        .upload-dropzone .form-group {
          margin-bottom: 0;
        }

        /* Empty state styling */
        .empty-state {
          display: flex;
          flex-direction: column;
          align-items: center;
          justify-content: center;
          padding: 2.5rem 1rem;
          color: #9ca3af;
        }
        .empty-state .fa, .empty-state .svg-inline--fa {
          font-size: 2.5rem;
          margin-bottom: 0.75rem;
          opacity: 0.5;
        }
        .empty-state p {
          font-size: 0.9rem;
          margin: 0;
        }

        /* Criteria checklist */
        .criteria-list .criterion-item {
          border-bottom: 1px solid #f0f0f0;
        }
        .criteria-list .criterion-item:last-child {
          border-bottom: none;
        }

        /* Metrics list */
        .metrics-list > div {
          border-bottom: 1px solid #f0f0f0;
        }
        .metrics-list > div:last-child {
          border-bottom: none;
        }

        /* Quality metrics section backgrounds */
        .quality-metrics-section {
          background: #fafbfd;
          border-radius: 0.5rem;
          padding: 1rem;
          border: 1px solid var(--app-card-border);
        }

        /* Report preview paper effect */
        .report-preview-paper {
          background: white;
          border: 1px solid #e0e0e0;
          border-radius: 4px;
          padding: 1.5rem;
          box-shadow: 0 2px 8px rgba(0, 0, 0, 0.06);
          font-size: 0.92rem;
        }

        /* Better progress bars */
        .progress {
          height: 1.25rem;
          border-radius: 0.375rem;
        }
        .progress-bar {
          font-size: 0.75rem;
          font-weight: 600;
          line-height: 1.25rem;
        }

        /* Section dividers (replace plain hr) */
        .section-divider {
          border: none;
          border-top: 1px solid #edf0f5;
          margin: 1rem 0;
        }

        /* Stage/thresholds tables */
        .table-clean {
          font-size: 0.88rem;
        }
        .table-clean th {
          background: #f8f9fa;
          font-weight: 600;
          font-size: 0.82rem;
          text-transform: uppercase;
          letter-spacing: 0.03em;
          color: #6c757d;
          border-bottom: 2px solid #dee2e6;
        }
        .table-clean td {
          vertical-align: middle;
        }
      ")),
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
        shiny::actionButton(
          "lang_switch",
          label = if (lang == "en") "FR" else "EN",
          class = "btn-outline-light btn-sm"
        )
      )
    )
  )
}

#' Default gross mechanical efficiency for cycling (percent).
#'
#' Single source of truth for the cycling gross-efficiency fallback used by
#' the Shiny settings module and the plots module.
#' @keywords internal
default_gross_efficiency_pct <- 20L

#' Named choices for the plot-type select input.
#'
#' @param language Language code ("en" or "fr").
#' @return Named character vector suitable for `selectInput(choices = ...)`.
#' @keywords internal
plot_type_choices <- function(language) {
  stats::setNames(
    c("panel", "vslope", "vent_eq", "gas", "hr", "power", "predicted"),
    c(
      tr("plot_panel", language),
      tr("plot_vslope", language),
      tr("plot_vent_eq", language),
      tr("plot_gas", language),
      tr("plot_hr", language),
      tr("plot_power", language),
      tr("plot_predicted", language)
    )
  )
}

#' Plots Module UI
#'
#' @param id Module namespace ID.
#' @param language Language code ("en" or "fr").
#'
#' @return A Shiny UI element.
#'
#' @keywords internal
mod_plots_ui <- function(id, language = "en", secondary_id = NULL) {
  ns <- shiny::NS(id)

  plots_card <- bslib::card(
    fill = TRUE,
    bslib::card_header(
      class = "d-flex justify-content-between align-items-center",
      shiny::span(
        shiny::icon("chart-line"),
        shiny::span(id = ns("plots_header"), tr("section_graphs", language))
      ),
      shiny::div(
        class = "d-flex gap-2",
        shiny::selectInput(
          ns("plot_type"),
          label = NULL,
          choices = plot_type_choices(language),
          selected = "panel",
          width = "200px"
        ),
        shiny::selectInput(
          ns("download_format"),
          label = NULL,
          choices = c("PNG" = "png", "SVG" = "svg"),
          width = "110px"
        ),
        shiny::downloadButton(
          ns("download_plot"),
          label = NULL,
          icon = shiny::icon("download"),
          class = "btn-outline-secondary",
          title = tr("export", language)
        )
      )
    ),
    bslib::card_body(
      class = "p-2",
      shiny::conditionalPanel(
        condition = "input.plot_type == 'panel'",
        ns = ns,
        shiny::div(
          class = "plot-container",
          shiny::plotOutput(ns("static_plot"), height = "550px")
        )
      ),
      shiny::conditionalPanel(
        condition = "input.plot_type != 'panel'",
        ns = ns,
        shiny::div(
          class = "plot-container",
          plotly::plotlyOutput(ns("interactive_plot"), height = "550px")
        )
      )
    )
  )

  if (is.null(secondary_id)) {
    plots_card
  } else {
    shiny::tagList(
      plots_card,
      mod_results_secondary_ui(secondary_id)
    )
  }
}

#' Plots Module Server
#'
#' @param id Module namespace ID.
#' @param language Reactive language value.
#' @param analysis Reactive CpetAnalysis object from results module.
#' @param settings Reactive settings list from settings module (optional).
#'
#' @keywords internal
mod_plots_server <- function(id, language, analysis, settings = NULL,
                             dark_mode = shiny::reactive(FALSE)) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Read settings lazily inside per-plot branches so that changes to
    # settings-only inputs (averaging window, modality, dark mode) do not
    # invalidate the reactive for plot types that ignore them.
    read_settings <- function() {
      if (is.null(settings)) list() else settings()
    }

    current_plot <- shiny::reactive({
      a <- analysis()
      shiny::req(a)
      lang <- language()

      switch(input$plot_type,
        panel = {
          s <- read_settings()
          plot_cpet_panel(
            a,
            language = lang,
            averaging_window = s$averaging_window %||% 30,
            expected_efficiency = (s$gross_efficiency %||% default_gross_efficiency_pct) / 100,
            modality = s$modality %||% NULL,
            dark = isTRUE(dark_mode())
          )
        },
        vslope = plot_v_slope(a, language = lang),
        vent_eq = plot_ventilatory_equivalents(a, language = lang),
        gas = plot_gas_exchange(a, language = lang),
        hr = plot_heart_rate(a, language = lang),
        power = {
          s <- read_settings()
          plot_power(
            a,
            language = lang,
            expected_efficiency = (s$gross_efficiency %||% default_gross_efficiency_pct) / 100
          )
        },
        predicted = {
          s <- read_settings()
          sport_val <- s$athlete_sport
          sport <- if (!is.null(sport_val) && nchar(sport_val) > 0 && sport_val != "general") {
            sport_val
          } else {
            NULL
          }
          plot_predicted_comparison(
            a,
            sport = sport,
            level = s$athlete_level %||% "recreational",
            language = lang
          )
        },
        plot_cpet_panel(a, language = lang)
      )
    })

    output$static_plot <- shiny::renderPlot(
      {
        shiny::req(input$plot_type == "panel")
        p <- current_plot()
        shiny::req(p)
        p
      },
      res = 96,
      bg = "transparent"
    )

    output$interactive_plot <- plotly::renderPlotly({
      shiny::req(input$plot_type != "panel")
      p <- current_plot()
      shiny::req(p)
      plotly::ggplotly(p, tooltip = c("x", "y")) |>
        plotly::config(displayModeBar = TRUE, displaylogo = FALSE)
    })

    shiny::observeEvent(language(), {
      lang <- language()
      shiny::updateSelectInput(session, "plot_type", choices = plot_type_choices(lang))
      session$sendCustomMessage("update_text", as.list(stats::setNames(
        tr("section_graphs", lang),
        ns("plots_header")
      )))
    })

    output$download_plot <- shiny::downloadHandler(
      filename = function() {
        fmt <- input$download_format %||% "png"
        paste0("cpet_", input$plot_type, "_", format(Sys.Date(), "%Y%m%d"), ".", fmt)
      },
      content = function(file) {
        p <- current_plot()
        fmt <- input$download_format %||% "png"

        if (input$plot_type == "panel") {
          width <- 12
          height <- 12
        } else {
          width <- 8
          height <- 6
        }

        ggplot2::ggsave(
          file,
          plot = p,
          device = fmt,
          width = width,
          height = height,
          dpi = 300
        )
      }
    )
  })
}

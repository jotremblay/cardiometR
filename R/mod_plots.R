#' Plots Module UI
#'
#' @param id Module namespace ID.
#' @param language Language code ("en" or "fr").
#'
#' @return A Shiny UI element.
#'
#' @keywords internal
mod_plots_ui <- function(id, language = "en") {
  ns <- shiny::NS(id)

  bslib::card(
    fill = TRUE,
    bslib::card_header(
      class = "d-flex justify-content-between align-items-center",
      shiny::span(
        shiny::icon("chart-line"),
        tr("section_graphs", language)
      ),
      shiny::div(
        class = "d-flex gap-2",
        shiny::selectInput(
          ns("plot_type"),
          label = NULL,
          choices = stats::setNames(
            c("panel", "vslope", "vent_eq", "gas", "hr", "power", "predicted"),
            c(tr("plot_panel", language),
              tr("plot_vslope", language),
              tr("plot_vent_eq", language),
              tr("plot_gas", language),
              tr("plot_hr", language),
              tr("plot_power", language),
              tr("plot_predicted", language))
          ),
          selected = "panel",
          width = "200px"
        ),
        shiny::selectInput(
          ns("download_format"),
          label = NULL,
          choices = c("PNG" = "png", "SVG" = "svg"),
          width = "80px"
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
        shiny::plotOutput(ns("static_plot"), height = "550px")
      ),
      shiny::conditionalPanel(
        condition = "input.plot_type != 'panel'",
        ns = ns,
        plotly::plotlyOutput(ns("interactive_plot"), height = "550px")
      )
    )
  )
}

#' Plots Module Server
#'
#' @param id Module namespace ID.
#' @param language Reactive language value.
#' @param analysis Reactive CpetAnalysis object from results module.
#' @param settings Reactive settings list from settings module (optional).
#'
#' @keywords internal
mod_plots_server <- function(id, language, analysis, settings = NULL) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Generate plot based on selection
    current_plot <- shiny::reactive({
      a <- analysis()
      shiny::req(a)
      lang <- language()

      # Extract athlete sport/level from settings if available
      sport <- NULL
      level <- "recreational"
      if (!is.null(settings)) {
        s <- settings()
        sport_val <- s$athlete_sport
        if (!is.null(sport_val) && nchar(sport_val) > 0 && sport_val != "general") {
          sport <- sport_val
        }
        level <- s$athlete_level %||% "recreational"
      }

      switch(input$plot_type,
        panel = plot_cpet_panel(a, language = lang),
        vslope = plot_v_slope(a, language = lang),
        vent_eq = plot_ventilatory_equivalents(a, language = lang),
        gas = plot_gas_exchange(a, language = lang),
        hr = plot_heart_rate(a, language = lang),
        power = plot_power(a, language = lang),
        predicted = plot_predicted_comparison(a, sport = sport, level = level, language = lang),
        # Default
        plot_cpet_panel(a, language = lang)
      )
    })

    # Render static plot (9-panel patchwork)
    output$static_plot <- shiny::renderPlot({
      shiny::req(input$plot_type == "panel")
      p <- current_plot()
      shiny::req(p)
      p
    }, res = 96)

    # Render interactive plot (all other types)
    output$interactive_plot <- plotly::renderPlotly({
      shiny::req(input$plot_type != "panel")
      p <- current_plot()
      shiny::req(p)
      plotly::ggplotly(p, tooltip = c("x", "y")) |>
        plotly::config(displayModeBar = TRUE, displaylogo = FALSE)
    })

    # Update plot type dropdown on language change
    shiny::observeEvent(language(), {
      lang <- language()
      shiny::updateSelectInput(session, "plot_type",
        choices = stats::setNames(
          c("panel", "vslope", "vent_eq", "gas", "hr", "power", "predicted"),
          c(tr("plot_panel", lang), tr("plot_vslope", lang),
            tr("plot_vent_eq", lang), tr("plot_gas", lang),
            tr("plot_hr", lang), tr("plot_power", lang),
            tr("plot_predicted", lang))
        )
      )
    })

    # Download handler
    output$download_plot <- shiny::downloadHandler(
      filename = function() {
        fmt <- input$download_format %||% "png"
        paste0("cpet_", input$plot_type, "_", format(Sys.Date(), "%Y%m%d"), ".", fmt)
      },
      content = function(file) {
        p <- current_plot()
        fmt <- input$download_format %||% "png"

        # Determine dimensions based on plot type
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

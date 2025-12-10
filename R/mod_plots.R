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
        shiny::downloadButton(
          ns("download_plot"),
          label = NULL,
          icon = shiny::icon("download"),
          class = "btn-outline-secondary"
        )
      )
    ),
    bslib::card_body(
      class = "p-2",
      shiny::plotOutput(ns("main_plot"), height = "550px")
    )
  )
}

#' Plots Module Server
#'
#' @param id Module namespace ID.
#' @param language Reactive language value.
#' @param analysis Reactive CpetAnalysis object from results module.
#'
#' @keywords internal
mod_plots_server <- function(id, language, analysis) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Generate plot based on selection
    current_plot <- shiny::reactive({
      a <- analysis()
      shiny::req(a)
      lang <- language()

      switch(input$plot_type,
        panel = plot_cpet_panel(a, language = lang),
        vslope = plot_v_slope(a, language = lang),
        vent_eq = plot_ventilatory_equivalents(a, language = lang),
        gas = plot_gas_exchange(a, language = lang),
        hr = plot_heart_rate(a, language = lang),
        power = plot_power(a, language = lang),
        predicted = plot_predicted_comparison(a, language = lang),
        # Default
        plot_cpet_panel(a, language = lang)
      )
    })

    # Render plot
    output$main_plot <- shiny::renderPlot({
      p <- current_plot()
      shiny::req(p)
      p
    }, res = 96)

    # Download handler
    output$download_plot <- shiny::downloadHandler(
      filename = function() {
        paste0("cpet_", input$plot_type, "_", format(Sys.Date(), "%Y%m%d"), ".png")
      },
      content = function(file) {
        p <- current_plot()

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
          width = width,
          height = height,
          dpi = 300
        )
      }
    )
  })
}

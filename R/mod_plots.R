#' Convert ggplot plotmath labels in a plotly object to Unicode
#'
#' `plotly::ggplotly()` does not render ggplot2 `expression()` axis/title
#' labels — they appear literally as `"VO[2]"`. This helper walks the plotly
#' layout/traces and substitutes the common CPET plotmath tokens with
#' Unicode subscripts so the in-app interactive plots match the PNG version.
#'
#' @param p A plotly object.
#' @return The same plotly object with axis/title/legend text made Unicode.
#' @keywords internal
plotlymath_to_unicode <- function(p) {
  fix <- function(s) {
    if (is.null(s) || !length(s)) return(s)
    if (is.expression(s) || is.call(s) || is.name(s)) s <- as.character(s)
    if (!is.character(s)) return(s)
    s <- gsub("P\\[ET\\]\\s*\\*\\s*CO\\[2\\]", "P\u2091\u209cCO\u2082", s)
    s <- gsub("P\\[ET\\]\\s*\\*\\s*O\\[2\\]",  "P\u2091\u209cO\u2082",  s)
    s <- gsub("VCO\\[2\\]", "V\u0307CO\u2082", s)
    s <- gsub("VO\\[2\\]",  "V\u0307O\u2082",  s)
    s <- gsub("O\\[2\\]",   "O\u2082",         s)
    s <- gsub("CO\\[2\\]",  "CO\u2082",        s)
    s <- gsub("\\s*\\*\\s*", "", s)
    s <- gsub("\\s*~\\s*",   " ", s)
    s
  }
  lay <- p$x$layout
  if (!is.null(lay)) {
    for (ax in c("xaxis", "yaxis", "xaxis2", "yaxis2")) {
      if (!is.null(lay[[ax]]) && !is.null(lay[[ax]]$title)) {
        if (is.list(lay[[ax]]$title) && !is.null(lay[[ax]]$title$text)) {
          p$x$layout[[ax]]$title$text <- fix(lay[[ax]]$title$text)
        } else if (is.character(lay[[ax]]$title)) {
          p$x$layout[[ax]]$title <- fix(lay[[ax]]$title)
        }
      }
    }
    if (!is.null(lay$title)) {
      if (is.list(lay$title) && !is.null(lay$title$text)) {
        p$x$layout$title$text <- fix(lay$title$text)
      } else if (is.character(lay$title)) {
        p$x$layout$title <- fix(lay$title)
      }
    }
    if (!is.null(lay$annotations) && is.list(lay$annotations)) {
      p$x$layout$annotations <- lapply(lay$annotations, function(a) {
        if (!is.null(a$text)) a$text <- fix(a$text); a
      })
    }
  }
  if (!is.null(p$x$data)) {
    p$x$data <- lapply(p$x$data, function(tr) {
      if (!is.null(tr$name)) tr$name <- fix(tr$name)
      if (!is.null(tr$legendgroup)) tr$legendgroup <- fix(tr$legendgroup)
      tr
    })
  }
  p
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
        shiny::div(class = "plot-container",
          shiny::plotOutput(ns("static_plot"), height = "550px")
        )
      ),
      shiny::conditionalPanel(
        condition = "input.plot_type != 'panel'",
        ns = ns,
        shiny::div(class = "plot-container",
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

    # Generate plot based on selection
    current_plot <- shiny::reactive({
      a <- analysis()
      shiny::req(a)
      lang <- language()
      dark <- isTRUE(dark_mode())

      # Extract settings if available
      sport <- NULL
      level <- "recreational"
      avg_window <- 30
      gross_efficiency <- 0.20
      modality <- NULL
      if (!is.null(settings)) {
        s <- settings()
        sport_val <- s$athlete_sport
        if (!is.null(sport_val) && nchar(sport_val) > 0 && sport_val != "general") {
          sport <- sport_val
        }
        level <- s$athlete_level %||% "recreational"
        avg_window <- s$averaging_window %||% 30
        gross_efficiency <- (s$gross_efficiency %||% 20) / 100
        modality <- s$modality %||% NULL
      }

      switch(input$plot_type,
        panel = plot_cpet_panel(a, language = lang, averaging_window = avg_window,
          expected_efficiency = gross_efficiency, modality = modality, dark = dark),
        vslope = plot_v_slope(a, language = lang),
        vent_eq = plot_ventilatory_equivalents(a, language = lang),
        gas = plot_gas_exchange(a, language = lang),
        hr = plot_heart_rate(a, language = lang),
        power = plot_power(a, language = lang, expected_efficiency = gross_efficiency),
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
    }, res = 96, bg = "transparent")

    # Render interactive plot (all other types)
    output$interactive_plot <- plotly::renderPlotly({
      shiny::req(input$plot_type != "panel")
      p <- current_plot()
      shiny::req(p)
      plotly::ggplotly(p, tooltip = c("x", "y")) |>
        plotlymath_to_unicode() |>
        plotly::config(displayModeBar = TRUE, displaylogo = FALSE)
    })

    # Update plot type dropdown and static text on language change
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

      # Update card header text via JS
      session$sendCustomMessage("update_text", as.list(stats::setNames(
        tr("section_graphs", lang),
        ns("plots_header")
      )))
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

# Threshold review: the operator inspects the automatic detection, corrects
# it by hand, or compares the detection methods side by side.

#' Threshold Review Module UI
#'
#' @param id Module namespace ID.
#' @param language Language code ("en" or "fr").
#'
#' @return A Shiny UI element.
#'
#' @keywords internal
mod_thresholds_ui <- function(id, language = "en") {
  ns <- shiny::NS(id)

  bslib::card(
    class = "threshold-review",
    bslib::card_header(
      class = "d-flex align-items-center gap-2",
      shiny::span(
        shiny::icon("crosshairs"),
        shiny::span(id = ns("review_header"), tr("threshold_review", language))
      ),
      shiny::span(class = "badge-proposal", tr("threshold_proposal", language)),
      shiny::div(
        class = "ms-auto",
        shiny::radioButtons(
          ns("mode"),
          label = NULL,
          choices = stats::setNames(
            c("auto", "adjust", "compare"),
            c(tr("threshold_mode_auto", language),
              tr("threshold_mode_adjust", language),
              tr("threshold_mode_compare", language))
          ),
          selected = "auto",
          inline = TRUE
        )
      )
    ),
    bslib::card_body(
      shiny::uiOutput(ns("mode_banner")),

      shiny::conditionalPanel(
        condition = "input.mode == 'compare'",
        ns = ns,
        shiny::uiOutput(ns("method_cards"))
      ),

      shiny::div(
        class = "plot-container",
        shiny::plotOutput(ns("vslope_plot"), height = "340px")
      ),

      shiny::conditionalPanel(
        condition = "input.mode == 'adjust'",
        ns = ns,
        shiny::div(
          class = "threshold-handles",
          shiny::sliderInput(
            ns("vt1_vo2"),
            label = tr("vt1_short", language),
            min = 0, max = 1, value = 0, step = 10, width = "100%"
          ),
          shiny::sliderInput(
            ns("vt2_vo2"),
            label = tr("vt2_short", language),
            min = 0, max = 1, value = 1, step = 10, width = "100%"
          )
        )
      ),

      shiny::uiOutput(ns("readout")),
      shiny::tags$small(
        class = "text-muted d-block",
        shiny::textOutput(ns("mode_blurb"), inline = TRUE),
        " ",
        shiny::span(id = ns("audit_note"), tr("threshold_audit_note", language))
      )
    )
  )
}


#' Threshold Review Module Server
#'
#' @param id Module namespace ID.
#' @param language Reactive language value.
#' @param averaged_data Reactive averaged `CpetData`, or NULL.
#' @param settings Reactive settings list from the settings module.
#' @param override A `reactiveVal` the module writes the manual correction
#'   into. `NULL` means the automatic detection stands.
#'
#' @keywords internal
mod_thresholds_server <- function(id, language, averaged_data, settings,
                                  override) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # The automatic detection, recomputed whenever the data or the chosen
    # methods change. This is the proposal the operator reviews.
    auto_thresholds <- shiny::reactive({
      data <- averaged_data()
      if (is.null(data)) return(NULL)
      methods <- settings()$threshold_methods
      if (length(methods) == 0) return(NULL)
      tryCatch(
        detect_thresholds(data, methods = methods,
                          window_s = settings()$averaging_window %||% 30),
        error = function(e) NULL
      )
    })

    # Detection per method, for the comparison view.
    method_thresholds <- shiny::reactive({
      data <- averaged_data()
      if (is.null(data)) return(list())
      methods <- settings()$threshold_methods
      if (length(methods) == 0) return(list())
      stats::setNames(
        lapply(methods, function(m) {
          tryCatch(
            detect_thresholds(data, methods = m,
                              window_s = settings()$averaging_window %||% 30),
            error = function(e) NULL
          )
        }),
        methods
      )
    })

    vo2_range <- shiny::reactive({
      data <- averaged_data()
      if (is.null(data)) return(NULL)
      vo2 <- data@breaths$vo2_ml
      vo2 <- vo2[is.finite(vo2)]
      if (length(vo2) < 2) return(NULL)
      range(vo2)
    })

    # The sliders start on the automatic proposal, so "adjust" means
    # nudging the detection rather than starting from nothing.
    shiny::observeEvent(list(auto_thresholds(), vo2_range()), {
      rng <- vo2_range()
      th <- auto_thresholds()
      if (is.null(rng) || is.null(th)) return()

      vt1 <- th@vt1_vo2 %||% (rng[1] + diff(rng) * 0.4)
      vt2 <- th@vt2_vo2 %||% (rng[1] + diff(rng) * 0.75)
      if (!is.finite(vt1)) vt1 <- rng[1] + diff(rng) * 0.4
      if (!is.finite(vt2)) vt2 <- rng[1] + diff(rng) * 0.75

      shiny::updateSliderInput(session, "vt1_vo2",
                               min = round(rng[1]), max = round(rng[2]),
                               value = round(vt1))
      shiny::updateSliderInput(session, "vt2_vo2",
                               min = round(rng[1]), max = round(rng[2]),
                               value = round(vt2))
    })

    # Nothing is overridden until the operator leaves automatic mode.
    shiny::observeEvent(
      list(input$mode, input$vt1_vo2, input$vt2_vo2, input$keep_method),
      {
        mode <- input$mode %||% "auto"
        if (identical(mode, "auto")) {
          override(NULL)
          return()
        }
        if (identical(mode, "adjust")) {
          if (is.null(input$vt1_vo2) || is.null(input$vt2_vo2)) return()
          override(list(
            vt1_vo2 = input$vt1_vo2,
            vt2_vo2 = input$vt2_vo2,
            source = "manual"
          ))
        }
      }
    )

    # "Retenir" on a method card adopts that method's estimate.
    shiny::observeEvent(input$keep_method, {
      chosen <- input$keep_method
      th <- method_thresholds()[[chosen]]
      if (is.null(th)) return()
      override(list(
        vt1_vo2 = th@vt1_vo2,
        vt2_vo2 = th@vt2_vo2,
        source = chosen
      ))
      shiny::updateRadioButtons(session, "mode", selected = "adjust")
    })

    shiny::observeEvent(input$go_adjust, {
      shiny::updateRadioButtons(session, "mode", selected = "adjust")
    })

    # The thresholds currently in force: the override when there is one,
    # the automatic detection otherwise.
    current <- shiny::reactive({
      ov <- override()
      th <- auto_thresholds()
      if (is.null(ov)) return(th)
      list(vt1_vo2 = ov$vt1_vo2, vt2_vo2 = ov$vt2_vo2)
    })

    output$mode_banner <- shiny::renderUI({
      lang <- language()
      if (!identical(input$mode %||% "auto", "auto")) return(NULL)

      th <- auto_thresholds()
      if (is.null(th)) {
        return(shiny::div(class = "alert alert-warning py-2 mb-2",
                          tr("message_no_thresholds", lang)))
      }

      confidence <- switch(th@confidence %||% "low",
        high = tr("confidence_high", lang),
        moderate = tr("confidence_moderate", lang),
        tr("confidence_low", lang)
      )

      shiny::div(
        class = "threshold-banner",
        shiny::span(class = "fw-semibold", tr("threshold_auto_detected", lang)),
        shiny::span(sprintf("%s : %s", tr("threshold_confidence", lang), confidence)),
        shiny::span(class = "flex-grow-1"),
        shiny::actionButton(
          ns("go_adjust"),
          label = tr("threshold_correct_manually", lang),
          class = "btn-sm btn-outline-success"
        )
      )
    })

    output$method_cards <- shiny::renderUI({
      lang <- language()
      ths <- method_thresholds()
      if (length(ths) == 0) {
        return(shiny::div(class = "text-muted small", tr("message_no_thresholds", lang)))
      }

      shiny::div(
        class = "method-card-row",
        lapply(names(ths), function(m) {
          th <- ths[[m]]
          shiny::div(
            class = "method-card",
            shiny::div(
              class = "method-card-head",
              shiny::span(class = "fw-semibold", threshold_method_label(m, lang)),
              shiny::span(
                class = "method-card-tag",
                if (identical(override()$source, m)) tr("threshold_kept", lang) else ""
              )
            ),
            shiny::div(
              class = "method-card-values",
              shiny::div(
                shiny::div(class = "method-card-label", "SV1"),
                shiny::div(class = "method-card-value", fmt_vo2(th, "vt1"))
              ),
              shiny::div(
                shiny::div(class = "method-card-label", "SV2"),
                shiny::div(class = "method-card-value", fmt_vo2(th, "vt2"))
              )
            ),
            shiny::actionLink(
              ns("keep_method"),
              label = tr("threshold_keep", lang),
              class = "method-card-keep",
              onclick = sprintf(
                "Shiny.setInputValue('%s', '%s', {priority: 'event'})",
                ns("keep_method"), m
              )
            )
          )
        })
      )
    })

    output$vslope_plot <- shiny::renderPlot({
      data <- averaged_data()
      shiny::req(data)
      cur <- current()
      th <- if (is.null(cur)) {
        NULL
      } else if (inherits(cur, "cardiometR::Thresholds")) {
        cur
      } else {
        Thresholds(vt1_vo2 = cur$vt1_vo2, vt2_vo2 = cur$vt2_vo2)
      }
      plot_v_slope(data, thresholds = th, language = language())
    }, res = 96)

    output$readout <- shiny::renderUI({
      lang <- language()
      cur <- current()
      data <- averaged_data()
      if (is.null(cur) || is.null(data)) return(NULL)

      vt1 <- if (inherits(cur, "cardiometR::Thresholds")) cur@vt1_vo2 else cur$vt1_vo2
      vt2 <- if (inherits(cur, "cardiometR::Thresholds")) cur@vt2_vo2 else cur$vt2_vo2
      peak <- max(data@breaths$vo2_ml, na.rm = TRUE)

      cell <- function(label, value, unit) {
        shiny::div(
          class = "readout-cell",
          shiny::div(class = "readout-label", label),
          shiny::div(class = "readout-value", value),
          shiny::div(class = "readout-unit", unit)
        )
      }
      pct <- function(v) {
        if (is.null(v) || !is.finite(v) || !is.finite(peak) || peak <= 0) return("--")
        paste0(round(v / peak * 100), " %")
      }
      lmin <- function(v) {
        if (is.null(v) || !is.finite(v)) return("--")
        format(round(v / 1000, 2), nsmall = 2)
      }

      shiny::div(
        class = "readout-row",
        cell(paste("SV1", "VO2"), lmin(vt1), tr("unit_l_min", lang)),
        cell(paste("SV1", tr("pct_of_peak", lang)), pct(vt1), tr("peak_values", lang)),
        cell(paste("SV2", "VO2"), lmin(vt2), tr("unit_l_min", lang)),
        cell(paste("SV2", tr("pct_of_peak", lang)), pct(vt2), tr("peak_values", lang))
      )
    })

    output$mode_blurb <- shiny::renderText({
      lang <- language()
      switch(input$mode %||% "auto",
        compare = tr("threshold_blurb_compare", lang),
        adjust = tr("threshold_blurb_adjust", lang),
        tr("threshold_blurb_auto", lang)
      )
    })

    shiny::observeEvent(language(), {
      lang <- language()
      session$sendCustomMessage("update_text", as.list(stats::setNames(
        c(tr("threshold_review", lang), tr("threshold_audit_note", lang)),
        c(ns("review_header"), ns("audit_note"))
      )))
      shiny::updateRadioButtons(
        session, "mode",
        choices = stats::setNames(
          c("auto", "adjust", "compare"),
          c(tr("threshold_mode_auto", lang),
            tr("threshold_mode_adjust", lang),
            tr("threshold_mode_compare", lang))
        ),
        selected = input$mode %||% "auto",
        inline = TRUE
      )
      shiny::updateSliderInput(session, "vt1_vo2", label = tr("vt1_short", lang))
      shiny::updateSliderInput(session, "vt2_vo2", label = tr("vt2_short", lang))
    })

    list(override = override)
  })
}


#' Label for One Threshold Detection Method
#'
#' @param method Method key, e.g. `"v_slope"`.
#' @param language Language code.
#' @return A character label.
#'
#' @keywords internal
threshold_method_label <- function(method, language = "en") {
  switch(method,
    v_slope = "V-slope",
    ve_vo2 = "VE/VO2",
    ve_vco2 = "VE/VCO2",
    end_tidal = "PetO2 / PetCO2",
    peto2 = "PetO2",
    petco2 = "PetCO2",
    method
  )
}


# VO2 at one threshold, formatted for a comparison card.
fmt_vo2 <- function(thresholds, which = c("vt1", "vt2")) {
  which <- match.arg(which)
  if (is.null(thresholds)) return("--")
  value <- if (identical(which, "vt1")) thresholds@vt1_vo2 else thresholds@vt2_vo2
  if (is.null(value) || !is.finite(value)) return("--")
  paste(round(value), "mL/min")
}


#' Apply a Manual Threshold Correction
#'
#' @description
#' Replaces the detected VT1 and VT2 oxygen uptakes with the operator's
#' values, then reads the heart rate and power at those points from the
#' breath data so every derived number stays consistent.
#'
#' @param thresholds The detected [Thresholds], or `NULL`.
#' @param override A list with `vt1_vo2`, `vt2_vo2` and `source`.
#' @param data The averaged [CpetData] the correction was made on.
#'
#' @return A [Thresholds] object.
#'
#' @keywords internal
apply_threshold_override <- function(thresholds, override, data) {
  breaths <- data@breaths

  # Value of one column at the breath whose VO2 is closest to a target.
  at_vo2 <- function(target, column) {
    if (is.null(target) || !is.finite(target)) return(NULL)
    if (!column %in% names(breaths)) return(NULL)
    idx <- which.min(abs(breaths$vo2_ml - target))
    if (length(idx) == 0) return(NULL)
    value <- breaths[[column]][idx]
    if (!is.finite(value)) NULL else value
  }

  method <- override$source %||% "manual"

  Thresholds(
    vt1_vo2 = override$vt1_vo2,
    vt1_hr = at_vo2(override$vt1_vo2, "hr_bpm"),
    vt1_power = at_vo2(override$vt1_vo2, "power_w"),
    vt1_method = method,
    vt2_vo2 = override$vt2_vo2,
    vt2_hr = at_vo2(override$vt2_vo2, "hr_bpm"),
    vt2_power = at_vo2(override$vt2_vo2, "power_w"),
    vt2_method = method,
    confidence = if (identical(method, "manual")) {
      "high"
    } else if (!is.null(thresholds)) {
      thresholds@confidence
    } else {
      "moderate"
    }
  )
}

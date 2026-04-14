# cardiometR ggplot2 theme and palette helpers

#' Okabe-Ito Colorblind-Safe Palette for cardiometR
#'
#' @description
#' Returns a named vector of 8 Okabe-Ito colors suitable for colorblind
#' viewers. Includes a `patient` accent (blue, `#0072B2`) and a neutral
#' `stratum_band` (gray) meant for reference band shading.
#'
#' @return A named character vector of hex color codes.
#' @examples
#' pal <- palette_cardiometr()
#' pal["patient"]
#' @export
palette_cardiometr <- function() {
  c(
    patient       = "#0072B2",  # blue
    vermillion    = "#D55E00",
    bluish_green  = "#009E73",
    orange        = "#E69F00",
    sky_blue      = "#56B4E9",
    reddish_purple = "#CC79A7",
    yellow        = "#F0E442",
    stratum_band  = "#9AA0A6"   # neutral gray for reference bands
  )
}


#' cardiometR ggplot2 Theme
#'
#' @description
#' A minimalist ggplot2 theme built on [ggplot2::theme_minimal()] tuned for
#' cardiometR reports and the Shiny app. Uses an Inter/sans font stack,
#' strong axis titles, subtle panel grid, white background, and 0.8 lwd
#' axis lines. Designed to read well under light and dark UI surfaces.
#'
#' @param base_size Base font size (default 12).
#' @return A ggplot2 theme object.
#' @examples
#' library(ggplot2)
#' ggplot(mtcars, aes(wt, mpg)) + geom_point() + theme_cardiometr()
#' @export
theme_cardiometr <- function(base_size = 12) {
  ggplot2::theme_minimal(base_size = base_size, base_family = "") |>
    (\(th) th + ggplot2::theme(
      text = ggplot2::element_text(family = "Inter, Helvetica, Arial, sans-serif"),
      plot.background = ggplot2::element_rect(fill = "white", color = NA),
      panel.background = ggplot2::element_rect(fill = "white", color = NA),
      panel.grid.major = ggplot2::element_line(color = "#E5E7EB", linewidth = 0.3),
      panel.grid.minor = ggplot2::element_blank(),
      axis.line = ggplot2::element_line(color = "#374151", linewidth = 0.8),
      axis.ticks = ggplot2::element_line(color = "#374151", linewidth = 0.6),
      axis.title = ggplot2::element_text(face = "bold"),
      axis.title.x = ggplot2::element_text(margin = ggplot2::margin(t = 6)),
      axis.title.y = ggplot2::element_text(margin = ggplot2::margin(r = 6)),
      plot.title = ggplot2::element_text(face = "bold"),
      legend.position = "bottom",
      legend.background = ggplot2::element_rect(fill = "white", color = NA),
      strip.text = ggplot2::element_text(face = "bold")
    ))()
}


#' Stratum Reference Band Layer
#'
#' @description
#' Returns a list of ggplot2 layers (a shaded mean +/- SD rectangle and
#' LLN/ULN vertical outlines) that the caller can add to a plot with `+`.
#' Useful for normative strip plots where the patient value is compared
#' against a population stratum.
#'
#' @param mean Stratum mean.
#' @param sd Stratum standard deviation.
#' @param lln_z Lower-limit-of-normal in z-units (default -1.645, 5th pct).
#' @param uln_z Upper-limit-of-normal in z-units (default 1.645, 95th pct).
#' @return A list of ggplot2 layers.
#' @examples
#' library(ggplot2)
#' ggplot(data.frame(x = 50, y = 1), aes(x, y)) +
#'   stratum_band_layer(mean = 45, sd = 5) +
#'   geom_point(size = 4, color = palette_cardiometr()["patient"])
#' @export
stratum_band_layer <- function(mean, sd, lln_z = -1.645, uln_z = 1.645) {
  pal <- palette_cardiometr()
  list(
    ggplot2::geom_rect(
      ggplot2::aes(xmin = mean - sd, xmax = mean + sd,
                   ymin = -Inf, ymax = Inf),
      fill = pal[["stratum_band"]], alpha = 0.25,
      inherit.aes = FALSE
    ),
    ggplot2::geom_vline(xintercept = mean + lln_z * sd,
                        color = pal[["stratum_band"]],
                        linetype = "dashed", linewidth = 0.5),
    ggplot2::geom_vline(xintercept = mean + uln_z * sd,
                        color = pal[["stratum_band"]],
                        linetype = "dashed", linewidth = 0.5)
  )
}

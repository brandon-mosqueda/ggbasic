#' @import ggplot2
#' @importFrom rlang enquo quo_is_null

#' @include utils.R
#' @include theme.R

#' @title Hexagonal binning plot
#'
#' @description
#' Elegant and easy to implement hexagonal binning plots.
#'
#' @param bins (`numeric(1)`) Number of bins to use in both vertical and
#'   horizontal directions. `30` by default.
#' @param low_color (`character(1)`) Gradient color to use for low values.
#'   `#132B43` (blue) by default.
#' @param high_color (`character(1)`) Gradient color to use for high values.
#'   `#56B1F7` (light blue) by default.
#' @template base_params
#' @template y_param
#' @template y_breaks_num_param
#' @template x_breaks_num_param
#'
#' @return A (`ggplot2`) plot object.
#'
#' @family plots
#'
#' @example inst/examples/hex_plot.R
#'
#' @export
hex_plot <- function(data,
                     x,
                     y,
                     bins = 30,
                     facet_row = NULL,
                     facet_col = NULL,

                     title = NULL,
                     x_label = NULL,
                     y_label = NULL,

                     low_color = "#132B43",
                     high_color = "#56B1F7",
                     theme = "light",
                     y_breaks_num = 10,
                     x_breaks_num = 10,
                     font_size = 15,
                     x_angle = 0,
                     alpha = 1,
                     with_legend = TRUE,
                     horizontal = FALSE) {
  if (is_character(x)) {
    x <- rlang::sym(x)
  }

  if (is_character(y)) {
    y <- rlang::sym(y)
  }

  if (is_character(facet_row)) {
    facet_row <- rlang::sym(facet_row)
  }

  if (is_character(facet_col)) {
    facet_col <- rlang::sym(facet_col)
  }

  x <- rlang::enquo(x)
  y <- rlang::enquo(y)
  facet_row <- rlang::enquo(facet_row)
  facet_col <- rlang::enquo(facet_col)

  plot <- ggplot(data, aes(x = !!x, y = !!y)) +
    geom_hex(bins = bins, alpha = alpha) +
    scale_fill_gradient(low = low_color, high = high_color)

  return(base_format(
    plot = plot,
    title = title,
    x_label = x_label,
    y_label = y_label,
    facet_row = !!facet_row,
    facet_col = !!facet_col,
    y_breaks_num = y_breaks_num,
    x_breaks_num = x_breaks_num,
    theme = theme,
    fill_colors = NULL,
    font_size = font_size,
    x_angle = x_angle,
    with_legend = with_legend,
    horizontal = horizontal
  ))
}

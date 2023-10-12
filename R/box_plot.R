#' @import ggplot2
#' @importFrom rlang enquo quo_is_null

#' @include utils.R
#' @include theme.R

#' @title Box plot
#'
#' @description
#' Elegant and easy to implement box plots.
#'
#' @param outlier_color (`character(1)`) Color to use for outliers. `#ef3b2c`
#'   (red) by default.
#' @param outlier_size (`numeric(1)`) Size to use for outliers. `5` by default.
#' @template base_params
#' @template y_null_param
#' @template fill_by_param
#' @template fill_colors_param
#' @template color_param
#'
#' @return A (`ggplot2`) plot object.
#'
#' @family plots
#'
#' @example inst/examples/box_plot.R
#'
#' @export
box_plot <- function(data,
                     x,
                     y = NULL,
                     fill_by = NULL,
                     facet_row = NULL,
                     facet_col = NULL,

                     title = NULL,
                     x_label = NULL,

                     fill_colors = BREWER_COLORS,
                     color = "#386cb0",
                     outlier_color = "#ef3b2c",
                     outlier_size = 5,
                     theme = "light",
                     font_size = 15,
                     alpha = 0.9,
                     with_legend = TRUE,
                     horizontal = FALSE) {
  if (is_character(x)) {
    x <- rlang::sym(x)
  }

  if (is_character(y)) {
    y <- rlang::sym(y)
  }

  if (is_character(fill_by)) {
    fill_by <- rlang::sym(fill_by)
  }

  if (is_character(facet_row)) {
    facet_row <- rlang::sym(facet_row)
  }

  if (is_character(facet_col)) {
    facet_col <- rlang::sym(facet_col)
  }

  x <- rlang::enquo(x)
  y <- rlang::enquo(y)
  fill_by <- rlang::enquo(fill_by)
  facet_row <- rlang::enquo(facet_row)
  facet_col <- rlang::enquo(facet_col)

  if (rlang::quo_is_null(y)) {
    if (rlang::quo_is_null(fill_by)) {
      plot <- ggplot(data, aes(x = !!x)) +
        geom_boxplot(
          fill = color,
          outlier.color = outlier_color,
          outlier.fill = outlier_color,
          outlier.size = outlier_size,
          alpha = alpha
        )
    } else {
      plot <- ggplot(data, aes(x = !!x, fill = !!fill_by)) +
        geom_boxplot(
          outlier.color = outlier_color,
          outlier.fill = outlier_color,
          outlier.size = outlier_size,
          alpha = alpha
        )
    }
  } else {
    if (rlang::quo_is_null(fill_by)) {
      plot <- ggplot(data, aes(x = !!x, y = !!y)) +
        geom_boxplot(
          fill = color,
          outlier.color = outlier_color,
          outlier.fill = outlier_color,
          outlier.size = outlier_size,
          alpha = alpha
        )
    } else {
      plot <- ggplot(data, aes(x = !!x, y = !!y, fill = !!fill_by)) +
        geom_boxplot(
          outlier.color = outlier_color,
          outlier.fill = outlier_color,
          outlier.size = outlier_size,
          alpha = alpha
        )
    }
  }

  return(base_format(
    plot = plot,
    title = title,
    x_label = x_label,
    y_label = NULL,
    facet_row = !!facet_row,
    facet_col = !!facet_col,
    x_breaks_num = NULL,
    y_breaks_num = NULL,
    theme = theme,
    fill_colors = fill_colors,
    font_size = font_size,
    x_angle = 0,
    with_legend = with_legend,
    horizontal = horizontal
  ))
}

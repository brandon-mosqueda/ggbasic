#' @import ggplot2

#' @include utils.R

#' @title Scatter plot
#'
#' @description
#' Elegant and easy to implement scatter plots.
#'
#' @param point_size (`numeric(1)`) Size of the points. `2` by default.
#' @template base_params
#' @template y_param
#' @template fill_by_param
#' @template fill_colors_param
#' @template color_param
#' @template y_breaks_num_param
#' @template x_breaks_num_param
#'
#' @return A (`ggplot2`) plot object.
#'
#' @family plots
#'
#' @example inst/examples/scatter_plot.R
#'
#' @export
scatter_plot <- function(data,
                         x,
                         y,
                         fill_by = NULL,
                         facet_row = NULL,
                         facet_col = NULL,
                         facet_wrap = NULL,

                         title = NULL,
                         x_label = NULL,
                         y_label = NULL,

                         color = "#386cb0",
                         fill_colors = BREWER_COLORS,
                         point_size = 2,
                         theme = "light",
                         y_breaks_num = 10,
                         x_breaks_num = 10,
                         font_size = 15,
                         x_angle = 0,
                         alpha = 1,
                         with_legend = TRUE,
                         horizontal = FALSE) {
  x <- as_symbol(x)
  y <- as_symbol(y)
  fill_by <- as_symbol(fill_by)
  facet_row <- as_symbol(facet_row)
  facet_col <- as_symbol(facet_col)
  facet_wrap <- as_symbol(facet_wrap)

  with_legend <- with_legend & !is.null(fill_by)

  params <- list(
    stat = "identity",
    size = point_size,
    alpha = alpha
  )
  if (is.null(fill_by)) {
    params$color <- color
  }

  plot <- ggplot(data, aes(x = !!x, y = !!y, color = !!fill_by)) +
    do.call(geom_point, params)

  return(base_format(
    plot = plot,
    title = title,
    x_label = x_label,
    y_label = y_label,
    facet_row = facet_row,
    facet_col = facet_col,
    facet_wrap = facet_wrap,
    y_breaks_num = y_breaks_num,
    x_breaks_num = x_breaks_num,
    theme = theme,
    fill_colors = fill_colors,
    font_size = font_size,
    x_angle = x_angle,
    with_legend = with_legend,
    horizontal = horizontal
  ))
}
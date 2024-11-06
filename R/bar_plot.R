#' @import ggplot2
#' @importFrom rlang enquo quo_is_null

#' @include utils.R
#' @include theme.R

#' @title Bar plot
#'
#' @description
#' Elegant and easy to implement bar plots.
#'
#' @template base_params
#' @template y_null_param
#' @template fill_by_param
#' @template fill_colors_param
#' @template color_param
#' @template y_breaks_num_param
#'
#' @return A (`ggplot2`) plot object.
#'
#' @family plots
#'
#' @example inst/examples/bar_plot.R
#'
#' @export
bar_plot <- function(data,
                     x,
                     y = NULL,
                     fill_by = NULL,
                     facet_row = NULL,
                     facet_col = NULL,
                     facet_wrap = NULL,

                     title = NULL,
                     x_label = NULL,
                     y_label = NULL,

                     fill_colors = BREWER_COLORS,
                     color = "#386cb0",
                     theme = "light",
                     y_breaks_num = 10,
                     font_size = 15,
                     x_angle = 0,
                     alpha = 0.9,
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
    position = position_dodge(width = 0.92),
    alpha = alpha
  )
  if (is.null(fill_by)) {
    params$color <- color
    params$fill <- color
  }

  if (is.null(y)) {
    plot <- ggplot(data, aes(x = !!x, fill = !!fill_by))
    plot_funct <- geom_bar
  } else {
    plot <- ggplot(data, aes(x = !!x, y = !!y, fill = !!fill_by))
    plot_funct <- geom_col
  }

  plot <- plot + do.call(plot_funct, params)

  return(base_format(
    plot = plot,
    title = title,
    x_label = x_label,
    y_label = y_label,
    facet_row = facet_row,
    facet_col = facet_col,
    facet_wrap = facet_wrap,
    x_angle = x_angle,
    with_legend = with_legend,
    y_breaks_num = y_breaks_num,
    x_breaks_num = NULL,
    theme = theme,
    fill_colors = fill_colors,
    font_size = font_size,
    horizontal = horizontal
  ))
}

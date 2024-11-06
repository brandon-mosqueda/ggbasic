#' @import ggplot2

#' @include utils.R

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
                     facet_wrap = NULL,

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
  x <- as_symbol(x)
  y <- as_symbol(y)
  fill_by <- as_symbol(fill_by)
  facet_row <- as_symbol(facet_row)
  facet_col <- as_symbol(facet_col)
  facet_wrap <- as_symbol(facet_wrap)

  with_legend <- with_legend & !is.null(fill_by)

  params <- list(
    outlier.color = outlier_color,
    outlier.fill = outlier_color,
    outlier.size = outlier_size,
    alpha = alpha
  )

  if (is.null(fill_by)) {
    params$fill <- color
  }

  if (is.null(y)) {
    plot <- ggplot(data, aes(x = !!x, fill = !!fill_by))
  } else {
    plot <- ggplot(data, aes(x = !!x, y = !!y, fill = !!fill_by))
  }

  plot <- plot + do.call(geom_boxplot, params)

  return(base_format(
    plot = plot,
    title = title,
    x_label = x_label,
    y_label = NULL,
    facet_row = facet_row,
    facet_col = facet_col,
    facet_wrap = facet_wrap,
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

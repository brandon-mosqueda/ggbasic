#' @import ggplot2

#' @include utils.R

#' @title Line plot
#'
#' @description
#' Elegant and easy to implement line plots.
#'
#' @param line_type_by (`character`) Name of column to use to
#'   differentiate lines styles. If `NULL`, the plot will use solid lines for
#'   all lines. `NULL` by default.
#' @param with_points (`logical(1)`) Whether to add points to the plot. `TRUE`
#'   by default.
#' @param line_width (`numeric(1)`) Width of the line. `2` by default.
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
#' @example inst/examples/line_plot.R
#'
#' @export
line_plot <- function(data,
                      x,
                      y,
                      fill_by = NULL,
                      line_type_by = NULL,
                      facet_row = NULL,
                      facet_col = NULL,
                      facet_wrap = NULL,

                      title = NULL,
                      x_label = NULL,
                      y_label = NULL,

                      with_points = TRUE,
                      color = "#386cb0",
                      fill_colors = BREWER_COLORS,
                      line_width = 2,
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
  line_type_by <- as_symbol(line_type_by)

  with_legend <- with_legend & !is.null(fill_by)

  params <- list(alpha = alpha, size = line_width)
  if (is.null(fill_by)) {
    params$color <- color
  }

  plot <- ggplot(data, aes(
      x = !!x,
      y = !!y,
      color = !!fill_by,
      linetype = !!line_type_by
    )) +
    do.call(geom_line, params)

  if (with_points) {
    points_params <- list(
      stat = "identity",
      size = line_width,
      alpha = alpha
    )

    if (is.null(fill_by)) {
      points_params$color <- color
    }

    plot <- plot + do.call(geom_point, points_params)
  }

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

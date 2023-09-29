#' @import ggplot2
#' @importFrom rlang enquo quo_is_null

#' @include utils.R
#' @include theme.R

#' @title Bar plot
#'
#' @description
#' Basic bar plot.
#'
#' @param data (`data.frame`) Data to use for plot.
#' @param x (`quote`) Quoted name of column to use for x-axis.
#' @param y (`quote`) Quoted name of column to use for y-axis.
#' @param fill_by (`quote(1)`) Quoted name of column to use to fill the elements
#'   of the plot. If `NULL`, the plot will be filled with a single color. `NULL`
#'   by default.`
#' @param facet_row (`quote(1)`) Quoted name of column to use for row faceting.
#'  `NULL` by default.
#' @param facet_col (`quote(1)`) Quoted name of column to use for column
#'  faceting. `NULL` by default.
#' @param title (`character(1)`) Title of the plot. `NULL` by default.
#' @param x_label (`character(1)`) Label for x-axis. `NULL` by default.
#' @param y_label (`character(1)`) Label for y-axis. `NULL` by default.
#' @param fill_colors (`character`) Colors to use for filling the plot. Only
#'   used if `fill_by` is not `NULL`. `BREWER_COLORS` by default.
#' @param color (`character(1)`) Color to use for the plot. Only used when
#'   `fill_by` is `NULL`. `#386cb0` by default.
#' @param theme (`character(1)`) Theme to use for the plot. The options are:
#'   `"light"`,  `"dark_grey"`, `"dark_blue"` and `"transparent"`. `light` by
#'   default.
#' @param y_breaks_num (`numeric(1)`) Number of breaks to use for y-axis. `10`
#'   by default.
#' @param font_size (`numeric(1)`) Font size to use for the plot. `18` by
#'  default.
#' @param x_angle (`numeric(1)`) Angle to use for x-axis labels. `0`
#'   (horizontal) by default.
#' @param alpha (`numeric(1)`) Alpha value (transparency) to use for elements in
#'   the plot. `0.9` by default.
#' @param with_legend (`logical(1)`) Whether to include a legend in the plot.
#'  Only used when `fill_by` is not `NULL`. `TRUE` by default.
#' @param horizontal (`logical(1)`) Whether to plot the elements horizontally.
#'  `FALSE` by default.
#'
#' @return A (`ggplot`) plot object.
#'
#' @export
bar_plot <- function(data,
                     x,
                     y = NULL,
                     fill_by = NULL,
                     facet_row = NULL,
                     facet_col = NULL,

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
  x <- rlang::enquo(x)
  y <- rlang::enquo(y)
  fill_by <- rlang::enquo(fill_by)
  facet_row <- rlang::enquo(facet_row)
  facet_col <- rlang::enquo(facet_col)

  if (rlang::quo_is_null(y)) {
    if (rlang::quo_is_null(fill_by)) {
      plot <- ggplot(data, aes(x = !!x)) +
        geom_bar(
          position = position_dodge(width = 0.92),
          alpha = alpha,
          color = color,
          fill = color
        )

      with_legend <- FALSE
    } else {
      plot <- ggplot(data, aes(x = !!x, fill = !!fill_by)) +
        geom_bar(position = position_dodge(width = 0.92), alpha = alpha)
    }
  } else {
    if (rlang::quo_is_null(fill_by)) {
      plot <- ggplot(data, aes(x = !!x, y = !!y)) +
        geom_col(
          position = position_dodge(width = 0.92),
          alpha = alpha,
          color = color,
          fill = color
        )

      with_legend <- FALSE
    } else {
      plot <- ggplot(
        data,
        aes(x = !!x, y = !!y, fill = !!fill_by)
      ) + geom_col(position = position_dodge(width = 0.92), alpha = alpha)
    }
  }

  return(base_format(
    plot = plot,
    title = title,
    x_label = x_label,
    y_label = y_label,
    facet_row = !!facet_row,
    facet_col = !!facet_col,
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

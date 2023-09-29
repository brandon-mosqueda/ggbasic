#' @import ggplot2
#' @importFrom rlang enquo quo_is_null

#' @include utils.R
#' @include theme.R

#' @export
scatter_plot <- function(data,
                         x,
                         y,
                         fill_by = NULL,
                         facet_row = NULL,
                         facet_col = NULL,

                         title = NULL,
                         x_label = NULL,
                         y_label = NULL,

                         fill_color = "#386cb0",
                         fill_colors = BREWER_COLORS,
                         point_size = 2,
                         theme = "light",
                         y_breaks_num = 10,
                         x_breaks_num = 10,
                         font_size = 18,
                         x_angle = 0,
                         alpha = 1,
                         with_legend = TRUE,
                         horizontal = FALSE) {
  x <- rlang::enquo(x)
  y <- rlang::enquo(y)
  fill_by <- rlang::enquo(fill_by)
  facet_row <- rlang::enquo(facet_row)
  facet_col <- rlang::enquo(facet_col)

  if (rlang::quo_is_null(fill_by)) {
    plot <- ggplot(data, aes(x = !!x, y = !!y)) +
      geom_point(
        stat = "identity",
        size = point_size,
        color = fill_color,
        alpha = alpha
      )
  } else {
    plot <- ggplot(
      data,
      aes(x = !!x, y = !!y, color = !!fill_by)
    ) +
    geom_point(stat = "identity", size = point_size, alpha = alpha)
  }

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
    fill_colors = fill_colors,
    font_size = font_size,
    x_angle = x_angle,
    with_legend = with_legend,
    horizontal = horizontal
  ))
}
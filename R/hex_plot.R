#' @import ggplot2
#' @importFrom rlang enquo quo_is_null

#' @include utils.R
#' @include theme.R

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
  facet_row <- rlang::enquo(facet_row)
  facet_col <- rlang::enquo(facet_col)

  plot <- ggplot(data, aes(x = !!x, y = !!y)) +
    geom_hex(bins = bins)

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

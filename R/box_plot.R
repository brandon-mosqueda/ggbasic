#' @import ggplot2
#' @importFrom rlang enquo quo_is_null

#' @include utils.R
#' @include theme.R

#' @export
box_plot <- function(data,
                     x,
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
                     font_size = 18,
                     alpha = 0.9,
                     with_legend = TRUE,
                     horizontal = FALSE) {
  x <- rlang::enquo(x)
  fill_by <- rlang::enquo(fill_by)
  facet_row <- rlang::enquo(facet_row)
  facet_col <- rlang::enquo(facet_col)

  if (rlang::quo_is_null(fill_by)) {
    plot <- ggplot(data, aes(x = !!x)) +
      geom_boxplot(
        fill = color,
        outlier.color = outlier_color,
        outlier.fill = outlier_color,
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

  return(base_format(
    plot = plot,
    title = title,
    x_label = x_label,
    y_label = NULL,
    facet_row = !!facet_row,
    facet_col = !!facet_col,
    y_breaks_num = NULL,
    x_breaks_num = NULL,
    theme = theme,
    fill_colors = fill_colors,
    font_size = font_size,
    x_angle = 0,
    with_legend = with_legend,
    horizontal = horizontal
  ))
}

#' @import ggplot2
#' @importFrom rlang enquo quo_is_null

#' @include utils.R
#' @include theme.R

#' @export
hist_plot <- function(data,
                      x,
                      bins = 20,
                      fill_by = NULL,
                      facet_row = NULL,
                      facet_col = NULL,

                      title = NULL,
                      x_label = NULL,
                      y_label = "count",

                      color = "#386cb0",
                      fill_colors = BREWER_COLORS,
                      theme = "light",
                      x_breaks_num = NULL,
                      y_breaks_num = 10,
                      font_size = 15,
                      x_angle = 0,
                      alpha = 0.7,
                      with_legend = TRUE,
                      horizontal = FALSE) {
  x <- rlang::enquo(x)
  fill_by <- rlang::enquo(fill_by)
  facet_row <- rlang::enquo(facet_row)
  facet_col <- rlang::enquo(facet_col)

  if (rlang::quo_is_null(fill_by)) {
    plot <- ggplot(data, aes(x = !!x)) +
      geom_histogram(
        bins = bins,
        color = "black",
        fill = color,
        alpha = alpha
      )

    with_legend <- FALSE
  } else {
    plot <- ggplot(data, aes(x = !!x, fill = !!fill_by)) +
      geom_histogram(bins = bins, alpha = alpha, color = "black")
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
    x_breaks_num = x_breaks_num,
    theme = theme,
    fill_colors = fill_colors,
    font_size = font_size,
    horizontal = horizontal
  ))
}

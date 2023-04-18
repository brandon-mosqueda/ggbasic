#' @import ggplot2

#' @include utils.R
#' @include theme_publication.R

#' @export
ggbars <- function(data,
                   x,
                   y = NULL,
                   fill_by = x,
                   grid_by = NULL,

                   title = NULL,
                   x_label = NULL,
                   y_label = NULL,

                   font_size = 18,
                   x_angle = 0,
                   with_legend = TRUE,
                   horizontal = FALSE) {
  if (is.null(y)) {
    plot <- ggplot(data, aes(x = .data[[x]], fill = .data[[fill_by]])) +
      geom_bar(position = position_dodge(width = 0.92))
  } else {
    plot <- ggplot(
        data,
        aes(x = .data[[x]], y = .data[[y]], fill = .data[[fill_by]])
      ) +
      geom_col(position = position_dodge(width = 0.92))
  }

  return(base_format(
    plot = plot,
    title = title,
    x_label = x_label,
    y_label = y_label,
    grid_by = grid_by,
    x_angle = x_angle,
    with_legend = with_legend,
    font_size = font_size,
    horizontal = horizontal
  ))
}

#' @export
gghist <- function(data,
                   x,
                   bins = 20,
                   grid_by = NULL,

                   title = NULL,
                   x_label = NULL,
                   y_label = "count",

                   fill_color = "#386cb0",

                   font_size = 18,
                   x_angle = 0,
                   with_legend = TRUE,
                   horizontal = FALSE) {
  plot <- ggplot(data, aes(x = .data[[x]])) +
    geom_histogram(
      bins = bins,
      color = fill_color,
      fill = fill_color,
      alpha = 0.7
    ) +
    scale_x_continuous(n.breaks = 10) +
    scale_y_continuous(n.breaks = 10)

  return(base_format(
    plot = plot,
    title = title,
    x_label = x_label,
    y_label = y_label,
    grid_by = grid_by,
    x_angle = x_angle,
    with_legend = with_legend,
    font_size = font_size,
    horizontal = horizontal
  ))
}

ggbox <- function(data,
                  x,
                  fill_by = NULL,
                  grid_by = NULL,

                  title = NULL,
                  x_label = NULL,

                  fill_color = "#386cb0",
                  outlier_color = "#ef3b2c",

                  with_legend = TRUE,
                  font_size = 18,
                  horizontal = FALSE) {
  if (is.null(fill_by)) {
    plot <- ggplot(data, aes_string(x = .data[[x]])) +
      geom_boxplot(
        fill = fill_color,
        outlier.color = outlier_color,
        outlier.fill = outlier_color
      )
  } else {
    plot <- ggplot(data, aes(x = .data[[x]], fill = .data[[fill_by]])) +
      geom_boxplot(
        outlier.color = outlier_color,
        outlier.fill = outlier_color,
        outlier.size = 5
      )
  }

  return(base_format(
    plot = plot,
    title = title,
    x_label = x_label,
    y_label = NULL,
    grid_by = grid_by,
    x_angle = 0,
    with_legend = with_legend,
    font_size = font_size,
    horizontal = horizontal
  ))
}

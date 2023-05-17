#' @import ggplot2

#' @include utils.R
#' @include theme.R

#' @export
ggbars <- function(data,
                   x,
                   y = NULL,
                   fill_by = NULL,
                   grid_by = NULL,

                   title = NULL,
                   x_label = NULL,
                   y_label = NULL,

                   y_breaks_num = 10,
                   theme = "light",
                   fill_colors = BREWER_COLORS,
                   color = "#386cb0",
                   font_size = 18,
                   x_angle = 0,
                   alpha = 0.9,
                   with_legend = TRUE,
                   horizontal = FALSE) {
  if (is.null(y)) {
    if (is.null(fill_by)) {
      plot <- ggplot(data, aes(x = .data[[x]])) +
        geom_bar(
          position = position_dodge(width = 0.92),
          alpha = alpha,
          color = color,
          fill = color
        )
      with_legend <- FALSE
    } else {
      plot <- ggplot(data, aes(x = .data[[x]], fill = .data[[fill_by]])) +
        geom_bar(position = position_dodge(width = 0.92), alpha = alpha)
    }
  } else {
    if (is.null(fill_by)) {
      plot <- ggplot(data, aes(x = .data[[x]], y = .data[[y]])) +
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
        aes(x = .data[[x]], y = .data[[y]], fill = .data[[fill_by]])
      ) + geom_col(position = position_dodge(width = 0.92), alpha = alpha)
    }
  }

  return(base_format(
    plot = plot,
    title = title,
    x_label = x_label,
    y_label = y_label,
    grid_by = grid_by,
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

#' @export
gghist <- function(data,
                   x,
                   bins = 20,
                   grid_by = NULL,

                   title = NULL,
                   x_label = NULL,
                   y_label = "count",

                   fill_color = "#386cb0",

                   y_breaks_num = 10,
                   theme = "light",
                   fill_colors = BREWER_COLORS,
                   font_size = 18,
                   x_angle = 0,
                   alpha = 0.7,
                   with_legend = TRUE,
                   horizontal = FALSE) {
  plot <- ggplot(data, aes(x = .data[[x]])) +
    geom_histogram(
      bins = bins,
      color = fill_color,
      fill = fill_color,
      alpha = alpha
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
    y_breaks_num = y_breaks_num,
    x_breaks_num = NULL,
    theme = theme,
    fill_colors = fill_colors,
    font_size = font_size,
    horizontal = horizontal
  ))
}

#' @export
ggbox <- function(data,
                  x,
                  fill_by = NULL,
                  grid_by = NULL,

                  title = NULL,
                  x_label = NULL,

                  fill_color = "#386cb0",
                  outlier_color = "#ef3b2c",

                  theme = "light",
                  fill_colors = BREWER_COLORS,
                  font_size = 18,
                  with_legend = TRUE,
                  alpha = 0.9,
                  horizontal = FALSE) {
  if (is.null(fill_by)) {
    plot <- ggplot(data, aes_string(x = .data[[x]])) +
      geom_boxplot(
        fill = fill_color,
        outlier.color = outlier_color,
        outlier.fill = outlier_color,
        alpha = alpha
      )
  } else {
    plot <- ggplot(data, aes(x = .data[[x]], fill = .data[[fill_by]])) +
      geom_boxplot(
        outlier.color = outlier_color,
        outlier.fill = outlier_color,
        outlier.size = 5,
        alpha = alpha
      )
  }

  return(base_format(
    plot = plot,
    title = title,
    x_label = x_label,
    y_label = NULL,
    grid_by = grid_by,
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

#' @export
ggscatter <- function(data,
                      x,
                      y,
                      fill_by = NULL,
                      grid_by = NULL,

                      title = NULL,
                      x_label = NULL,
                      y_label = NULL,

                      fill_color = "#386cb0",
                      point_size = 2,
                      y_breaks_num = 10,
                      x_breaks_num = 10,
                      theme = "light",
                      fill_colors = BREWER_COLORS,
                      font_size = 18,
                      x_angle = 0,
                      alpha = 1,
                      with_legend = TRUE,
                      horizontal = FALSE) {
  if (is.null(fill_by)) {
    plot <- ggplot(data, aes(x = .data[[x]], y = .data[[y]])) +
      geom_point(
        stat = "identity",
        size = point_size,
        color = fill_color,
        alpha = alpha
      )
  } else {
    plot <- ggplot(
      data,
      aes(x = .data[[x]], y = .data[[y]], color = .data[[fill_by]])
    ) +
    geom_point(stat = "identity", size = point_size, alpha = alpha)
  }

  return(base_format(
    plot = plot,
    title = title,
    x_label = x_label,
    y_label = y_label,
    grid_by = grid_by,
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

#' @export
ggline <- function(data,
                   x,
                   y,
                   fill_by = NULL,
                   grid_by = NULL,

                   title = NULL,
                   x_label = NULL,
                   y_label = NULL,

                   fill_color = "#386cb0",
                   line_width = 2,
                   y_breaks_num = 10,
                   x_breaks_num = 10,
                   theme = "light",
                   fill_colors = BREWER_COLORS,
                   font_size = 18,
                   x_angle = 0,
                   alpha = 1,
                   with_legend = TRUE,
                   horizontal = FALSE) {
  if (is.null(fill_by)) {
    plot <- ggplot(data, aes(x = .data[[x]], y = .data[[y]])) +
      geom_line(color = fill_color, alpha = alpha)
  } else {
    plot <- ggplot(
      data,
      aes(
        x = .data[[x]],
        y = .data[[y]],
        color = .data[[fill_by]],
        alpha = alpha
      )
    ) +
    geom_line()
  }

  return(base_format(
    plot = plot,
    title = title,
    x_label = x_label,
    y_label = y_label,
    grid_by = grid_by,
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

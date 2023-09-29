#' @import ggplot2

#' @export
ggplot2::ggsave

rotate_x <- function(angle = 45) {
  return(theme(
    axis.text.x = element_text(angle = angle, hjust = 1)
  ))
}

gglabels <- function(plot, title = NULL, x_label = NULL, y_label = NULL) {
  if (!is.null(title)) {
    plot <- plot + ggtitle(title)
  }
  if (!is.null(x_label)) {
    plot <- plot + xlab(x_label)
  }
  if (!is.null(y_label)) {
    plot <- plot + ylab(y_label)
  }

  return(plot)
}

base_format <- function(plot,
                        facet_row,
                        facet_col,
                        title,
                        x_label,
                        y_label,
                        y_breaks_num,
                        x_breaks_num,
                        theme,
                        fill_colors,
                        font_size,
                        x_angle,
                        with_legend,
                        horizontal) {
  facet_row <- rlang::enquo(facet_row)
  facet_col <- rlang::enquo(facet_col)

  plot <- gglabels(plot, title = title, x_label = x_label, y_label = y_label)

  plot <- plot + facet_grid(
    rows = vars(!!facet_row),
    cols = vars(!!facet_col)
  )

  if (!is.null(y_breaks_num)) {
    plot <- plot + scale_y_continuous(n.breaks = y_breaks_num)
  }

  if (!is.null(x_breaks_num)) {
    plot <- plot + scale_x_continuous(n.breaks = x_breaks_num)
  }

  if (horizontal) {
    plot <- plot + coord_flip()
  }

  theme_function <- switch(
    tolower(theme),
    light = theme_publication,
    transparent = theme_transparent,
    dark_grey = theme_dark_grey,
    dark_blue = theme_dark_blue
  )
  plot <- theme_function(plot, font_size = font_size, fill_colors = fill_colors)

  if (!with_legend) {
    plot <- plot + theme(legend.position = "none")
  }

  if (x_angle != 0) {
    plot <- plot + rotate_x(x_angle)
  }

  return(plot)
}

#' @export
error_intervals <- function(value,
                            standar_error,
                            margin = 1.96,
                            line_width = 0.2,
                            color = "black") {
  value <- rlang::enquo(value)
  standar_error <- rlang::enquo(standar_error)

  return(ggplot2::geom_errorbar(
    ggplot2::aes(
      ymin = !!value - margin * !!standar_error,
      ymax = !!value + margin * !!standar_error
    ),
    width = line_width,
    linewidth = line_width,
    position = ggplot2::position_dodge(0.9),
    colour = color
  ))
}

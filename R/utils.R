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
                        grid_by,
                        title,
                        x_label,
                        y_label,
                        theme,
                        fill_colors,
                        font_size,
                        x_angle,
                        with_legend,
                        horizontal) {
  plot <- gglabels(plot, title = title, x_label = x_label, y_label = y_label)

  if (!is.null(grid_by)) {
    # scales = "free" remove empty factors
    plot <- plot + facet_grid(
      as.formula(sprintf("~ %s", grid_by)),
      scales = "free",
      space = "free"
    )
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

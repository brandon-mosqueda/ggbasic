#' @import ggplot2

#' @export
save_plot <- function(Plot, file, width = 35, height = 20, units = "cm", ...) {
  background <- NULL
  if ("transparent" %in% class(Plot)) {
    background <- "transparent"
  }

  ggplot2::ggsave(
    file,
    Plot,
    width = width,
    height = height,
    bg = background,
    units = units,
    ...
  )

  return(invisible(Plot))
}

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
                        x_angle,
                        with_legend,
                        font_size,
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

  plot <- theme_publication(plot, font_size)

  if (!with_legend) {
    plot <- plot + theme(legend.position = "none")
  }

  if (x_angle != 0) {
    plot <- plot + rotate_x(x_angle)
  }

  return(plot)
}

#' @import ggplot2

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

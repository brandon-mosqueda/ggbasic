#' @import ggplot2

#' @include utils.R
#' @include theme_publication.R

#' @export
ggbars <- function(data,
                   x,
                   y = NULL,
                   fill_by = x,
                   grid_by = NULL,

                   x_label = NULL,
                   y_label = NULL,

                   x_angle = 0,
                   font_size = 18,
                   with_legend = TRUE,
                   horizontal = FALSE) {
  if (missing(y)) {
    plot <- ggplot(data, aes(x = {{x}}, fill = {{fill_by}})) +
      geom_bar(position = position_dodge(width = 0.92))
  } else {
    plot <- ggplot(data, aes(x = {{x}}, y = {{y}}, fill = {{fill_by}})) +
      geom_col(position = position_dodge(width = 0.92))
  }

  if (!is.null(x_label)) {
    plot <- plot + xlab(x_label)
  }
  if (!is.null(y_label)) {
    plot <- plot + ylab(y_label)
  }

  if (!missing(grid_by)) {
    # scales = "free" remove empty factors
    plot <- plot + facet_grid(
      cols = vars({{grid_by}}),
      scales = "free",
      space = "free"
    )
  }

  if (horizontal) {
    plot <- plot + coord_flip()
  }

  plot <- plot +
    theme_publication(font_size) +
    scale_fill_publication() +
    scale_colour_publication()

  if (!with_legend) {
    plot <- plot + theme(legend.position = "none")
  }

  if (x_angle != 0) {
    plot <- plot + rotate_x(x_angle)
  }

  return(plot)
}

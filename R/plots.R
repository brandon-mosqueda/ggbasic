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
  if (missing(y)) {
    plot <- ggplot(data, aes(x = {{x}}, fill = {{fill_by}})) +
      geom_bar(position = position_dodge(width = 0.92))
  } else {
    plot <- ggplot(data, aes(x = {{x}}, y = {{y}}, fill = {{fill_by}})) +
      geom_col(position = position_dodge(width = 0.92))
  }

  plot <- gglabels(plot, title = title, x_label = x_label, y_label = y_label)

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

  plot <- theme_publication(plot, font_size)

  if (!with_legend) {
    plot <- plot + theme(legend.position = "none")
  }

  if (x_angle != 0) {
    plot <- plot + rotate_x(x_angle)
  }

  return(plot)
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
                   with_legend = TRUE) {
  plot <- ggplot(data, aes(x = {{x}})) +
    geom_histogram(
      bins = bins,
      color = fill_color,
      fill = fill_color,
      alpha = 0.7
    ) +
    scale_x_continuous(n.breaks = 10) +
    scale_y_continuous(n.breaks = 10)

  plot <- gglabels(plot, title = title, x_label = x_label, y_label = y_label)

  if (!missing(grid_by)) {
    # scales = "free" remove empty factors
    plot <- plot + facet_grid(
      cols = vars({{ grid_by }}),
      scales = "free",
      space = "free"
    )
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

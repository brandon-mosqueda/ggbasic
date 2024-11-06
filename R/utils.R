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
                        facet_wrap,
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
  plot <- gglabels(plot, title = title, x_label = x_label, y_label = y_label)

  if (is.null(facet_wrap)) {
    plot <- plot + facet_grid(
      rows = vars(!!facet_row),
      cols = vars(!!facet_col)
    )
  } else {
    plot <- plot + ggplot2::facet_wrap(vars(!!facet_wrap))
  }

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
    dark_blue = theme_dark_blue,
    paper = theme_paper
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

#' @title Error intervals
#'
#' @description
#' Add error intervals to a plot.
#'
#' @param value (`character`) Name of column to take the reference
#'   value for the error intervals.
#' @param standard_error (`character`) Name of column to take the
#'   standard error for the error intervals.
#' @param margin (`numeric(1)`) Margin to use for the error intervals. Margin
#'   applies on both sides. Error bar goes from
#'   `value - margin * standard_error` to `value + margin * standard_error`.
#'   `1.96` by default.
#' @param line_width (`numeric(1)`) Width of the error bar. `0.5` by default.
#' @param color (`character(1)`) Color to use for the error bar. `"black"` by
#'   default.
#'
#' @return
#' A `ggplot2::geom_errorbar` object.
#'
#' @example inst/examples/error_intervals.R
#'
#' @export
error_intervals <- function(value,
                            standard_error,
                            margin = 1.96,
                            line_width = 0.5,
                            color = "black") {
  value <- as_symbol(value)
  standard_error <- as_symbol(standard_error)

  return(ggplot2::geom_errorbar(
    ggplot2::aes(
      ymin = !!value - margin * !!standard_error,
      ymax = !!value + margin * !!standard_error
    ),
    width = line_width,
    linewidth = line_width,
    position = ggplot2::position_dodge(0.9),
    colour = color
  ))
}

as_symbol <- function(x) {
  if (is.null(x)) {
    return(NULL)
  }

  return(rlang::ensym(x))
}

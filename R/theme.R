#' @import ggplot2

#' @importFrom ggthemes theme_foundation
#' @importFrom grid unit
#' @importFrom scales manual_pal

#' @include palettes.R

# Adapted from @koundy on GitHub
# https://github.com/koundy/ggplot_theme_Publication

base_theme_publication <- function(font_size = 14, font_family = "sans") {
  return(
    ggthemes::theme_foundation(
      base_size = font_size,
      base_family = font_family
    ) +
    theme(
      plot.title = element_text(
        face = "bold",
        size = rel(1.2),
        hjust = 0.5,
        margin = margin(0, 0, 20, 0)
      ),
      text = element_text(),
      panel.background = element_rect(colour = NA),
      plot.background = element_rect(colour = NA),
      panel.border = element_rect(colour = NA),
      axis.title = element_text(face = "bold", size = rel(1)),
      axis.title.y = element_text(angle = 90, vjust = 2),
      axis.title.x = element_text(vjust = -0.2),
      axis.text = element_text(),
      axis.line.x = element_line(colour = "black"),
      axis.line.y = element_line(colour = "black"),
      axis.ticks = element_line(),
      panel.grid.major = element_line(colour="#efefef"),
      panel.grid.minor = element_blank(),
      legend.key = element_rect(colour = NA),
      legend.position = "bottom",
      legend.direction = "horizontal",
      legend.box = "vetical",
      legend.key.size = grid::unit(10, "pt"),
      legend.margin = margin(t = 15),
      legend.title = element_text(
        face="italic",
        size = font_size,
        margin = margin(r = 15)
      ),
      plot.margin = margin(10, 10, 10, 10),
      strip.background = element_rect(colour = "#cacaca", fill = "#cacaca"),
      strip.text = element_text(face = "bold")
    )
  )
}

scale_fill_publication <- function(fill_colors, ...) {
  return(discrete_scale(
    "fill",
    "publication",
    scales::manual_pal(values = fill_colors),
    ...
  ))
}

scale_colour_publication <- function(fill_colors, ...) {
  return(discrete_scale(
    "colour",
    "publication",
    scales::manual_pal(values = fill_colors),
    ...
  ))
}

base_theme_dark_grey <- function(font_size=14, font_family="sans") {
  return(
    ggthemes::theme_foundation(
      base_size = font_size,
      base_family = font_family
    ) +
    theme(
      plot.title = element_text(
        face = "bold",
        colour = "#ffffb3",
        size = rel(1.2),
        hjust = 0.5,
        margin = margin(0, 0, 20, 0)
      ),
      text = element_text(),
      panel.background = element_rect(colour = NA, fill = "grey20"),
      plot.background = element_rect(colour = NA, fill = "#262626"),
      panel.border = element_rect(colour = NA),
      axis.title = element_text(face = "bold", size = rel(1), colour = "white"),
      axis.title.y = element_text(angle = 90, vjust = 2),
      axis.title.x = element_text(vjust = -0.2),
      axis.text = element_text(colour = "grey70"),
      axis.line.x = element_line(colour = "grey70"),
      axis.line.y = element_line(colour = "grey70"),
      axis.ticks = element_line(colour = "grey70"),
      panel.grid.major = element_line(colour = "#262626"),
      panel.grid.minor = element_blank(),
      legend.background = element_rect(fill = "#262626"),
      legend.text = element_text(color = "white"),
      legend.key = element_rect(colour = NA, fill = "#262626"),
      legend.position = "bottom",
      legend.direction = "horizontal",
      legend.box = "vetical",
      legend.key.size = grid::unit(10, "pt"),
      legend.margin = margin(t = 15),
      legend.title = element_text(
        face = "italic",
        colour = "white",
        size = font_size,
        margin = margin(r = 15)
      ),
      plot.margin = margin(10, 10, 10, 10),
      strip.background = element_rect(colour = "#2D3A4C", fill = "#2D3A4C"),
      strip.text = element_text(face = "bold", colour = "white")
     ))
}

base_theme_transparent <- function(font_size = 14, font_family = "sans") {
  return(
    ggthemes::theme_foundation(
      base_size = font_size,
      base_family = font_family
    ) +
    theme(
      plot.title = element_text(
        face = "bold",
        colour = "#ffffb3",
        size = rel(1.2),
        hjust = 0.5
      ),
      text = element_text(),
      panel.background = element_rect(colour = NA, fill = "transparent"),
      plot.background = element_rect(colour = NA, fill = "transparent"),
      panel.border = element_rect(colour = NA),
      axis.title = element_text(face = "bold", size = rel(1), colour = "white"),
      axis.title.y = element_text(angle = 90, vjust = 2),
      axis.title.x = element_text(vjust = -0.2),
      axis.text = element_text(colour = "grey70"),
      axis.line.x = element_line(colour = "grey70"),
      axis.line.y = element_line(colour = "grey70"),
      axis.ticks = element_line(colour = "grey70"),
      panel.grid.major = element_line(colour = "#262626"),
      panel.grid.minor = element_blank(),
      legend.background = element_rect(fill = "transparent"),
      legend.text = element_text(color = "white"),
      legend.key = element_rect(colour = NA, fill = "grey20"),
      legend.position = "bottom",
      legend.direction = "horizontal",
      legend.box = "vetical",
      legend.key.size = grid::unit(10, "pt"),
      legend.margin = margin(t = 15),
      legend.title = element_text(
        face="italic",
        colour = "white",
        size = font_size,
        margin = margin(r = 15)
      ),
      plot.margin = margin(10, 10, 10, 10),
      strip.background = element_rect(colour = "#2D3A4C", fill = "#2D3A4C"),
      strip.text = element_text(face = "bold", colour = "white")
    )
  )
}

base_theme_dark_blue <- function(font_size=14, font_family="sans") {
  return(
    ggthemes::theme_foundation(
      base_size = font_size,
      base_family = font_family
    ) +
    theme(
      plot.title = element_text(
        face = "bold",
        colour = "#ffffb3",
        size = rel(1.2),
        hjust = 0.5,
        margin = margin(0, 0, 20, 0)
      ),
      text = element_text(),
      panel.background = element_rect(colour = NA, fill = "#282C33"),
      plot.background = element_rect(colour = NA, fill = "#282C33"),
      panel.border = element_rect(colour = NA),
      axis.title = element_text(face = "bold", size = rel(1), colour = "white"),
      axis.title.y = element_text(angle = 90, vjust = 2),
      axis.title.x = element_text(vjust = -0.2),
      axis.text = element_text(colour = "grey70"),
      axis.line.x = element_line(colour = "grey70"),
      axis.line.y = element_line(colour = "grey70"),
      axis.ticks = element_line(colour = "grey70"),
      panel.grid.major = element_line(colour = "#343840"),
      panel.grid.minor = element_blank(),
      legend.background = element_rect(fill = "#282C33"),
      legend.text = element_text(color = "white"),
      legend.key = element_rect(colour = NA, fill = "#282C33"),
      legend.position = "bottom",
      legend.direction = "horizontal",
      legend.box = "vetical",
      legend.key.size = grid::unit(10, "pt"),
      legend.margin = margin(t = 15),
      legend.title = element_text(
        face="italic",
        colour = "white",
        size = font_size,
        margin = margin(r = 15)
      ),
      plot.margin = margin(10, 10, 10, 10),
      strip.background = element_rect(colour = "#2D3A4C", fill = "#2D3A4C"),
      strip.text = element_text(face = "bold", colour = "white")
    ))
}

#' @export
theme_publication <- function(plot,
                              font_size = 14,
                              font_family = "sans",
                              fill_colors = BREWER_COLORS) {
  plot <- plot +
    base_theme_publication(font_size = font_size, font_family = font_family)

  if (!is.null(fill_colors)) {
    plot <- plot +
      scale_fill_publication(fill_colors) +
      scale_colour_publication(fill_colors)
  }

  return(plot)
}

#' @export
theme_transparent <- function(plot,
                              font_size = 14,
                              font_family = "sans",
                              fill_colors = BREWER_COLORS) {
  plot <- plot +
    base_theme_transparent(font_size = font_size, font_family = font_family)

  if (!is.null(fill_colors)) {
    plot <- plot +
      scale_fill_publication(fill_colors) +
      scale_colour_publication(fill_colors)
  }

  return(plot)
}

#' @export
theme_dark_grey <- function(plot,
                            font_size = 14,
                            font_family = "sans",
                            fill_colors = BREWER_COLORS) {
  plot <- plot +
    base_theme_dark_grey(font_size = font_size, font_family = font_family)

  if (!is.null(fill_colors)) {
    plot <- plot +
      scale_fill_publication(fill_colors) +
      scale_colour_publication(fill_colors)
  }

  return(plot)
}

#' @export
theme_dark_blue <- function(plot,
                            font_size = 14,
                            font_family = "sans",
                            fill_colors = BREWER_COLORS) {
  plot <- plot +
    base_theme_dark_blue(font_size = font_size, font_family = font_family)

  if (!is.null(fill_colors)) {
    plot <- plot +
      scale_fill_publication(fill_colors) +
      scale_colour_publication(fill_colors)
  }

  return(plot)
}

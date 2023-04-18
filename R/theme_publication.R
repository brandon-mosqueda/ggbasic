#' @import ggplot2

#' @importFrom ggthemes theme_foundation
#' @importFrom grid unit
#' @importFrom scales manual_pal

#' @include palettes.R

# Adapted from @koundy on GitHub
# https://github.com/koundy/ggplot_theme_Publication

#' @export
theme_publication <- function(plot, base_size = 14, base_family = "sans") {
  plot <- plot +
    base_theme_publication(base_size = base_size, base_family = base_family) +
    scale_fill_publication() +
    scale_colour_publication()

  return(plot)
}

#' @export
base_theme_publication <- function(base_size = 14, base_family = "sans") {
  return(
    ggthemes::theme_foundation(
      base_size = base_size,
      base_family = base_family
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
      panel.grid.major = element_line(colour="#f0f0f0"),
      panel.grid.minor = element_blank(),
      legend.key = element_rect(colour = NA),
      legend.position = "bottom",
      legend.direction = "horizontal",
      legend.box = "vetical",
      legend.key.size= grid::unit(1, "cm"),
      legend.margin = margin(0, 0, 0, 0, "cm"),
      legend.title = element_text(face="italic"),
      plot.margin = margin(10, 5, 5, 5, "mm"),
      strip.background = element_rect(colour = "#cacaca", fill = "#cacaca"),
      strip.text = element_text(face = "bold")
    )
  )
}

#' @export
scale_fill_publication <- function(...) {
  return(discrete_scale(
    "fill",
    "publication",
    scales::manual_pal(values = BREWER_COLORS),
    ...
  ))
}

#' @export
scale_colour_publication <- function(...) {
  return(discrete_scale(
    "colour",
    "publication",
    scales::manual_pal(values = BREWER_COLORS),
    ...
  ))
}

#' @export
theme_dark_grey <- function(base_size=14, base_family="sans") {
  return(
    ggthemes::theme_foundation(
      base_size = base_size,
      base_family = base_family
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
      legend.key.size = grid::unit(0.5, "cm"),
      legend.margin = margin(0, 0, 0, 0, "cm"),
      legend.title = element_text(face = "italic", colour = "white"),
      plot.margin = margin(10, 5, 5, 5, "mm"),
      strip.background = element_rect(colour = "#2D3A4C", fill = "#2D3A4C"),
      strip.text = element_text(face = "bold", colour = "white")
     ))
}

#' @export
scale_fill_publication_dark <- function(...){
  return(discrete_scale(
    "fill",
    "publication",
    scales::manual_pal(values = BREWER_COLORS),
    ...
  ))
}

#' @export
scale_colour_publication_dark <- function(...){
  return(discrete_scale(
    "colour",
    "publication",
    scales::manual_pal(values = BREWER_COLORS),
    ...
  ))
}

#' @export
theme_transparent <- function(base_size=14, base_family="sans") {
  return(
    ggthemes::theme_foundation(
      base_size = base_size,
      base_family = base_family
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
      legend.key.size = grid::unit(0.5, "cm"),
      legend.margin = margin(0, 0, 0, 0, "cm"),
      legend.title = element_text(face = "italic", colour = "white"),
      plot.margin = margin(10, 5, 5, 5, "mm"),
      strip.background = element_rect(colour = "#2D3A4C", fill = "#2D3A4C"),
      strip.text = element_text(face = "bold", colour = "white")
    )
  )
}

#' @export
theme_dark_blue <- function(base_size=14, base_family="sans") {
  return(
    ggthemes::theme_foundation(
      base_size = base_size,
      base_family = base_family
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
      legend.key.size = grid::unit(0.5, "cm"),
      legend.margin = margin(0, 0, 0, 0, "cm"),
      legend.title = element_text(face = "italic", colour = "white"),
      plot.margin = margin(10, 5, 5, 5, "mm"),
      strip.background = element_rect(colour = "#2D3A4C", fill = "#2D3A4C"),
      strip.text = element_text(face = "bold", colour = "white")
    ))
}

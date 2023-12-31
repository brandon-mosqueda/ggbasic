% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/box_plot.R
\name{box_plot}
\alias{box_plot}
\title{Box plot}
\usage{
box_plot(
  data,
  x,
  y = NULL,
  fill_by = NULL,
  facet_row = NULL,
  facet_col = NULL,
  title = NULL,
  x_label = NULL,
  fill_colors = BREWER_COLORS,
  color = "#386cb0",
  outlier_color = "#ef3b2c",
  outlier_size = 5,
  theme = "light",
  font_size = 15,
  alpha = 0.9,
  with_legend = TRUE,
  horizontal = FALSE
)
}
\arguments{
\item{data}{(\code{data.frame}) Data to use for plot.}

\item{x}{(\code{quote} | \code{character}) Name of column to use for x-axis.}

\item{y}{(\code{quote} | \code{character}) Name of column to use for y-axis. \code{NULL} by
default.}

\item{fill_by}{(\code{quote} | \code{character}) Name of column to use to fill the
elements of the plot. If \code{NULL}, the plot will be filled with a single
color. \code{NULL} by default.`}

\item{facet_row}{(\code{quote} | \code{character}) Name of column to use for row
faceting. \code{NULL} by default.}

\item{facet_col}{(\code{quote} | \code{character}) Name of column to use for column
faceting. \code{NULL} by default.}

\item{title}{(\code{character(1)}) Title of the plot. \code{NULL} by default.}

\item{x_label}{(\code{character(1)}) Label for x-axis. \code{NULL} by default which
uses the name provided in \code{x}.}

\item{fill_colors}{(\code{character}) Colors to use for filling the plot. Only
used if \code{fill_by} is not \code{NULL}. \code{BREWER_COLORS} by default.}

\item{color}{(\code{character(1)}) Color to use for the plot. Only used when
\code{fill_by} is \code{NULL}. \verb{#386cb0} by default.}

\item{outlier_color}{(\code{character(1)}) Color to use for outliers. \verb{#ef3b2c}
(red) by default.}

\item{outlier_size}{(\code{numeric(1)}) Size to use for outliers. \code{5} by default.}

\item{theme}{(\code{character(1)}) Theme to use for the plot. The options are:
\code{"light"},  \code{"dark_grey"}, \code{"dark_blue"} and \code{"transparent"}. \code{light} by
default.}

\item{font_size}{(\code{numeric(1)}) Font size to use for the plot. \code{15} by
default.}

\item{alpha}{(\code{numeric(1)}) Alpha value (transparency) to use for elements in
the plot. \code{0.9} by default.}

\item{with_legend}{(\code{logical(1)}) Whether to include a legend in the plot.
Only used when \code{fill_by} is not \code{NULL}. \code{TRUE} by default.}

\item{horizontal}{(\code{logical(1)}) Whether to plot the elements horizontally.
\code{FALSE} by default.}

\item{y_label}{(\code{character(1)}) Label for y-axis. \code{NULL} by default which
uses the name provided in \code{y}.}

\item{x_angle}{(\code{numeric(1)}) Angle to use for x-axis labels. \code{0}
(horizontal) by default.}
}
\value{
A (\code{ggplot2}) plot object.
}
\description{
Elegant and easy to implement box plots.
}
\examples{
# For a single variable
box_plot(iris, x = Sepal.Length)

# Desagregated by a categorical variable
box_plot(iris, x = Sepal.Length, y = Species)

# Easily divide the plot into multiple plots using facet_row and facet_col
titanic <- as.data.frame(Titanic)
box_plot(
  titanic,
  x = Class,
  y = Freq,
  fill_by = Survived,
  facet_row = Sex,
  title = "Titanic survivors by sex and class"
)
}
\seealso{
Other plots: 
\code{\link{bar_plot}()},
\code{\link{hex_plot}()},
\code{\link{hist_plot}()},
\code{\link{line_plot}()},
\code{\link{scatter_plot}()}
}
\concept{plots}

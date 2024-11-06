j#' @param data (`data.frame`) Data to use for plot.
#' @param x (`character`) Name of column to use for x-axis.
#' @param facet_row (`character`) Name of column to use for row
#'   faceting. `NULL` by default.
#' @param facet_col (`character`) Name of column to use for column
#'   faceting. `NULL` by default.
#' @param facet_wrap (`character`) Name of column to use for automatic
#'   faceting. `NULL` by default.
#' @param title (`character(1)`) Title of the plot. `NULL` by default.
#' @param x_label (`character(1)`) Label for x-axis. `NULL` by default which
#'   uses the name provided in `x`.
#' @param y_label (`character(1)`) Label for y-axis. `NULL` by default which
#'   uses the name provided in `y`.
#' @param theme (`character(1)`) Theme to use for the plot. The options are:
#'   `"light"`,  `"dark_grey"`, `"dark_blue"`, `"transparent"` and `"paper"`.
#'   `light` by default.
#' @param font_size (`numeric(1)`) Font size to use for the plot. `15` by
#'  default.
#' @param x_angle (`numeric(1)`) Angle to use for x-axis labels. `0`
#'   (horizontal) by default.
#' @param alpha (`numeric(1)`) Alpha value (transparency) to use for elements in
#'   the plot. `0.9` by default.
#' @param with_legend (`logical(1)`) Whether to include a legend in the plot.
#'  Only used when `fill_by` is not `NULL`. `TRUE` by default.
#' @param horizontal (`logical(1)`) Whether to plot the elements horizontally.
#'  `FALSE` by default.

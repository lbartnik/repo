#' @export
print.rawplot <- function (x, ...) {
  ccat(grey = "<rawplot>\n", silver = "# use the plot() method to re-plot\n")
}

#' @export
plot.rawplot <- function (x, ...) {
  grid::grid.raster(x$png)
}

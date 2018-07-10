#' @export
print.rawplot <- function (x, ...) {
  ccat(grey = "<rawplot>\n", silver = "# use the plot() method to re-plot\n")
}

#' @importFrom png readPNG
#' @importFrom jsonlite base64_dec
#' @export
plot.rawplot <- function (x, ...) {
  raster <- readPNG(as.raw(base64_dec(x$png)), native = TRUE)
  grid::grid.raster(raster)
}

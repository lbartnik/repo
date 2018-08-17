#' Container for plots.
#'
#' @param rp value returned by [grDevices::recordPlot].
#'
#' @rdname rawplot
rawplot <- function (rp) {
  stopifnot(is_recorded_plot(rp))

  structure(list(recordedplot = rp), class = 'rawplot')
}

is_rawplot <- function (x) inherits(x, 'rawplot')



#' @param x `rawplot` object
#' @param ... further arguments passed to or from other methods.
#'
#' @export
#' @rdname rawplot
print.rawplot <- function (x, ...) {
  ccat0(grey = "<rawplot>\n", silver = "# use the plot() method to re-plot\n")
}


#' @importFrom png readPNG
#' @importFrom jsonlite base64_dec
#' @export
#' @rdname rawplot
plot.rawplot <- function (x, ...) {
  raster <- readPNG(as.raw(base64_dec(x$png)), native = TRUE)
  grid::grid.raster(raster)
}


#' Returns a base64-encoded, SVG or PNG plot.
#'
#' @param pl Plot recorded by [grDevices::recordPlot()].
#' @param type Output format: `"png"` or `"svg"`.
#' @param ... Passed to [svg()] or [png()].
#'
#' @return `character` string, base64-encoded SVG or PNG plot.
#' @import jsonlite
#' @importFrom grDevices dev.off png svg replayPlot
#'
#' @rdname replot
#'
replot_as <- function (pl, type, ...) {
  guard()
  if (is.null(pl)) return(NULL)

  stopifnot(is_recorded_plot(pl))
  stopifnot(type %in% c("svg", "png"))

  # TODO use svglite::stringSVG

  path <- tempfile(fileext = paste0('.', type))

  # TODO if `pl` has been recorded without dev.control("enable"), the
  #      plot might be empty; it might be necessary to check for that

  if (identical(type, "svg")) svg(path, ...) else png(path, ...)

  replayPlot(pl)
  dev.off()

  contents <- readBin(path, "raw", n = file.size(path))
  jsonlite::base64_enc(contents)
}


#' @param x `rawplot` object.
#' @param width PNG width.
#' @param height PNG height.
#'
#' @rdname replot
as_png <- function (x, width = 1280, height = 720) {
  stopifnot(is_rawplot(x))
  replot_as(x$recordedplot, 'png', width = width, height = height)
}

#' @rdname replot
as_svg <- function (x) {
  stopifnot(is_rawplot(x))
  replot_as(x$recordedplot, 'svg')
}


#' @param a PNG plot.
#' @param b PNG plot.
#'
#' @rdname rawplot
png_equal <- function (a, b) {
  isTRUE(all.equal(a, b))
}


#' @description `for_store` prepares a `rawplot` for writing to an
#' object store (e.g. see [storage::filesystem]).
#'
#' @rdname rawplot
#'
for_store <- function (x, store) {
  stopifnot(is_rawplot(x))

  if (is.null(x$svg)) x$svg <- as_svg(x)
  if (is.null(x$png)) x$png <- as_png(x)

  x
}

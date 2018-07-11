#' Container for plots.
#'
#' @rdname rawplot
rawplot <- function (rp) {
  stopifnot(is_recorded_plot(rp))

  structure(list(recordedplot = rp), class = 'rawplot')
}

is_rawplot <- function (x) inherits(x, 'rawplot')



#' @export
#' @rdname rawplot
print.rawplot <- function (x, ...) {
  ccat(grey = "<rawplot>\n", silver = "# use the plot() method to re-plot\n")
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
#' @param pl Plot recorded by [recordPlot()].
#' @param type Output format: `"png"` or `"svg"`.
#' @param ... Passed to [svg()] or [png()].
#'
#' @return `character` string, base64-encoded SVG or PNG plot.
#' @import jsonlite
#'
#' @rdname plots
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


as_png <- function (x, width = 1280, height = 720) {
  stopifnot(is_rawplot(x))
  replot_as(x$recordedplot, 'png', width = width, height = height)
}

as_svg <- function (x) {
  stopifnot(is_rawplot(x))
  replot_as(x$recordedplot, 'svg')
}



#' @rdname plots
png_equal <- function (a, b) {
  isTRUE(all.equal(a, b))
}


for_store <- function (x, store) {
  stopifnot(is_rawplot(x))

  if (is.null(x$svg)) x$svg <- as_svg(x)
  if (is.null(x$png)) x$png <- as_png(x)

  x
}


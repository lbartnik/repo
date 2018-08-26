#' Abstract container.
#'
#' Used to store artifacts or commits.
#'
#' @param x object to be tested.
#'
#' @export
is_container <- function (x) inherits(x, 'container')

as_container <- function (x) {
  stopifnot(is.list(x))
  structure(x, class = 'container')
}

#' @export
print.container <- function (x, ...) {
  cat0('<container, ', length(x), ' element(s)>')
  invisible(x)
}

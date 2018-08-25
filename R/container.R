#' @export
is_container <- function (x) inherits(x, 'container')

#' @export
print.container <- function (x, ...) {
  cat0('<container, ', length(x), ' element(s)>')
  invisible(x)
}

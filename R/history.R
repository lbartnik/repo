#' History of R session(s).
#'
#' @param x History object to be printed.
#'
#' @rdname history
#'
print.history <- function (x, ...) {
  cat('<history:commits>\n')
  cat(length(x), 'node(s)')
}

is_history <- function (x) inherits(x, 'history') && storage::is_object_store(attr(x, 'store'))


#' @param .data `history` object.`
#'
#' @rdname history
#' @export
#'
filter.history <- function (.data, ...)
{
  stopifnot(is_graph(.data))

  quo <- rlang::enquos(...)
  stopifnot(identical(length(quo), 1L))

  conditions <- list(
    ancestor_of  = function (y) graph_reduce(.data, to = y),
    branch_tip   = function ()  {
    },
    data_matches = function (..., data) {
    },
    no_parent = function () graph_roots(.data)
  )

  ans <- rlang::eval_tidy(first(quo), conditions)
  preserve_attributes(ans, .data)
}


preserve_attributes <- function (x, from) {
  stopifnot(is_history(from))

  class(x) <- class(from)
  attr(x, 'store') <- attr(from, 'store')

  x
}

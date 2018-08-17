#' History of R session(s).
#'
#' @param x History object to be printed.
#' @param ... Extra arguments.
#'
#' @rdname history
#' @export
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
      Filter(.data, f = function (commit) identical(length(commit$children), 0L))
    },
    data_matches = function (..., data) {
      data <- if (missing(data)) list(...) else c(data, list(...))
      stopifnot(is_all_named(data))

      data <- lapply(data, storage::compute_id)
      Filter(.data, f = function (commit) {
        setequal(names(commit$objects), names(data)) && setequal(unname(commit$objects), unname(data))
      })
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


introduced <- function (commits, id) {
  stopifnot(id %in% names(commits))

  c <- commits[[id]]
  if (is.na(c$parent)) return(names(c$objects))

  p <- commits[[c$parent]]
  new_objs <- Filter(function (n) {
    is.na(match(n, names(p$objects))) || !identical(c$objects[[n]], p$objects[[n]])
  }, names(c$objects))

  # there is a plot (first condition) and it's different from
  # what was there before (second condition)
  if (!is.null(c$plot) && !identical(c$plot, p$plot)) {
    return(c(new_objs, '::plot::'))
  }

  new_objs
}

#' History of R session(s).
#'
#' @rdname history
#' @export
#'
print.history <- function (x) {
  cat('<history:commits>\n')
  cat(length(x), 'node(s)')
}

is_history <- function (x) inherits(x, 'history') && storage::is_object_store(attr(x, 'store'))


#' @rdname history
#' @export
#'
filter <- function (x, ...)
{
  stopifnot(is_graph(x))

  quo <- rlang::enquos(...)
  stopifnot(identical(length(quo), 1L))

  conditions <- list(
    ancestor_of  = function (y) graph_reduce(x, to = y),
    branch_tip   = function ()  {
      Filter(x, f = function (commit) identical(length(commit$children), 0L))
    },
    data_matches = function (..., data) {
      data <- if (missing(data)) list(...) else c(data, list(...))
      stopifnot(all_named(data))

      data <- lapply(data, storage::compute_id)
      Filter(x, f = function (commit) {
        setequal(names(commit$objects), names(data)) && setequal(unname(commit$objects), unname(data))
      })
    },
    no_parent = function () graph_roots(x)
  )

  ans <- rlang::eval_tidy(first(quo), conditions)
  preserve_attributes(ans, x)
}


preserve_attributes <- function (x, from) {
  stopifnot(is_history(from))

  class(x) <- class(from)
  attr(x, 'store') <- attr(from, 'store')

  x
}


introduced <- function (hist, id) {
  stopifnot(is_history(hist))
  stopifnot(id %in% names(hist))

  c <- hist[[id]]
  if (is.na(c$parent)) return(names(c$objects))

  p <- hist[[c$parent]]
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

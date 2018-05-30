#' History of R session(s).
#'
#' @rdname history
#' @export
#'
print.history <- function (x) {
  cat('<history:commits>\n')
  cat(length(x), 'node(s)')
}

is_graph <- function (x) inherits(x, 'graph')

is_history <- function (x) inherits(x, 'history') && storage::is_object_store(attr(x, 'store'))


#' @rdname history
#' @export
#'
filter <- function (x, ...)
{
  stopifnot(is_graph(x))
  cls <- class(x)
  str <- attr(x, 'store')

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
    no_parent = function () {
      Filter(x, f = function (commit) is.na(commit$parent))
    }
  )

  ans <- rlang::eval_tidy(first(quo), conditions)
  structure(ans, class = cls, store = str)
}



graph_reduce <- function (x, from = NULL, to = NULL) {
  stopifnot(is_graph(x))
  cls <- class(x)

  if (!is.null(from)) {
    stopifnot(from %in% names(x))

    extract <- function (id) {
      c(x[id], unlist(lapply(x[[id]]$children, extract), recursive = FALSE))
    }

    x <- extract(from)
  }

  if (!is.null(to)) {
    stopifnot(to %in% names(x))

    extract <- function (id) {
      ans <- x[id]
      parent <- first(ans)$parent
      if (match(parent, names(x), nomatch = FALSE) && !is.na(parent)) {
        ans <- c(extract(first(ans)$parent), ans)
      }
      ans
    }

    x <- extract(to)
  }

  class(x) <- cls
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

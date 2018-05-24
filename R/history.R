#' History of R session(s).
#'
#' @rdname history
#' @export
#'
print.history <- function (x) {
  cat('<history:commits>\n')
  cat(length(x$data), 'node(s)')
}

is_graph <- function (x) inherits(x, 'graph')

is_history <- function (x) inherits(x, 'history')


#' @rdname history
#' @export
#'
filter <- function (x, ...)
{
  stopifnot(is_graph(x))
  # ancestors_of
  # branch_ends
  # data_matches

  quo <- rlang::enquos(...)
  stopifnot(identical(length(quo), 1L))

  conditions <- list(
    ancestor_of  = function (y) graph_reduce(x, to = y),
    branch_tip   = function ()  {
      Filter(x, f = function (commit) identical(length(commit$children), 0L))
    },
    data_matches = function (...) {
      data <- list(...)
      stopifnot(all_named(data))

      data <- lapply(data, storage::compute_id)
      Filter(x, f = function (commit) {
        setequal(names(commit$objects), names(data)) && setequal(unname(commit$objects), unname(data))
      })
    }
  )

  rlang::eval_tidy(first(quo), conditions)
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
      if (!is.na(first(ans)$parent)) {
        ans <- c(extract(first(ans)$parent), ans)
      }
      ans
    }

    x <- extract(to)
  }

  class(x) <- cls
  x
}




step_over <- function (x, f, ...) UseMethod("step_over")

step_over.default <- function (x, f, ...) stop("cannot iterate over class ", class(x))

step_over.commits <- function (x, f, ...) {

  nodes <- napply(x$nodes, function(id, node) {
    node$id <- id
    node$parent <- NA_character_
    node$children <- c()
    node
  })

  lapply(x$edges, function (edge) {
    nodes[[edge$target]]$parent <<- edge$source
    nodes[[edge$source]]$children <<- append(nodes[[edge$source]]$children, edge$target)
  })

  ans <- vector()

  traverse <- function (id) {
    a <- f(nodes[[id]], ...)
    ans$push_back(a)
    lapply(nodes[[id]]$children, traverse)

    nodes[[id]] <<- NULL
  }

  roots <- names(Filter(function (node) is.na(node$parent), nodes))
  lapply(roots, traverse)

  if (length(nodes)) {
    stop("the commit graph is malformed")
  }

  invisible()
}

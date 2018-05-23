#' @export
print.history <- function (x) {
  cat('<history:commits>\n')
  cat(length(x), 'node(s)')
}

is_graph <- function (x) inherits(x, 'graph')

is_history <- function (x) inherits(x, 'history')


#' Show ancestors.
#'
#' @export
#' @rdname history
#'
history_ancestors <- function (x, id) {
  stopifnot(is_history(x))
  stopifnot(id %in% names(x$data))

  graph_reduce(x, to = id)
}


#' @export
#' @rdname history
#'
history_leaves <- function (x) {
  stopifnot(is_history(x))

  names(Filter(function (commit) { length(commit$children) == 0 }, x$data))
}


#' @export
#' @rdname history
#'
history_match <- function (x, m) {
  stopifnot(is_history(x))
  stopifnot(all_named(m))

  m <- lapply(m, storage::compute_id)
  d <- Filter(x$data, f = function (commit) {
    setequal(names(commit$objects), names(m)) && setequal(unname(commit$objects), unname(m))
  })

  names(d)
}


#' @export
#' @rdname history
#'
history_data <- function (x, id) {
  stopifnot(is_history(x))
  stopifnot(id %in% names(x$data))

  ct <- x$data[[id]]
  lapply(ct$objects, function (id) storage::os_read_object(x$repo$store, id))
}


graph_reduce <- function (x, from = NULL, to = NULL) {
  stopifnot(is_graph(x))
  nodes <- x$data

  if (!is.null(from)) {
    extract <- function (id) {
      c(nodes[id],
        unlist(lapply(nodes[[id]]$children, extract), recursive = FALSE))
    }
    nodes <- extract(from)
  }

  if (!is.null(to)) {
    stopifnot(to %in% names(nodes))

    extract <- function (id) {
      ans <- nodes[id]
      if (!is.na(first(ans)$parent)) {
        ans <- c(extract(first(ans)$parent), ans)
      }
      ans
    }

    nodes <- extract(to)
  }

  x$data <- nodes
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

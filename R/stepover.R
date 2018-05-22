#' @export
print.commits <- function (x) {
  cat('<commits>\n')
  cat(length(x$nodes), 'node(s) and', length(x$edges), 'edge(s)\n')
}

is_commits <- function (x) inherits(x, 'commits')


is_graph <- function (x) inherits(x, 'graph')


graph_reduce <- function (x, from = NULL, to = NULL) {
  stopifnot(is_graph(x))

  nodes <- napply(x$nodes, function(id, node) {
    list(
      id       = id,
      parent   = NA_character_,
      children = c(),
      data     = node
    )
  })

  lapply(x$edges, function (edge) {
    nodes[[edge$target]]$parent <<- edge$source
    nodes[[edge$source]]$children <<- append(nodes[[edge$source]]$children, edge$target)
  })

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
        ans <- c(ans, extract(first(ans)$parent))
      }
      ans
    }

    nodes <- extract(to)
  }

  edges <- vector()
  nodes <- lapply(nodes, function (node) {
    if (!is.na(node$parent)) {
      edges$push_back(list(source = node$parent, target = node$id))
    }
    node$data
  })

  structure(list(nodes = nodes, edges = edges$data()),
            class = class(x))
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

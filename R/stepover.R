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

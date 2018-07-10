is_graph <- function (x) inherits(x, 'graph')


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


graph_roots <- function (x) {
  stopifnot(is_graph(x))
  Filter(x, f = function (node) is.na(node$parent))
}


graph_stratify <- function (x) {
  stopifnot(is_graph(x))

  # TODO if a parent has more than one child, displaying the tree might
  #      tricky: the branch that the parent is assigned to needs to be
  #      displayed first if the sequence of commands is to produce the
  #      final object; but even then, if names collide, the actual parent
  #      might be overwritten before the child is created based on it

  process_node <- function (id) {
    nodes$erase(id)
    node <- x[[id]]
    node$children <- lapply(node$children, process_node)
    node$parent <- NULL
    node
  }

  nodes <- vector(data = names(x))
  stopifnot(nodes$size() != 0)

  # 1. find roots
  roots <- lapply(names(graph_roots(x)), process_node)
  stopifnot(length(roots) != 0)

  # 2. iterate over roots, descend over children
  lapply(roots, function (id) {
    abort("not implemented yet")
  })

  # 4. if there is more than one top-level root, create an "abstract" root
  if (length(roots) == 1) {
    roots <- first(roots)
  }

  structure(roots, class = c('stratified', class(x)))
}

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

  process_node <- function (id) {
    nodes$erase(id)
    node <- x[[id]]
    node$children <- lapply(node$children, process_node)
    node$parent <- NULL
    node
  }

  nodes <- vector(data = names(x))
  roots <- lapply(names(graph_roots(x)), process_node)
  stopifnot(nodes$size() == 0)

  if (length(roots) == 1) {
    roots <- first(roots)
  }

  # TODO
  # 1. find roots
  # 2. iterate over roots, descend over children
  # 3. if a child has more than one parent, issue a warning, choose the first parent
  #    and assign the child under that parent

  structure(roots, class = c('stratified', class(x)))
}

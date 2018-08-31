#' Turn a container of artifacts into a graph of artifacts.
#'
#' `connect_artifacts` can be called on the output of [read_artifacts]
#' to ensure that all artifacts become connected and can be turned into
#' a tree by calling [stratify]. It performs three operations:
#'
#'   * given the full graph of artifacts, it connects artifacts that
#'     are on a path in the graph (tree) but the intermediate nodes
#'     (artifacts) are not present in the container; this produces
#'     a __connected graph__
#'   * adds a `children` key to each element of the container
#'   * removes references to parents that cannot be reassigned to
#'     artifacts in the containers because no artifact until the very
#'     root of the full tree is present in the container
#'
#' @param artifacts `container` of artifacts; see [read_artifacts]
#' @return The input container, `artifacts`, whose elements are altered
#' as described above.
#'
#' @importFrom rlang quos
#' @import utilities
#'
#' @export
#' @rdname graph
connect_artifacts <- function (artifacts) {
  stopifnot(is_container(artifacts))
  # make sure all artifacts come from the same store
  stopifnot(length(unique(lapply(artifacts, artifact_store))) == 1)

  # find identifiers of all artifacts
  store <- artifact_store(first(artifacts))
  all_ids <- storage::os_find(store, quos(isTRUE(artifact)))

  chosen_ids <- map_chr(artifacts, `[[`, 'id')
  graph <- ancestry_graph(chosen_ids, all_ids, store)

  # assign newly computed paretns and children
  artifacts <- lapply(artifacts, function (a) {
    node <- graph[[a$id]]
    a$parents <- node$parents
    a$children <- node$children
    a
  })

  as_graph(as_container(artifacts))
}


#' @param x container returned by `connect_artifacts`.
#' @export
#' @rdname graph
stratify <- function (x) {
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
    node
  }

  names(x) <- map_chr(x, `[[`, 'id')
  nodes <- new_vector(data = names(x))
  stopifnot(nodes$size() != 0)

  # iterate over roots, descend over children
  roots <- lapply(names(find_roots(x)), process_node)
  stopifnot(length(roots) != 0)
  stopifnot(nodes$size() == 0)

  # if there is more than one top-level root, create an "abstract" root
  if (length(roots) > 1) {
    roots <- list(
      class = c('abstract_root', class(roots)),
      children = roots
    )
  }
  else {
    roots <- first(roots)
  }

  roots
}


find_roots <- function (x) {
  Filter(x, f = function (node) is_empty(node$parents))
}

as_graph <- function (x) structure(x, class = c('graph', class(x)))

is_graph <- function (x) inherits(x, 'graph')

# --- old code ---------------------------------------------------------

is_graph <- function (x) inherits(x, 'graph')


#' @import utilities
graph_reduce <- function (x, from = NULL, to = NULL) {
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

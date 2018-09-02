as_graph <- function (x) structure(x, class = c('graph', class(x)))

is_graph <- function (x) inherits(x, 'graph')



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



# TODO merge with graph_reduce and connect_artifacts

#' Build a connected ancestry graph.
#'
#' For each identifier in `ids`, `ancestry_graph` creates and returns a
#' `list` with the two following keys:
#'
#'   * `parents` which is read directly from `store`
#'   * `children` which is inferred from `parents`
#'
#' Those lists are wrapped in a single list and named with values from `ids`.
#' Furthermore, `ancestry_graph` verifies that all values present in `parents`
#' and `children` are also present in `ids` and that the resulting graph is
#' connected, that is, whether there is a path between any pair of nodes.
#'
#' @param chosen_ids identifiers, possibly disconnected
#' @param all_ids all identifiers, superset of `chosen_ids`
#' @param store object store; see [storage::object_store]
#'
#' @return A `list` named according to `ids`; each element is a list with
#' two keys: `parents` and `children`.
#'
ancestry_graph <- function (chosen_ids, all_ids, store) {

  # read parent pointers written in storage as tags
  parents <- map(all_ids, function (id) {
    tags <- storage::os_read_tags(store, id)
    # artifact stores multiple parents
    if (!is.null(tags$parents)) parents <- as.character(tags$parents)
    # commit stores a single parent
    if (!is.null(tags$parent)) parents <- as.character(tags$parent)
    if (is.null(parents)) parents <- character()
    if (length(parents) && is.na(parents)) parents <- character()
    parents
  })

  children <- map(all_ids, function(...)character())
  imap(parents, function (parents_for_id, id) {
    for (parent in parents_for_id) {
      children[[parent]] <<- c(children[[parent]], id)
    }
  })

  # if a given artifact is not in the input list, reassign its id among
  # its children's parents with that artifact's parents; in doing so, it
  # "shrinks" the graph but keeps the lineage information
  for (id in all_ids) {
    # if among chosen artifacts, skip
    if (id %in% chosen_ids) next

    # otherwise delete
    for (child in children[[id]]) {
      childs_parents <- parents[[child]]
      childs_parents <- setdiff(childs_parents, id)
      childs_parents <- c(childs_parents, parents[[id]])
      parents[[child]] <- childs_parents
    }

    for (parent in parents[[id]]) {
      children[[parent]] <- setdiff(children[[parent]], id)
    }

    parents[[id]] <- NULL
    children[[id]] <- NULL
  }

  nodes <- lapply(chosen_ids, function (id) {
    list(
      parents  = parents[[id]],
      children = children[[id]]
    )
  })
  names(nodes) <- chosen_ids

  as_graph(nodes)
}


#' Traverse a graph.
#'
#' Traverse the whole graph or its subset. `graph` is a graph structure
#' as returned by [connect_artifacts]; it is a named `list` of `list`s,
#' each of which contains two keys: `children` and `parents`, which are
#' `character` vectors with node identifiers matching `names(graph)`.
#' The traverse starts in each identifier passed in `start` (there can
#' by multiple starting nodes). It uses `neighbours` to pick nodes to go
#' to from any given node; this function accepts two arguments: `id`, a
#' node identifier and `graph` which is the original graph structure.
#'
#' @param graph graph structure, as returned by [connect_artifacts].
#' @param start a `vector` of `character` node identifiers.
#' @param neighbours a `function` which accepts a node identifier
#'        and the `graph` structure and returns a `vector` of node
#'        identifiers.
#' @return a `vector` of node identifiers visited on the traverse.
#'
#' @seealso ancestry_graph, adjust_ancestry
#'
#' @examples
#' \dontrun{
#'    g <- ancestry_graph(...)
#'    # descend in a tree which start in the first node in the graph
#'    traverse(g, names(g)[[1]], function(id, graph) graph[[id]]$children)
#' }
traverse <- function (graph, start, neighbours) {
  stopifnot(is_graph(graph))
  stopifnot(is.function(neighbours))
  stopifnot(all(start %in% names(graph)))

  # BFS
  black <- new_set()
  grey  <- new_set(data = start)

  while (grey$size() > 0) {
    id <- grey$pop_front()
    black$insert(id)

    new_ids <- neighbours(id, graph)
    if (length(new_ids) > 0) {
      stopifnot(all(new_ids %in% names(graph)))
    }

    lapply(new_ids, function (new_id) {
      if (!black$contains(new_id)) {
        grey$insert(new_id)
        grey$remove(id)
      }
    })
  }

  unlist(black$data())
}


#' Fix ancestry information.
#'
#' After defining a graph subset by a traverse, it might be desirable
#' to have the `children` and `parents` keys of each graph node contain
#' only identifiers of nodes present in the graph.
#'
#' @param graph a graph as returned by [traverse].
#' @return Input `graph` with `children` and `parents` adjusted to
#' contain only identifiers of nodes present in `graph`.
#'
#' @seealso ancestry_graph, traverse
adjust_ancestry <- function (graph) {
  stopifnot(is_graph(graph))

  nodes <- lapply(graph, function (node) {
    node$children <- intersect(node$children, names(graph))
    node$parents <- intersect(node$parents, names(graph))
    node
  })

  class(nodes) <- class(graph)
  nodes
}


commit_graph <- function (store) {
  # TODO replace with is_commit()
  ids <- os_find(store, list(quo('commit' %in% class)))
  ancestry_graph(ids, ids, store)
}

artifact_graph <- function (store) {
  # TODO replace with is_artifact()
  ids <- os_find(store, list(quo(artifact)))
  ancestry_graph(ids, ids, store)
}


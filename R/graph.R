is_graph <- function (x) inherits(x, 'graph')


#' @import utilities
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
  Filter(x, f = function (node) is_empty(node$parents))
}


graph_stratify <- function (x) {
  stopifnot(is_graph(x))
  stopifnot(length(x) > 0)
  stopif(is_stratified(x))

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

  nodes <- new_vector(data = names(x))
  stopifnot(nodes$size() != 0)

  # iterate over roots, descend over children
  roots <- lapply(names(graph_roots(x)), process_node)
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

  structure(roots, class = c('stratified', class(first(x))))
}

is_stratified <- function (x) inherits(x, 'stratified')

#' Turn a list of artifacts into a graph of artifacts.
#'
#' A list of artifacts, whose `names()` are artifact identifiers, is
#' transformed into a graph by adding a `children` key each element of
#' the list, removing `parents` that do exist in the list and finally
#' re-assigning `parents` if a certain parent artifact is not in the
#' input list.
#'
#' @param artifacts `list` of artifacts, currently retrieved only in
#'        `repository_explain`.
#' @param store The store artifacts were read from.
#'
#' @importFrom rlang quos
#' @import utilities
#'
#' @rdname graph
#'
graph_of_artifacts <- function (artifacts, store) {
  ids <- storage::os_find(store, quos(isTRUE(artifact)))

  parents <- map(ids, function (id) {
    tags <- storage::os_read_tags(store, id)
    if (!is.null(tags$parents)) return(as.character(tags$parents))
    character()
  })

  children <- map(ids, function(...)character())
  imap(parents, function (parents_for_id, id) {
    for (parent in parents_for_id) {
      children[[parent]] <<- c(children[[parent]], id)
    }
  })

  # if a given artifact is not in the input list, reassign its id amongs
  # its children's parents with that artifact's parents; "shrink" the graph
  # but keep the lineage information
  for (id in ids) {
    # if among chosen artifacts, skip
    if (id %in% names(artifacts)) next

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

  # assign newly computed paretns and children
  for (id in names(parents)) {
    artifacts[[id]]$parents <- parents[[id]]
    artifacts[[id]]$children <- children[[id]]
  }

  structure(artifacts, class = 'graph')
}



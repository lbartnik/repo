# TODO move to selection.R

#' Selectors for history queries.
#'
#' @param x identifier of a commit or an artifact.
#'
#' @rdname history
#' @export
ancestor_of <- function(x) {
  abort("This function should not be called directly")
}

#' @rdname history
#' @export
descendant_of <- function (x) {
  abort("This function should not be called directly")
}

#' @rdname history
#' @export
no_parent <- function() {
  abort("This function should not be called directly")
}

#' @rdname history
#' @export
no_children <- function() {
  abort("This function should not be called directly")
}

#' @description `data_matches` searches for a commit whose artifacts
#' match objects passed via `...` and `data`.
#'
#' @param ... named objects to look for.
#' @param data (Optional). `...` as a single `list`.
#'
#' @importFrom rlang abort
#' @rdname history
#' @export
data_matches <- function(..., data) {
  abort("This function should not be called directly")
}



#' @importFrom rlang eval_tidy
extract_id <- function (quo) {
  eval_tidy(quo, data = list(ancestor_of = function(x)x, descendant_of = function(x)x))
}

ancestor_of_impl <- function (root, graph) {
  stopifnot(is_graph(graph))
  traverse(graph, root, function(node_id, graph) nth(graph, node_id)$parents)
}

descendant_of_impl <- function (root, graph) {
  stopifnot(is_graph(graph))
  ans <- traverse(graph, root, function(node_id, graph) nth(graph, node_id)$children)
  setdiff(ans, root)
}

no_children_impl <- function (graph) {
  stopifnot(is_graph(graph))
  filtered <- Filter(graph, f = function (node) identical(length(node$children), 0L))
  names(filtered)
}


no_parents_impl <- function (graph) {
  stopifnot(is_graph(graph))
  filtered <- Filter(graph, f = function (node) identical(length(node$parents), 0L))
  names(filtered)
}


extract_data_match <- function (quo) {
  impl <- list(data_matches = function(..., data) {
    data <- if (missing(data)) list(...) else c(list(...), data)
    stopifnot(is_all_named(data))
    lapply(data, storage::compute_id)
  })

  eval_tidy(quo, data = impl)
}

#' @importFrom rlang quo
data_matches_impl <- function (data, store) {
  ids <- os_find(store, list(quo('commit' %in% class)))

  Filter(ids, f = function (id) {
    commit <- new_commit(as_id(id), store)
    setequal(names(commit$objects), names(data)) && setequal(unname(commit$objects), unname(data))
  })
}










#' Tag names.
#'
#' Tag names need to be declared as objects to silence `R CMD check`.
#'
#' @rdname tagnames
#' @export
time <- NULL

#' @rdname tagnames
#' @export
object <- NULL

#' @rdname tagnames
#' @export
#plot <- NULL

#' @rdname tagnames
#' @export
id <- NULL

#' @rdname tagnames
#' @export
artifact <- NULL

#' @rdname tagnames
#' @export
parent_commit <- NULL

#' @rdname tagnames
#' @export
parents <- NULL

#' @rdname tagnames
#' @export
session <- NULL


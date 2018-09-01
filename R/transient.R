# TODO move to selection.R

#' Selectors for history queries.
#'
#' @importFrom rlang abort
#' @rdname history
#' @export
ancestor_of <- function(x) {
  abort("This function should not be called directly")
}

#' @importFrom rlang abort
#' @rdname history
#' @export
no_parent <- function() {
  abort("This function should not be called directly")
}

#' @importFrom rlang abort
#' @rdname history
#' @export
branch_tip <- function() {
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
extract_ancestor_id <- function (quo) {
  eval_tidy(quo, data = list(ancestor_of = function(x)x))
}

ancestor_of_impl <- function (root, graph) {
  stopifnot(is_graph(graph))
  traverse(graph, root, function(node_id, graph) nth(graph, node_id)$parents)
}

no_children_impl <- function (graph) {
  stopifnot(is_graph(graph))
  filtered <- Filter(graph, f = function (node) identical(length(node$children), 0L))
  names(filtered)
}


no_parents_impl <- function (query) {

}

data_matches_impl <- function (query) {
  data <- if (missing(data)) list(...) else c(data, list(...))
  stopifnot(is_all_named(data))

  data <- lapply(data, storage::compute_id)
  Filter(.data, f = function (commit) {
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


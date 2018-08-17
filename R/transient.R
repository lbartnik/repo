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
plot <- NULL

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


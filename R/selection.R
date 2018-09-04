#' @export
#' @importFrom dplyr filter
dplyr::filter

#' @export
#' @importFrom dplyr arrange
dplyr::arrange

#' @export
#' @importFrom dplyr summarise
dplyr::summarise

#' @export
#' @importFrom dplyr n
dplyr::n

#' @export
#' @importFrom magrittr %>%
magrittr::`%>%`


#' Retrieve top `n` elements.
#'
#' `dplyr`'s [dplyr::top_n] is a function, here we define an S3 method
#' whose default implementation calls the function from `dplyr`.
#'
#' @param .data A `query`, `repository` or a class supported by `dplyr`.
#' @param n The number of elements to retrieve.
#' @param wt (Optional). The variable to use for ordering. Supported only
#'        in `dplyr`.
#'
#' @export
#'
top_n <- function (.data, n, wt) UseMethod("top_n")

#' @importFrom dplyr top_n
#' @export
top_n.default <- function (.data, n, wt) dplyr::top_n(.data, n, wt)



#' Extra query API.
#'
#' @description `tag_names` returns all tag names occuring among objects
#' selected in the query `x`.
#'
#' @param x query object.
#'
#' @export
#' @rdname query-extra
tag_names <- function (x) {
  q <- as_query(x)
  ids <- match_ids(q)
  ans <- read_tag_names(ids, q$repository$store)
  # TODO return all tag names
  setdiff(ans, 'artifact')
}


#' @description `tag_values` returns a `list` of vectors of values for
#' all tags occuring among objects selected in the query `x`.
#'
#' @export
#' @rdname query-extra
tag_values <- function (x) {
  q <- as_query(x)
  ids <- match_ids(q)
  names <- read_tag_names(ids, q$repository$store)
  values <- read_tag_values(ids, names, q$repository$store)
  lapply(values, unique)
}

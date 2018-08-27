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
  ans <- all_tag_names(as_query(x))
  setdiff(ans, 'artifact')
}


#' @description `tag_values` returns a `list` of vectors of values for
#' all tags occuring among objects selected in the query `x`.
#'
#' @export
#' @rdname query-extra
tag_values <- function (x) {
  ans <- all_tag_values(as_query(x))
  nms <- setdiff(names(ans), 'artifact')
  ans[nms]
}


# --- impl -------------------------------------------------------------

# A stop-gap function: check if the only summary is n() and if so, returns TRUE.
# If there is no summary at all, returns FALSE.
# If there's an unsupported summary, throws an exception.
#' @importFrom rlang abort quo_expr
only_n_summary <- function (qry) {
  if (!length(qry$summarise)) return(FALSE)
  if (!is_all_named(qry$summarise)) abort("all summaries expressions need to be named")

  i <- map_lgl(qry$summarise, function (s) {
    expr <- quo_expr(s)
    is.call(expr) && identical(expr, quote(n()))
  })

  all(i)
}


#' @importFrom rlang warn
flatten_lists <- function (values) {

  # simplify columns which hold single, atomic values
  values <- imap(values, function (column, name) {
    # check which values are NULL
    inl <- map_lgl(column, is.null)

    # if all, drop the column
    if (all(inl)) {
      warn(sprintf("tag %s rendered no values, removing from result", name))
      return(NULL)
    }

    # otherwise, replace NULLs with NAs
    if (any(inl)) {
      cls <- first(typeof(unlist(column[!inl])))
      nav <- switch(cls, double = NA_real_, integer = NA_integer_, character = NA_character_,
                    complex = NA_complex_, NA)
      column[inl] <- nav
    }

    # if there are multi-valued elements, return a list
    len <- map_int(column, length)
    if (any(len != 1)) return(column)

    # if there is a class, as long as it's the same (esp. POSIXct), apply that here
    cls <- unique(map(column, class))
    if (length(cls) == 1 && !is_atomic_class(first(cls))) {
      column <- unlist(column, recursive = FALSE)
      return(structure(column, class = first(cls)))
    }

    # if all are atomic, return a vector and let R choose the type
    if (all(map_lgl(column, is.atomic))) return(unlist(column))

    # finally return a list
    column
  })

  values <- Filter(function(x)!is.null(x), values)
  tibble::as_tibble(values)
}

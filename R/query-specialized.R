#' @description `as_commits` creates a `query` to search for commits.
#' @export
#' @rdname query
as_commits <- function (x) {
  stopifnot(is_repository(x))
  filter(set_type(as_query(x), 'commits'),
         'commit' %in% class)
}


#' @description `as_artifacts` creates a `query` to search for artifacts.
#' @export
#' @rdname query
as_artifacts <- function (x) {
  stopifnot(is_repository(x))
  filter(set_type(as_query(x), 'artifacts'),
         artifact)
}


#' @description `as_tags` creates a `query` to search for tag values.
#' @export
#' @rdname query
as_tags <- function (x) {
  stopifnot(is_repository(x))
  filter(set_type(as_query(x), 'tags'),
         artifact)
}


is_commits <- function (x) is_query(x) && identical(x$type, 'commits')

is_artifacts <- function (x) is_query(x) && identical(x$type, 'artifacts')

is_tags <- function (x) is_query(x) && identical(x$type, 'tags')

#' @export
#' @rdname query
read_artifacts <- function (.data) {
  stopifnot(is_artifacts(.data))
  stopifnot(identical(length(.data$select), 0L))

  store <- .data$repository$store
  ans <- lapply(match_ids(.data), function (id) {
    new_artifact(id, store)
  })

  structure(ans, class = 'container')
}


#' @export
#' @rdname query
read_commits <- function (.data) {
  stopifnot(is_commits(.data))
  stopifnot(identical(length(.data$select), 0L))

  abort("read_commits not yet implemented")

  structure(list(), class = 'container')
}


#' @importFrom rlang quos is_symbol is_character
#' @importFrom tidyselect vars_select
#' @importFrom dplyr bind_cols
#' @importFrom tibble tibble
read_tags <- function (.data, ...) {
  stopifnot(is_tags(.data))
  selection <- quos(...)

  ids <- match_ids(.data)
  if (!length(ids)) {
    abort("query does not match any objects")
  }

  # if nothing is specified, choose everything
  if (!length(selection)) {
    names <- c("id", read_tag_names(ids, .data$repository$store))
  }
  else {
    # if only symbols or characters, that's it
    exprs <- map(selection, quo_expr)
    is_name <- map_lgl(exprs, function(x) is_symbol(x) || is_character(x))
    if (all(is_name)) {
      names <- as.character(exprs)
    } else {
      # is not only names, try tidyselect
      names <- c("id", read_tag_names(ids, .data$repository$store))
      names <- tryCatch(vars_select(names, UQS(selection)), error = function(e)e)

      if (is_error(names)) {
        abort(sprintf("could not select names: %s", names$message))
      }
      if (!length(names)) {
        abort("selection reduced to an empty set")
      }
    }
  }

  if (!length(names)) {
    abort("no tag names to selected")
  }

  # handle id
  if (!match("id", names, nomatch = FALSE)) {
    return(flatten_lists(read_tag_values(ids, names, .data$repository$store)))
  }

  names <- setdiff(names, "id")
  if (!length(names)) return(tibble(id = ids))

  bind_cols(tibble(id = ids),
            flatten_lists(read_tag_values(ids, names, .data$repository$store)))
}

#' @importFrom rlang caller_env eval_tidy quo quo_get_env warn UQS
#' @importFrom tibble tibble
match_ids <- function (query) {
  stopifnot(is_query(query))
  store <- query$repository$store

  # identify which filter expressions include symbol `id`
  with_id <- quos_match(query$filter, 'id')

  # if expressions
  if (!any(with_id)) {
    matching_ids <- os_find(store, query$filter)
  } else {
    # retrieve ids matching the query
    matching_ids <- os_find(store, query$filter[!with_id])
    matching_ids <- dplyr::filter_(tibble(id = matching_ids),
                                   .dots = query$filter[with_id])
    matching_ids <- nth(matching_ids, 'id')
  }

  # TODO if arrange is malformed, maybe intercept the exception and provide
  #      a custom error message to the user?
  if (length(query$arrange)) {
    names  <- read_tag_names(matching_ids, store)
    names  <- Filter(function (name) quos_match(query$arrange, name), names)
    values <- read_tag_values(matching_ids, names, store)
    values <- flatten_lists(c(list(id = matching_ids), values))
    values <- dplyr::arrange_(values, .dots = query$arrange)
    matching_ids <- nth(values, 'id')
  }

  if (!is.null(query$top)) {
    matching_ids <- head(matching_ids, query$top)
  }

  matching_ids
}


#' @importFrom rlang quos quo_expr
quos_match <- function (quos, ...) {
  symbols <- lapply(list(...), function (e) {
    if (is.symbol(e)) return(e)
    if (is.character(e)) return(as.symbol(e))
    stop('cannot process ', as.character(e))
  })

  map_lgl(quos, function (quo) {
    any(map_lgl(symbols, function (sym) expr_match(quo_expr(quo), sym)))
  })
}

expr_match <- function (expr, sym) {
  recurse <- function (x) any(unlist(lapply(x, function (e) expr_match(e, sym))))
  if (!is.recursive(expr)) return(identical(expr, sym))
  if (is.call(expr)) return(recurse(expr[-1]))
  stop('cannot process ', deparse(expr))
}

#' @importFrom rlang quo
read_tag_names <- function (ids, store) {
  all_names <- lapply(ids, function (id) names(storage::os_read_tags(store, id)))
  unique(unlist(all_names))
}

#' @importFrom rlang quo
read_tag_values <- function (ids, selected, store) {
  columns <- map(selected, function(x) base::vector("list", length(ids)))

  # read id by id, set tag values in respective column vector
  Map(ids, seq_along(ids), f = function (id, i) {
    tags <- storage::os_read_tags(store, id)
    imap(tags[selected], function (value, name) {
      columns[[name]][[i]] <<- value
    })
  })

  columns
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

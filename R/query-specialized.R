#' @description `as_commits` creates a `query` to search for commits.
#' @export
#' @rdname query
as_commits <- function (x) {
  if (!is_query(x)) x <- as_query(x)
  filter(set_type(x, 'commits'), 'commit' %in% class)
}

#' @description `as_artifacts` creates a `query` to search for artifacts.
#' @export
#' @rdname query
as_artifacts <- function (x) {
  if (!is_query(x)) x <- as_query(x)
  filter(set_type(x, 'artifacts'), artifact)
}

#' @description `as_tags` creates a `query` to search for tag values.
#' @export
#' @rdname query
as_tags <- function (x) {
  if (!is_query(x)) x <- as_query(x)
  filter(set_type(x, 'tags'), artifact)
}


is_commits <- function (x) is_query(x) && identical(x$type, 'commits')

is_artifacts <- function (x) is_query(x) && identical(x$type, 'artifacts')

is_tags <- function (x) is_query(x) && identical(x$type, 'tags')

#' @export
#' @rdname query
read_artifacts <- function (.data) {
  stopifnot(is_artifacts(.data))
  stopifnot(identical(length(.data$select), 0L))

  store <- .data$store

  # TODO make it so that each new condition doesn't have to be added below explicitly
  # all special call-like conditions need to be processed here
  ans <- lapply(.data$filter, function (quo) {
    # find ancestors
    if (expr_match_fun(quo_squash(quo), quote(ancestor_of))) {
      id <- match_short(extract_id(quo), store)
      return(ancestor_of_impl(id, artifact_graph(store)))
    }
    # find descendants
    if (expr_match_fun(quo_squash(quo), quote(descendant_of))) {
      id <- match_short(extract_id(quo), store)
      return(descendant_of_impl(id, artifact_graph(store)))
    }
  })

  .data$filter[!map_lgl(ans, is.null)] <- NULL

  ans <- c(ans, list(match_ids(.data)))
  ans <- Reduce(ans, NULL, f = function (a, b) {
    if (is.null(a)) return(b)
    if (is.null(b)) return(a)
    intersect(a, b)
  })

  ans <- lapply(ans, function (id) {
    new_artifact(id, store)
  })

  structure(ans, class = 'container')
}


#' @importFrom rlang quos is_symbol is_character
#' @importFrom tidyselect vars_select
#' @importFrom dplyr bind_cols
#' @importFrom tibble tibble
#' @export
#' @rdname query
read_tags <- function (.data, ...) {
  stopifnot(is_tags(.data))
  selection <- quos(...)

  ids <- match_ids(.data)
  if (!length(ids)) {
    abort("query does not match any objects")
  }

  # if nothing is specified, choose everything
  if (!length(selection)) {
    names <- c("id", read_tag_names(ids, .data$store))
  }
  else {
    # if only symbols or characters, that's it
    exprs <- map(selection, quo_squash)
    is_name <- map_lgl(exprs, function(x) is_symbol(x) || is_character(x))
    if (all(is_name)) {
      names <- as.character(exprs)
    } else {
      # is not only names, try tidyselect
      names <- c("id", read_tag_names(ids, .data$store))
      names <- tryCatch(vars_select(names, UQS(selection)), error = function(e)e)

      if (is_error(names)) {
        abort(glue("could not select names: {names$message}"))
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
    return(flatten_lists(read_tag_values(ids, names, .data$store)))
  }

  names <- setdiff(names, "id")
  if (!length(names)) return(tibble(id = ids))

  bind_cols(tibble(id = ids),
            flatten_lists(read_tag_values(ids, names, .data$store)))
}

#' @importFrom rlang caller_env eval_tidy quo quo_get_env warn UQS
#' @importFrom tibble tibble
match_ids <- function (query) {
  stopifnot(is_query(query))
  store <- query$store

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


#' @importFrom rlang quos quo_squash
quos_match <- function (quos, ...) {
  symbols <- lapply(list(...), function (e) {
    if (is.symbol(e)) return(e)
    if (is.character(e)) return(as.symbol(e))
    stop('cannot process ', as.character(e))
  })

  map_lgl(quos, function (quo) {
    any(map_lgl(symbols, function (sym) expr_match_sym(quo_squash(quo), sym)))
  })
}

expr_match <- function (expr, sym, mode) {
  stopifnot(mode %in% c('no-call', 'call', 'fun-only'))

  if (!is.recursive(expr)) {
    if (identical(mode, 'fun-only')) return(FALSE)
    return(identical(expr, sym))
  }

  recurse <- function (x) any(unlist(lapply(x, function (e) expr_match(e, sym, mode))))
  if (is.call(expr)) {
    if (identical(mode, 'fun-only') && identical(expr[[1]], sym)) return(TRUE)
    return(recurse(expr[-1]))
  }

  abort('cannot process: ', deparse(expr))
}

expr_match_sym <- function (expr, sym) expr_match(expr, sym, 'no-call')

expr_match_fun <- function (expr, sym) expr_match(expr, sym, 'fun-only')


#' @importFrom rlang quo
read_tag_names <- function (ids, store) {
  all_names <- lapply(ids, function (id) names(storage::os_read_tags(store, as_id(id))))
  unique(unlist(all_names))
}

#' @importFrom rlang quo
read_tag_values <- function (ids, selected, store) {
  columns <- map(selected, function(x) base::vector("list", length(ids)))

  # read id by id, set tag values in respective column vector
  Map(ids, seq_along(ids), f = function (id, i) {
    tags <- storage::os_read_tags(store, as_id(id))
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


#' @export
#' @rdname query
read_commits <- function (.data) {
  stopifnot(is_commits(.data))
  store <- .data$store

  # TODO move this to match_ids
  ans <- lapply(.data$filter, function (quo) {

    if (expr_match_fun(quo_squash(quo), quote(ancestor_of))) {
      id <- match_short(extract_id(quo), store)
      return(ancestor_of_impl(id, commit_graph(store)))
    }
    if (expr_match_fun(quo, quote(no_children))) {
      return(no_children_impl(commit_graph(store)))
    }
    if (expr_match_fun(quo, quote(no_parents))) {
      return(no_parents_impl(commit_graph(store)))
    }
    if (expr_match_fun(quo, quote(data_matches))) {
      match <- extract_data_match(quo)
      return(data_matches_impl(match, store))
    }
    return(NULL)
  })

  # remove complex filters
  .data$filter[!map_lgl(ans, is.na)] <- NULL
  ans <- c(ans, list(match_ids(.data)))

  ids <- Reduce(ans, NULL, f = function(a, b) {
    if (is.null(a)) return(b)
    if (is.null(b)) return(b)
    intersect(a, b)
  })

  commits <- lapply(ids, function (id) {
    new_commit(as_id(id), store)
  })

  structure(commits, class = 'container')
}

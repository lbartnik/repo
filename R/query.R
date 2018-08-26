#' Internal API for query objects.
#'
#' @param x `repository` object or an object to be turned into a `query`.
#' @rdname query-internal
new_query <- function (x) {
  stopifnot(is_repository(x))
  structure(list(repository = x,
                 filter = list(),
                 arrange = list(),
                 select = NULL,
                 top_n = NULL,
                 summarise = list(),
                 type = 'raw'),
            class = 'query')
}


#' @param type make `x` be of type `type`.
#' @rdname query-internal
set_type <- function (x, type) {
  x$type <- type
  x
}


# TODO a general query man page mentioning all categories of functions + specific pages (query, read, etc.)

#' Query the repository of artifacts.
#'
#' @param x Object to be tested (`is_query()`, `is_artifacts()`, `is_commits()`),
#'        printed, cast as `query` (`as_query()`, `as_artifacts()`,
#'        `as_commits()`) or querried (verbs).
#'
#' @rdname query
#' @name query
NULL


#' @description `as_query` creates a general `query`.
#' @export
#' @rdname query
as_query <- function (x) {
  if (is_query(x)) {
    return(x)
  }
  if (is_repository(x)) {
    return(new_query(x))
  }

  stop("cannot coerce class ", first(class(x)), " to query")
}


#' @description `as_commits` creates a `query` to search for commits.
#' @export
#' @rdname query
as_commits <- function (x) {
  stopifnot(is_repository(x))
  filter(set_type(as_query(x), 'commits'),
         'commit' %in% class)
}


#' @description `as_commits` creates a `query` to search for commits.
#' @export
#' @rdname query
as_artifacts <- function (x) {
  stopifnot(is_repository(x))
  filter(set_type(as_query(x), 'artifacts'),
         artifact)
}


is_raw <- function (x) is_query(x) && identical(x$type, 'raw')

is_commits <- function (x) is_query(x) && identical(x$type, 'commits')

is_artifacts <- function (x) is_query(x) && identical(x$type, 'artifacts')


#' @return `TRUE` if `x` inherits from `"query"`.
#'
#' @export
#' @rdname query
is_query <- function (x) inherits(x, 'query')


#' @param ... further arguments passed to or from other methods.
#'
#' @importFrom rlang expr_deparse get_expr
#' @export
#' @rdname query
print.query <- function (x, ...) {

  quos_text <- function (x) {
    join(map_chr(x, function (f) expr_deparse(get_expr(f))), ', ')
  }

  # describe the source repo
  lines <- new_vector()
  lines$push_back(toString(x$repository))

  # print the full query
  for (part in c('select', 'filter', 'arrange', 'top_n', 'summarise')) {
    if (length(x[[part]])) {
      lines$push_back(paste0(part, '(', quos_text(x[[part]]), ')'))
    }
  }

  cat0('  ', join(lines$data(), ' %>%\n    '), '\n')

  invisible(x)
}

#' @param .data `query` object.
#'
#' @name query
#' @rdname query
NULL


#' @importFrom rlang quos
#' @export
#' @rdname query
filter.query <- function (.data, ...) {
  dots <- quos(...)
  .data$filter <- c(.data$filter, dots)
  .data
}


#' @importFrom rlang abort quos
#' @importFrom tidyselect vars_select
#' @export
#' @rdname query
select.query <- function (.data, ...) {
  sel <- quos(...)

  # TODO store & print the expressions and perform tidyselect only when
  #      ready to read the data; select() will be disallowed in read_objects
  #      read_ids, read_artifacts and read_commits

  # TODO only if query type is tags select() will narrow down; in every
  #      other case it will replace the current select with a warning

  if (!length(.data$select)) {
    names <- all_select_names(.data)
  }
  else {
    names <- .data$select
  }

  if (!length(names)) {
    abort("select: no tag names to select from, filter matches no objects?")
  }

  names <- tryCatch(vars_select(names, UQS(sel), .exclude = "artifact"), error = function(e)e)
  if (is_error(names)) {
    abort(sprintf("select: could not select names: %s", names$message))
  }
  if (!length(names)) {
    abort("select: selection reduced to an empty set")
  }

  .data$select <- names
  .data
}


#' @description `unselect` clears the list of selected tag names.
#'
#' @export
#' @rdname query
unselect <- function (.data) {
  stopifnot(is_query(.data))
  .data$select <- list()
  .data
}


#' @importFrom rlang quos quo
#' @export
#' @rdname query
arrange.query <- function (.data, ...) {
  dots <- quos(...)
  .data$arrange <- c(.data$arrange, dots)
  .data
}


#' @inheritParams top_n
#'
#' @importFrom rlang quos quo abort
#' @export
#' @rdname query
top_n.query <- function (.data, n, wt) {
  if (!missing(wt)) {
    abort("wt not yet supported in top_n")
  }
  if (missing(n) || !is.numeric(n) || isFALSE(n > 0)) {
    abort("n has to be a non-negative number")
  }

  .data$top_n <- n
  .data
}


#' @export
#' @rdname query
summarise.query <- function (.data, ...) {
  if (length(.data$summarise)) {
    warn("overwriting the query summary")
  }

  .data$summarise <- quos(...)
  .data
}


#' @export
#' @rdname query
read_artifacts <- function (.data) {
  stopifnot(is_artifacts(.data))
  stopifnot(identical(length(.data$select), 0L))

  store <- .data$repository$store
  ans <- lapply(select_ids(.data), function (id) {
    new_artifact(id, store)
  })

  structure(ans, class = 'container')
}


#' @export
#' @rdname query
read_commits <- function (.data) {
  stopifnot(is_commits(.data))
  stopifnot(identical(length(.data$select), 0L))

  structure(list(), class = 'container')
}


#' @description `execute` runs the query and retrieves its results.
#'
#' @importFrom rlang UQS warn
#' @export
#' @rdname query
execute <- function (.data) {

  .warn <- FALSE # TODO revisit warnings

  stopifnot(is_query(.data))
  if (!length(.data$select)) {
    warn("selection is empty, returning an empty set")
    return(tibble::tibble())
  }

  # TODO summarise is mutually exclusive with top_n and arrange

  store <- .data$repository$store

  # 1. find artifacts that match the filter
  ids <- select_ids(.data)

  # 1a. if there's a simple counting summary, this is where we can actually
  #     return the result
  if (only_n_summary(.data)) {
    ans <- tibble::tibble(length(ids))
    return(with_names(ans, names(.data$summarise)))
  }

  if (!length(ids)) {
    if (isTRUE(.warn)) warn("filter did not match any objects, returning an empty set")
    return(tibble::tibble())
  }

  # 2. decide what to read from the object store
  sel <- if (length(.data$select)) .data$select else all_select_names(.data)

  values <- read_tags(setdiff(sel, c("id", "object")), ids, store)

  # object is the actual original data, be it an R object or a plot
  if ("object" %in% sel) {
    values <- c(values, list(object = lapply(ids, function (id) storage::os_read_object(store, id))))
  }

  # id is not present among tags
  if ("id" %in% sel) {
    values <- c(values, list(id = ids))
  }

  # simplify list-based tags into a tibble
  values <- flatten_lists(values)

  # 3. summarise goes before arrange and top_n and if defined is the last step
  if (length(.data$summarise)) {
    return(dplyr::summarise(values, UQS(.data$summarise)))
  }

  # 4. arrange
  # TODO if arrange is malformed, maybe intercept the exception and provide
  #      a custom error message to the user?
  if (length(.data$arrange)) {
    values <- dplyr::arrange_(values, .dots = .data$arrange)
  }

  # 5. top_n
  if (!is.null(.data$top)) {
    values <- head(values, .data$top)
  }

  values
}


#' @importFrom rlang abort caller_env expr_text eval_tidy quos quo_get_expr
#' @rdname query
update <- function (.data, ...) {
  stopifnot(is_query(.data))
  stopif(length(.data$select), length(.data$summarise), length(.data$arrange), length(.data$top_n))

  quos <- quos(...)
  e <- caller_env()

  ids <- select_ids(.data)
  lapply(ids, function (id) {
    tags <- storage::os_read_tags(.data$repository$store, id)

    newt <- unlist(lapply(seq_along(quos), function (i) {
      n <- nth(names(quos), i)
      q <- nth(quos, i)

      if (nchar(n)) {
        return(with_names(list(eval_tidy(q, tags, e)), n))
      }

      update_tag_values(quo_get_expr(q), tags)
    }), recursive = FALSE)

    storage::os_update_tags(.data$repository$store, id, combine(newt, tags))
  })
}



# --- internal ---------------------------------------------------------


#' @importFrom rlang quo
update_tag_values <- function (expr, tags) {
  what <- nth(expr, 1)
  stopifnot(identical(what, quote(append)) || identical(what, quote(remove)))

  where <- as.character(nth(expr, 2))
  if (!has_name(tags, where)) tags[[where]] <- character()

  e <- new.env(parent = emptyenv())
  e$append <- function (where, what) union(where, what)
  e$remove <- function (where, what) setdiff(where, what)

  with_names(list(eval_tidy(expr, tags, e)), where)
}

all_select_names <- function(qry) {
  regular_names <- all_tag_names(qry)
  if (is.null(regular_names)) return(character())

  c(regular_names, "id", "object")
}

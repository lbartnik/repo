#' Internal API for query objects.
#'
#' @param x `repository` object or an object to be turned into a `query`.
#' @rdname query-internal
new_query <- function (x) {
  stopifnot(is_repository(x))
  structure(list(repository = x,
                 filter = list(),
                 arrange = list(),
                 top_n = NULL,
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

#' @return `TRUE` if `x` inherits from `"query"`.
#'
#' @export
#' @rdname query
is_query <- function (x) inherits(x, 'query')


is_raw <- function (x) is_query(x) && identical(x$type, 'raw')


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
#' @importFrom tibble tibble
summarise.query <- function (.data, ...) {
  expr <- quos(...)

  if (!length(expr)) {
    abort("empty summary not supported")
  }

  if (!is_all_named(expr)) {
    abort("all summary expressions need to be named")
  }

  if (!only_n_summary(expr)) {
    abort("only the n() summary is supported")
  }

  n <- length(match_ids(.data))
  with_names(tibble(n), names(expr))
}

# A stop-gap function: check if the only summary is n() and if so, returns TRUE.
# If there is no summary at all, returns FALSE.
# If there's an unsupported summary, throws an exception.
#' @importFrom rlang abort quo_expr
only_n_summary <- function (expr) {
  if (!length(expr)) return(FALSE)

  i <- map_lgl(expr, function (s) {
    e <- quo_expr(s)
    is.call(e) && identical(e, quote(n()))
  })

  all(i)
}




# --- old code ---------------------------------------------------------


#' @description `execute` runs the query and retrieves its results.
#'
#' @importFrom rlang UQS warn
#' @rdname query
deprecated_execute <- function (.data) {

  .warn <- FALSE # TODO revisit warnings

  stopifnot(is_query(.data))
  if (!length(.data$select)) {
    warn("selection is empty, returning an empty set")
    return(tibble::tibble())
  }

  # TODO summarise is mutually exclusive with top_n and arrange

  store <- .data$repository$store

  # 1. find artifacts that match the filter
  ids <- match_ids(.data)

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

  ids <- match_ids(.data)
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

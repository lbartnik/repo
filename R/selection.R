#' @export
#' @importFrom dplyr filter
dplyr::filter

#' @export
#' @importFrom dplyr arrange
dplyr::arrange

#' @export
#' @importFrom dplyr select
dplyr::select

#' @export
#' @importFrom dplyr summarise
dplyr::summarise

#' @export
#' @importFrom magrittr %>%
magrittr::`%>%`


#' @export
top_n <- function (x, n, wt) UseMethod("top_n")

#' @importFrom dplyr top_n
#' @export
top_n.default <- function (x, n, wt) dplyr::top_n(x, n, wt)



#' @export
filter.repository <- function (repo, ...) {
  filter(as_query(repo), ...)
}

#' @export
arrange.repository <- function (repo, ...) {
  arrange(as_query(repo), ...)
}

#' @export
select.repository <- function (repo, ...) {
  select(as_query(repo), ...)
}

#' @export
summarise.repository <- function (repo, ...) {
  summarise(as_query(repo), ...)
}

#' @export
top_n.repository <- function (repo, n, wt) {
  top_n(as_query(repo), n, wt)
}


as_query <- function (x) {
  if (is_query(x)) {
    return(x)
  }
  if (is_repository(x)) {
    return(query(x))
  }

  stop("cannot coerce class ", first(class(x)), " to query")
}

query <- function (x) {
  stopifnot(is_repository(x))
  structure(list(repository = x, filter = list(), arrange = list(), select = NULL,
                 top_n = NULL, summarise = list()),
            class = 'query')
}

#' @export
is_query <- function (x) inherits(x, 'query')

#' @importFrom rlang expr_deparse get_expr
#'
quos_text <- function (x) {
  map_chr(x, function (f) expr_deparse(get_expr(f)))
}


#' @export
print.query <- function (x, ...) {
  lines <- vector(toString(x$repository))

  for (part in c('select', 'filter', 'arrange', 'top_n', 'summarise')) {
    if (length(x[[part]])) {
      lines$push_back(paste0(part, '(', join(quos_text(x[[part]]), ', '), ')'))
    }
  }

  cat0('  ', join(lines$data(), ' %>%\n    '), '\n')
}


#' @export
tag_names <- function (x) {
  # TODO take selection and filters into consideration
  ans <- all_tag_names(as_query(x))
  setdiff(ans, 'artifact')
}


#' @export
tag_values <- function (x) {
  # TODO take selection and filters into consideration
  ans <- all_tag_values(as_query(x))
  nms <- setdiff(names(ans), 'artifact')
  ans[nms]
}


# --- impl -------------------------------------------------------------

#' @importFrom rlang quos
#' @export
filter.query <- function (qry, ...) {
  dots <- quos(...)
  qry$filter <- c(qry$filter, dots)
  qry
}

#' @importFrom rlang abort quos
#' @importFrom tidyselect vars_select
#' @export
#'
select.query <- function (qry, ...) {
  sel <- quos(...)

  if (!length(qry$select)) {
    names <- all_select_names(qry)
  }
  else {
    names <- qry$select
  }

  names <- tryCatch(vars_select(names, UQS(sel), .exclude = "artifact"), error = function(e)e)
  if (is_error(names)) {
    abort(sprintf("could not select names: %s", names$message))
  }
  if (!length(names)) {
    abort("selection reduced to an empty set")
  }

  qry$select <- names
  qry
}

#' @export
unselect <- function (qry) {
  stopifnot(is_query(qry))
  qry$select <- list()
  qry
}

all_select_names <- function(qry) c(all_tag_names(qry), "id", "object")


#' @importFrom rlang quos quo
#' @export
arrange.query <- function (qry, ...) {
  dots <- quos(...)
  qry$arrange <- c(qry$arrange, dots)
  qry
}


#' @importFrom rlang quos quo abort
#' @export
top_n.query <- function (qry, n, wt) {
  if (!missing(wt)) {
    abort("wt not yet supported in top_n")
  }
  if (missing(n) || !is.numeric(n) || isFALSE(n > 0)) {
    abort("n has to be a non-negative number")
  }

  qry$top_n <- n
  qry
}


#' @export
summarise.query <- function (qry, ...) {
  if (length(qry$summarise)) {
    warn("overwriting the query summary")
  }

  qry$summarise <- quos(...)
  qry
}


#' @importFrom rlang UQS warn
#' @export
#'
execute <- function (x, .warn = TRUE) {
  stopifnot(is_query(x))
  if (!length(x$select)) {
    if (isTRUE(.warn)) warn("selection is empty, returning an empty set")
    return(tibble::tibble())
  }

  # TODO summarise is mutually exclusive with top_n and arrange

  store <- x$repository$store

  # 1. find artifacts that match the filter
  ids <- select_ids(x)

  # 1a. if there's a simple counting summary, this is where we can actually
  #     return the result
  if (only_n_summary(x)) {
    ans <- tibble::tibble(length(ids))
    return(with_names(ans, names(x$summarise)))
  }

  if (!length(ids)) {
    if (isTRUE(.warn)) warn("filter did not match any objects, returning an empty set")
    return(tibble::tibble())
  }

  # 2. decide what to read from the object store
  sel <- if (length(x$select)) x$select else all_select_names()

  # we will append to this one
  values <- list()

  # object is the actual original data, be it an R object or a plot
  if ("object" %in% sel) {
    values <- c(values, list(object = lapply(ids, function (id) storage::os_read_object(store, id))))
    sel <- setdiff(sel, "object")
  }

  # id is not present among tags
  if ("id" %in% sel) {
    values <- c(values, list(id = ids))
    sel <- setdiff(sel, "id")
  }

  values2 <- read_tags(sel, ids, store)

  # simplify list-based tags into a tibble
  values <- dplyr::bind_cols(tibble::as_tibble(values), flatten_lists(values2))

  # 3. summarise goes before arrange and top_n and if defined is the last step
  if (length(x$summarise)) {
    return(dplyr::summarise(values, UQS(x$summarise)))
  }

  # 4. arrange
  # TODO if arrange is malformed, maybe intercept the exception and provide
  #      a custom error message to the user?
  if (length(x$arrange)) {
    values <- dplyr::arrange_(values, .dots = x$arrange)
  }

  # 5. top_n
  if (!is.null(x$top)) {
    values <- head(values, x$top)
  }

  values
}


#' @importFrom rlang abort caller_env expr_text eval_tidy quos quo_get_expr
update <- function (x, ...) {
  stopifnot(is_query(x))
  stopif(length(x$select), length(x$summarise), length(x$arrange), length(x$top_n))

  quos <- quos(...)

  ids <- select_ids(x)
  lapply(ids, function (id) {
    tags <- storage::os_read_tags(x$repository$store, id)

    newt <- unlist(lapply(seq_along(quos), function (i) {
      n <- nth(names(quos), i)
      q <- nth(quos, i)

      if (nchar(n)) {
        return(with_names(list(eval_tidy(q, tags, e)), n))
      }

      process_update(quo_get_expr(q), tags)
    }), recursive = FALSE)

    storage::os_update_tags(x$repository$store, id, combine(newt, tags))
  })
}


#' @importFrom rlang quo
process_update <- function (expr, tags) {
  what <- nth(expr, 1)
  stopifnot(identical(what, quote(append)) || identical(what, quote(remove)))

  where <- as.character(nth(expr, 2))
  if (!has_name(tags, where)) tags[[where]] <- character()

  e <- new.env(parent = emptyenv())
  e$append <- function (where, what) union(where, what)
  e$remove <- function (where, what) setdiff(where, what)

  with_names(list(eval_tidy(expr, tags, e)), where)
}


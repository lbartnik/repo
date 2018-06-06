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
#' @importFrom magrittr %>%
magrittr::`%>%`


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


as_query <- function (x) {
  if (is_repository(x)) {
    return(query(x))
  }

  stop("cannot coerce class ", first(class(x)), " to query")
}

query <- function (x) {
  stopifnot(is_repository(x))
  structure(list(repository = x, filter = list(), arrange = list(), select = NULL),
            class = 'query')
}

is_query <- function (x) inherits(x, 'query')

#' @importFrom rlang expr_deparse get_expr
#'
quos_text <- function (x) {
  map_chr(x, function (f) expr_deparse(get_expr(f)))
}


#' @export
print.query <- function (x, ...) {
  lines <- vector(toString(x$repository))

  for (part in c('select', 'filter', 'arrange')) {
    if (length(x[[part]])) {
      lines$push_back(paste0(part, '(', join(quos_text(x[[part]]), ', '), ')'))
    }
  }

  cat0('  ', join(lines$data(), ' %>%\n    '), '\n')
}


# --- impl -------------------------------------------------------------

#' @importFrom rlang quos
#' @export
filter.query <- function (qry, ...) {
  dots <- quos(...)
  qry$filter <- c(qry$filter, dots)
  qry
}

#' @importFrom rlang quos
#' @export
select.query <- function (qry, ...) {
  sel <- quos(...)

  if (is.null(qry$select)) {
    qry$select <- sel
    return(qry)
  }

  i <- (quos_text(qry$select) %in% quos_text(sel))
  if (!any(i)) {
    stop("selection reduced to an empty set", call. = FALSE)
  }

  qry$select <- qry$select[i]
  qry
}

#' @importFrom rlang quos quo
#' @export
arrange.query <- function (qry, ...) {
  dots <- quos(...)
  qry$arrange <- c(qry$arrange, dots)
  qry
}

#' @export
execute <- function (x) {
  stopifnot(is_query(x))
  if (!length(x$select)) {
    warning("selection is empty, returning an empty set", call. = FALSE)
    return(data.frame())
  }

  store <- x$repository$store

  # 1. find artifacts that match the filter
  ids <- storage::os_find(store, c(quo(artifact), x$filter))

  # 2. decide what to read from the object store
  sel <- quos_text(x$select)
  values <- list()

  if ("object" %in% sel) {
    values <- c(values, list(object = lapply(ids, function (id) storage::os_read_object(store, id))))
    sel <- setdiff(sel, "object")
  }

  if ("id" %in% sel) {
    values <- c(values, list(id = ids))
    sel <- setdiff(sel, "id")
  }

  values2 <- map_lst(sel, function(x) base::vector("list", length(sel)))

  Map(ids, seq_along(ids), f = function (id, i) {
    tags <- storage::os_read_tags(store, id)
    tags <- with_names(tags[sel], sel)
    napply(tags, function (name, value) {
      values2[[name]][[i]] <<- value
    })
  })

  values2 <- lapply(values2, function (column) {
    type <- unique(map_chr(column, class))
    if (length(type) == 1 && is.atomic(first(column))) as(column, type) else column
  })

  values <- tibble::as_tibble(c(values, values2))

  if (!length(x$arrange)) {
    return(values)
  }

  # 3. arrange
  # TODO if arrange is malformed, maybe intercept the exception and provide
  #      a custom error message to the user?
  dplyr::arrange_(values, .dots = x$arrange)
}

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
    return(tibble::tibble())
  }

  store <- x$repository$store

  # 1. find artifacts that match the filter
  ids <- storage::os_find(store, c(quo(artifact), x$filter))

  # 2. decide what to read from the object store
  available_tags <- c(all_tag_names(x), "id", "object")
  sel <- tidyselect::vars_select(available_tags, !!!x$select, .exclude = "artifact")

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

  # everything else can be read from tags
  values2 <- map_lst(sel, function(x) base::vector("list", length(ids)))

  Map(ids, seq_along(ids), f = function (id, i) {
    tags <- storage::os_read_tags(store, id)
    tags <- with_names(tags[sel], sel)
    napply(tags, function (name, value) {
      values2[[name]][[i]] <<- value
    })
  })

  # simplify columns which hold single, atomic values
  values2 <- lapply(values2, function (column) {
    len <- map_int(column, length)
    if (any(len != 1)) return(column)
    cls <- unique(map_lst(column, class))
    if (length(cls) > 1) return(column)
    cls <- first(cls)
    ref <- first(column)
    if (is.atomic(ref)) `class<-`(as.vector(column, typeof(ref)), cls) else column
  })

  # make sure there is at least one value in each column
  i <- (map_dbl(values2, length) < 1)
  if (any(i)) {
    empty <- names(values2)[i]
    warning("tags ", join(empty, ', '), " rendered no values, removing from result",
            call. = FALSE)
    values2 <- values2[setdiff(names(values2), empty)]
  }

  values <- tibble::as_tibble(c(values, values2))

  if (!length(x$arrange)) {
    return(values)
  }

  # 3. arrange
  # TODO if arrange is malformed, maybe intercept the exception and provide
  #      a custom error message to the user?
  dplyr::arrange_(values, .dots = x$arrange)
}

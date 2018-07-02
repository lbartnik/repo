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

  names <- vars_select(names, UQS(sel), .exclude = "artifact")
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

  # everything else can be read from tags
  values2 <- map_lst(sel, function(x) base::vector("list", length(ids)))

  Map(ids, seq_along(ids), f = function (id, i) {
    tags <- storage::os_read_tags(store, id)
    tags <- with_names(tags[sel], sel)
    imap(tags, function (value, name) {
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


#' @importFrom rlang quos
write <- function (x, ...) {
  stopifnot(is_query(x))
  stopifnot(!length(x$select))
  stopifnot(!length(x$summarise))
  stopifnot(!length(x$arrange))
  stopifnot(!length(x$top_n))

  quo <- quos(...)

  res <- x %>% select(id) %>% execute
  lapply(res$id, function (id) {

  })
}



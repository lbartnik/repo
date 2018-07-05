#' @importFrom rlang quo
all_tag_names <- function (query) {
  stopifnot(is_query(query))
  store <- query$repository$store

  ids <- storage::os_find(store, list(quo(artifact)))
  nms <- lapply(ids, function (id) names(storage::os_read_tags(store, id)))
  unique(unlist(nms))
}

#' @importFrom rlang quo
all_tag_values <- function (query) {
  stopifnot(is_query(query))
  store <- query$repository$store

  ids <- storage::os_find(store, list(quo(artifact)))
  raw <- lapply(ids, function (id) storage::os_read_tags(store, id))

  nms <- unique(unlist(lapply(raw, names)))

  vls <- lapply(nms, function (name) {
    val <- lapply(raw, `[[`, name)
    unq <- unique(unlist(val))
    if (is.atomic(first(val))) `class<-`(unq, class(first(val))) else unq
  })

  with_names(vls, nms)
}

# A stop-gap function: check if the only summary is n() and if so, returns TRUE.
# If there is no summary at all, returns FALSE.
# If there's an unsupported summary, throws an exception.
#' @importFrom rlang abort quo_expr
only_n_summary <- function (qry) {
  if (!length(qry$summarise)) return(FALSE)
  if (!all_named(qry$summarise)) abort("all summaries expressions need to be named")

  i <- map_lgl(qry$summarise, function (s) {
    expr <- quo_expr(s)
    is.call(expr) && identical(expr, quote(n()))
  })

  all(i)
}


#' @importFrom rlang caller_env eval_tidy quo quo_get_env
select_ids <- function (qry) {
  stopifnot(is_query(qry))

  s <- qry$repository$store

  if (!any(quos_match(qry$filter, id))) {
    return(storage::os_find(s, c(quo(artifact), qry$filter)))
  }

  if (length(qry$filter) > 1) {
    abort("if `id` is used in filter, it must be the only expression")
  }

  flt <- first(qry$filter)
  expr <- quo_expr(flt)

  if (is.call(expr) && identical(first(expr), quote(`==`))) {
    i <- if (identical(second(expr), quote(id))) 3 else 2
    return(eval_tidy(expr[[i]], env = quo_get_env(flt)))
  }

  if (is.call(expr) && identical(first(expr), quote(`%in%`))) {
    if (!identical(second(expr), quote(id))) abort('cannot process ', deparse(expr))
    return(eval_tidy(expr[[3]]))
  }

  ids <- storage::os_find(s, list(quo(artifact)))
  i <- eval_tidy(flt, list(id = ids))
  ids[i]
}



#' @importFrom rlang quos quo_expr
quos_match <- function (quos, ...) {
  symbols <- lapply(quos(...), function (q) {
    e <- quo_expr(q)
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


read_tags <- function (tag_names, ids, store) {
  columns <- map_lst(tag_names, function(x) base::vector("list", length(ids)))

  # read all tags' values for given ids
  Map(ids, seq_along(ids), f = function (id, i) {
    tags <- storage::os_read_tags(store, id)
    imap(tags[tag_names], function (value, name) {
      columns[[name]][[i]] <<- value
    })
  })

  columns
}


flatten_lists <- function (values) {

  # simplify columns which hold single, atomic values
  values <- lapply(values, function (column) {
    len <- map_int(column, length)
    if (any(len != 1)) return(column)
    cls <- unique(map_lst(column, class))
    if (length(cls) > 1) return(column)
    cls <- first(cls)
    ref <- first(column)
    if (is.atomic(ref)) `class<-`(as.vector(column, typeof(ref)), cls) else column
  })

  # make sure there is at least one value in each column
  i <- (map_dbl(values, length) < 1)
  if (any(i)) {
    empty <- names(values)[i]
    warning("tags ", join(empty, ', '), " rendered no values, removing from result",
            call. = FALSE)
    values <- values2[setdiff(names(values), empty)]
  }

  tibble::as_tibble(values)
}


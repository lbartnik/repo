#' @importFrom rlang quo
all_tag_names <- function (query) {
  stopifnot(is_query(query))
  store <- query$repository$store

  ids <- select_ids(query)
  if (!length(ids)) return(NULL)

  nms <- lapply(ids, function (id) names(storage::os_read_tags(store, id)))
  unique(unlist(nms))
}

#' @importFrom rlang quo
all_tag_values <- function (query) {
  stopifnot(is_query(query))
  store <- query$repository$store

  ids <- select_ids(query)
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
    cls <- unique(map_lst(column, class))
    if (length(cls) == 1 && !is_atomic_class(first(cls))) {
      column <- unlist(column, recursive = FALSE)
      return(structure(column, class = first(cls)))
    }

    # if all are atomic, return a vector and let R choose the type
    if (all(map_lgl(column, is.atomic))) return(unlist(column))

    # finally return a list
    column
  })

  values <- Filter(not(is.null), values)
  tibble::as_tibble(values)
}


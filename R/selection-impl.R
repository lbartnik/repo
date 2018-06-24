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

#' @importFrom rlang quo
all_tag_names <- function (store) {
  ids <- storage::os_find(store, list(quo(artifact)))
  nms <- lapply(ids, function (id) names(storage::os_read_tags(store, id)))
  unique(unlist(nms))
}

#' @importFrom rlang quo
all_tag_values <- function (store) {
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

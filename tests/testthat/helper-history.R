sample_graph <- function () {
  structure(
    list(
      a = list(parent = NA_character_, children = c("b", "c"), objects = list()),
      b = list(parent = "a", children = c("d", "e"), objects = list()),
      c = list(parent = "a", children = c("f", "g"), objects = list()),
      d = list(parent = "b", children = c(), objects = list()),
      e = list(parent = "b", children = c(), objects = list()),
      f = list(parent = "c", children = c(), objects = list()),
      g = list(parent = "c", children = c(), objects = list(x = storage::compute_id(1)))
    ),
    class = c('history', 'graph')
  )
}

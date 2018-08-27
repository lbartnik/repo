sample_graph <- function () {
  structure(
    list(
      a = list(id = 'a', parents = NA_character_, children = c("b", "c"), objects = list(x = 'a')),
      b = list(id = 'b', parents = "a", children = c("d", "e"), objects = list(x = 'a')),
      c = list(id = 'c', parents = "a", children = c("f", "g"), objects = list(x = 'a')),
      d = list(id = 'd', parents = "b", children = c(), objects = list(x = 'a', y = 'b')),
      e = list(id = 'e', parents = "b", children = c(), objects = list()),
      f = list(id = 'f', parents = "c", children = c(), objects = list()),
      g = list(id = 'g', parents = "c", children = c(), objects = list(x = storage::compute_id(1)))
    ),
    class = c('graph', 'container')
  )
}

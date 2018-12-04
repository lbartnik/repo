sample_graph <- function () {
  structure(
    list(
      a = list(id = 'a', parents = c(), children = c("b", "c"), objects = list(x = 'a'), time = 1),
      b = list(id = 'b', parents = "a", children = c("d", "e"), objects = list(x = 'a'), time = 2),
      c = list(id = 'c', parents = "a", children = c("f", "g"), objects = list(x = 'a'), time = 5),
      d = list(id = 'd', parents = "b", children = c(), objects = list(x = 'a', y = 'b'), time = 3),
      e = list(id = 'e', parents = "b", children = c(), objects = list(), time = 4),
      f = list(id = 'f', parents = "c", children = c(), objects = list(), time = 6),
      g = list(id = 'g', parents = "c", children = c(), objects = list(x = storage::compute_id(1)), time = 7)
    ),
    class = c('graph', 'container')
  )
}

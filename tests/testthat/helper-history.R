sample_graph <- function () {
  structure(
    list(
      a = list(parents = NA_character_, children = c("b", "c"), objects = list(x = 'a')),
      b = list(parents = "a", children = c("d", "e"), objects = list(x = 'a')),
      c = list(parents = "a", children = c("f", "g"), objects = list(x = 'a')),
      d = list(parents = "b", children = c(), objects = list(x = 'a', y = 'b')),
      e = list(parents = "b", children = c(), objects = list()),
      f = list(parents = "c", children = c(), objects = list()),
      g = list(parents = "c", children = c(), objects = list(x = storage::compute_id(1)))
    ),
    class = c('container')
  )
}

sample_graph <- function () {
  structure(
    list(
      a = list(parent = NA_character_, children = c("b", "c")),
      b = list(parent = "a", children = c("d", "e")),
      c = list(parent = "a", children = c("f", "g")),
      d = list(parent = "b", children = c()),
      e = list(parent = "b", children = c()),
      f = list(parent = "c", children = c()),
      g = list(parent = "c", children = c())
    ),
    class = c('history', 'graph')
  )
}

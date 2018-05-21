step_over <- function (x, f, ...) UseMethod("step_over")

step_over.default <- function (x, f, ...) stop("cannot iterate over class ", class(x))

step_over.commits <- function (x, f, ...) {
  # 1. find root(s)
  #    (maybe by assigning parent?)
  # 2. create a map of children, or maybe simply assign children to nodes
  #    (it's copy-on-write after all; it could be a single loop together with (1))
  # 3. create a stack of nodes
  # 4. iterate on all roots; remove nodes from the stack; if there are nodes
  #    left after all the roots are done, the graph is malformed

  parents <- map()
  lapply(x$edges, function (edge) {
    parents$assign(edge$target, edge$source)
  })

  nodes <- vector()
  lapply(names(x$nodes), function (id) nodes$push_back(id))

  while (nodes$size()) {

  }
}

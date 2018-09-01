context("graph")

test_that("graph of artifacts", {
  r <- many_repository()
  a <- many_artifacts(r)
  g <- connect_artifacts(a)

  expect_length(g, 4)
  names(g) <- utilities::map_chr(g, `[[`, 'id')

  expect_equal(g$a$parents, character())
  expect_equal(g$b$parents, character())
  expect_equal(g$c$parents, c('a', 'b'))
  expect_equal(g$d$parents, 'c')

  expect_equal(g$a$children, 'c')
  expect_equal(g$b$children, 'c')
  expect_equal(g$c$children, 'd')
  expect_equal(g$d$children, character())
})


test_that("subgraph of artifacts", {
  r <- many_repository()
  a <- many_artifacts(r)
  g <- connect_artifacts(as_container(a[2:3]))

  expect_length(g, 2)
  names(g) <- utilities::map_chr(g, `[[`, 'id')

  expect_named(g, c('b', 'c'), ignore.order = TRUE)

  expect_equal(g$b$parents, character())
  expect_equal(g$c$parents, 'b')

  expect_equal(g$b$children, 'c')
  expect_equal(g$c$children, character(0))
})

test_that("stratify sample graph", {
  h <- sample_graph()

  expect_names <- function (x)
    expect_named(x, c('id', 'children', 'objects', 'parents'), ignore.order = TRUE)

  x <- stratify(h)
  expect_length(x$children, 2)
  expect_names(x)

  b <- first(x$children)
  expect_length(b$children, 2)
  expect_names(b)

  c <- last(x$children)
  expect_length(c$children, 2)
  expect_names(c)

  lapply(b$children, expect_names)
  lapply(c$children, expect_names)
})

test_that("actual repo can be stratified", {
  a <- read_artifacts(as_artifacts(sample_repository()))
  s <- stratify(connect_artifacts(a))

  # root
  expect_s3_class(s, 'artifact')
  expect_length(s$children, 1)

  # branching
  expect_length(s$children[[1]]$children, 3)
})

test_that("traverse", {
  g <- sample_graph()
  x <- traverse(g, 'g', function(id, graph) graph[[id]]$parents)
  expect_equal(x, c('g', 'c', 'a'))
})

test_that("traverse", {
  g <- sample_graph()
  x <- traverse(g, 'a', function(id, graph) graph[[id]]$children)
  expect_equal(x, letters[1:7])
})

test_that("adjust ancestry", {
  g <- sample_graph()

  h <- g[c('a', 'c', 'g')]
  expect_equal(map_int(h, function(n)length(n$parents)), c(0, 1, 1))
  expect_equal(map_int(h, function(n)length(n$children)), c(2, 2, 0))

  x <- adjust_ancestry(as_graph(h))
  expect_true(is_graph(x))
  expect_length(x, 3)
  expect_equal(map_int(x, function(n)length(n$parents)), c(0, 1, 1))
  expect_equal(map_int(x, function(n)length(n$children)), c(1, 1, 0))
})


# --- old code ---------------------------------------------------------






test_that("reduce graph", {
  h <- sample_graph()

  x <- graph_reduce(h, to = 'd')
  expect_length(x, 3)
  expect_named(x, c('a', 'b', 'd'))

  x <- graph_reduce(h, from = 'b')
  expect_length(x, 3)
  expect_named(x, c('b', 'd', 'e'))

  x <- graph_reduce(h, from = 'b', to = 'd')
  expect_length(x, 2)
  expect_named(x, c('b', 'd'))
})



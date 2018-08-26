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


test_that("stratify", {
  h <- sample_graph()

  expect_named <- function (x) testthat::expect_named(x, c('children', 'objects', 'parents'),
                                                      ignore.order = TRUE)

  x <- graph_stratify(h)
  expect_s3_class(x, 'stratified')
  expect_length(x$children, 2)
  expect_named(x)

  b <- first(x$children)
  expect_length(b$children, 2)
  expect_named(b)

  c <- last(x$children)
  expect_length(c$children, 2)
  expect_named(c)

  lapply(b$children, expect_named)
  lapply(c$children, expect_named)
})


test_that("preserve class", {
  g <- sample_graph()
  x <- graph_stratify(g)
  expect_s3_class(x, c('stratified', 'list'))

  g <- structure(list(a = structure(list(), class = c('a', 'b', 'c'))), class = 'graph')
  x <- graph_stratify(g)
  expect_s3_class(x, c('a', 'b', 'c', 'stratified'))

  g <- structure(list(a = structure(list(1), class = c('a', 'b', 'c')),
                      b = structure(list(2), class = c('a', 'b', 'c'))),
                 class = 'graph')
  x <- graph_stratify(g)
  expect_s3_class(x, c('a', 'b', 'c', 'stratified'))
})

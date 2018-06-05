context("graph")

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

  expect_named <- function (x) testthat::expect_named(x, c('children', 'objects'), ignore.order = TRUE)

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

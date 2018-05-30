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

  x <- graph_stratify(h)
  expect_s3_class(x, 'stratified')

})

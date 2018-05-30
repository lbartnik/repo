context("history")

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

test_that("find ancestors", {
  h <- sample_graph()

  x <- filter(h, ancestor_of("d"))
  expect_true(is_history(x))
  expect_named(x, c("a", "b", "d"))

  x <- filter(h, ancestor_of("g"))
  expect_true(is_history(x))
  expect_named(x, c("a", "c", "g"))

  x <- filter(h, ancestor_of("c"))
  expect_true(is_history(x))
  expect_named(x, c("a", "c"))
})


test_that("find matching data", {
  h <- sample_graph()

  x <- filter(h, data_matches(x = 1))
  expect_named(x, "g")

  x <- filter(h, data_matches(data = list(x = 1)))
  expect_named(x, "g")
})


test_that("introduced", {
  h <- sample_graph()

  expect_equal(introduced(h, 'g'), 'x')
  expect_equal(introduced(h, 'd'), 'y')
  expect_equal(introduced(h, 'b'), character())
})

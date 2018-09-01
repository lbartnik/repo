context("history")

test_that("find matching data", {
  skip("move filtering history into filtering query")
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

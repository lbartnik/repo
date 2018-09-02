context("commit")

test_that("introduced", {
  h <- sample_graph()

  expect_equal(introduced(h, 'g'), 'x')
  expect_equal(introduced(h, 'd'), 'y')
  expect_equal(introduced(h, 'b'), character())
})

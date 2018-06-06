context("selection")

test_that("filter adds up", {
  r <- many_repository()

  q <- filter(r, x == 1)
  expect_equal(qexpr_text(q, 'filter'), "x == 1")

  q <- filter(q, y == 2)
  expect_equal(qexpr_text(q, 'filter'), c("x == 1", "y == 2"))
})


test_that("arrange adds up", {
  r <- many_repository()

  q <- arrange(r, x)
  expect_equal(qexpr_text(q, 'arrange'), "x")

  q <- arrange(q, y)
  expect_equal(qexpr_text(q, 'arrange'), c("x", "y"))
})


test_that("select subsets", {
  r <- many_repository()

  q <- select(r, x, y)
  expect_equal(qexpr_text(q, 'select'), c("x", "y"))

  q <- arrange(q, x)
  expect_equal(qexpr_text(q, 'select'), "x")

  q <- arrange(q, y)
  expect_length(qexpr_text(q, 'select'), 0)
})

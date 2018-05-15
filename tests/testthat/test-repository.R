context("repository")


test_that("parents are extracted", {
  e <- as.environment(list(a = 1, b = 2))
  p <- extract_parents(e, bquote(a <- b))
  expect_equal(p, 'b')

  e <- as.environment(list(a = 1, b = 2, c = 3))
  p <- extract_parents(e, bquote(a <- b + c))
  expect_equal(p, c('b', 'c'))

  e <- as.environment(list(a = 1, b = 2, c = 3, f = function(x)x**2))
  p <- extract_parents(e, bquote(a <- f(b + c)))
  expect_equal(p, c('b', 'c', 'f'))
})


test_that("object is stripped of environments", {
  m <- lm(Sepal.Length ~ Species, iris)
  n <- strip_object(m)

  this_env <- environment()
  expect_identical(attr(m$terms, '.Environment'), this_env)
  expect_identical(attr(n$terms, '.Environment'), emptyenv())
})


test_that("stripping preserves address", {
  skip_if_not_installed("data.table")

  stripped <- strip_object(iris)
  expect_identical(stripped, iris)
  expect_identical(data.table::address(stripped), data.table::address(iris))
})

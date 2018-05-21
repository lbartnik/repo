context("utils")

test_that("nth gives n-th element", {
  expect_equal(nth(1:10, 3), 3)
  expect_equal(nth(list(1, 2, 3), 2), 2)
})


test_that("empty is recognized", {
  expect_true(is_empty(new.env()))

  e <- as.environment(list(a = 1))
  expect_false(is_empty(e))
  expect_silent(is_empty(e))

  expect_true(is_empty(NULL))
  expect_true(is_empty(NA))
  expect_true(is_empty(list()))
  expect_true(is_empty(character()))
  expect_true(is_empty(numeric()))
  expect_true(is_empty(""))

  expect_false(is_empty("a"))
  expect_false(is_empty(0))
})


test_that("error is recognized", {
  a <- try(stop("e"), silent = TRUE)
  expect_true(is_error(a))

  a <- tryCatch(stop("e"), error = function(e)e)
  expect_true(is_error(a))
})


test_that("FALSE is recognized", {
  expect_true(isFALSE(FALSE))
  expect_false(isFALSE(TRUE))
  expect_false(isFALSE(1))
})


test_that("all_named requires names", {
  expect_true(all_named(list(a = 1, b = 2)))
  expect_false(all_named(list(a = 1, 2)))
})

test_that("combine merges lists", {
  x <- combine(list(a=1), list(b=2))
  expect_equal(x, list(a=1, b=2))

  x <- combine(list(a=1), list(a=2))
  expect_equal(x, list(a=1))

  x <- combine(list(a=1), list(a=2, b=2), list(a=2, b=3, c=3))
  expect_equal(x, list(a=1, b=2, c=3))
})


test_that("map_lst assigns names", {
  x <- map_lst(list(a = 1), I)
  expect_named(x, 'a')

  x <- map_lst('a', I)
  expect_named(x, 'a')
})

test_that("napply passes names and values", {
  napply(list(a = 1), function (...) {
    args <- list(...)
    expect_length(args, 2)
    expect_equal(nth(args, 1), 'a')
    expect_equal(nth(args, 2), 1)
  })
})


test_that("napply handles edge cases", {
  expect_length(napply(NULL, print), 0)
})


test_that("not negates a function", {
  f <- function (x) identical(x, 2)
  expect_true(f(2))
  expect_false(f(1))

  nf <- not(f)
  expect_false(nf(2))
  expect_true(nf(1))
})

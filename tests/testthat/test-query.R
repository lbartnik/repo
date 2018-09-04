context("query")

test_that("repository can be turned into a query", {
  r <- london_meters()

  q <- expect_silent(as_query(r))
  expect_s3_class(q, 'query')
  expect_true(is_raw(q))
})

test_that("n() is recognized", {
  expect_true(only_n_summary(quos(n())))
  expect_true(only_n_summary(quos(a = n())))

  expect_false(only_n_summary(quos(n)))
  expect_false(only_n_summary(quos(m())))
  expect_false(only_n_summary(quos(x(n()))))
})

test_that("summary works for n()", {
  q <- as_query(many_repository())

  x <- summarise(q, n = n())
  expect_named(x, 'n')
  expect_equal(x$n, 8)
})

test_that("only n() summary is allowed", {
  q <- as_query(many_repository())
  expect_error(summarise(q, id = min(id)), regexp = "only the n.. summary is supported")
})

context("query")

test_that("repository can be turned into a query", {
  r <- london_meters()

  q <- expect_silent(as_query(r))
  expect_s3_class(q, 'query')
  expect_true(is_raw(q))
})

test_that("query can be reset", {
  s <- london_meters()$store
  q <- as_query(s)

  x <- reset_query(q)
  expect_true(is_raw(x))
  expect_equal(x$store, r$store)

  x <- reset_query(q %>% filter(a == 1) %>% arrange(desc(b)) %>% top_n(10))
  expect_length(x$filter, 0)
  expect_length(x$arrange, 0)
  expect_length(x$top_n, 0)
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

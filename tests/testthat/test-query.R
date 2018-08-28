context("query")

test_that("repository can be turned into a query", {
  r <- sample_repository()

  q <- expect_silent(as_query(r))
  expect_s3_class(q, 'query')
  expect_true(is_raw(q))
})

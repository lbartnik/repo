context("query")

test_that("repository can be turned into a query", {
  r <- sample_repository()

  q <- expect_silent(as_query(r))
  expect_s3_class(q, 'query')
  expect_true(is_raw(q))

  q <- expect_silent(as_artifacts(r))
  expect_s3_class(q, 'query')
  expect_true(is_artifacts(q))

  q <- expect_silent(as_commits(r))
  expect_s3_class(q, 'query')
  expect_true(is_commits(q))
})

test_that("only artifact query reads artifacts", {
  r <- sample_repository()

  expect_error(read_artifacts(as_query(r)))
  expect_error(read_artifacts(as_commits(r)))

  x <- expect_silent(read_artifacts(as_artifacts(r)))
  expect_length(x, 17)
})


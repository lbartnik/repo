context("artifact")

test_that("plot is recognized as such", {
  a <- structure(list(class = 'plot'), class = 'artifact')
  expect_true(artifact_is(a, 'plot'))
})

test_that("data can be loaded", {
  r <- many_repository()
  a <- new_artifact('a', r$store)
  expect_true(is_artifact(a))

  d <- artifact_data(a)
  expect_equal(d, 1)
})

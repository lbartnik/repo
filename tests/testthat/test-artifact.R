context("artifact")

test_that("plot is recognized as such", {
  a <- structure(list(class = 'plot'), class = 'artifact')
  expect_true(artifact_is(a, 'plot'))
})

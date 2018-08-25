context("artifact")

test_that("artifact is recognized as valid", {
  r <- sample_repository()
  a <- read_artifacts(as_artifacts(r))
  map(a, function (x) {
    expect_true(artifact_assert_valid(x), info = x$id)
  })
})

test_that("parents are set correctly", {
  r <- sample_repository()
  a <- read_artifacts(as_artifacts(r))
  n <- map_int(a, function (x) length(x$parents))
  expect_equal(sum(n == 0), 1)
  expect_equal(sum(n == 1), 16)
})

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

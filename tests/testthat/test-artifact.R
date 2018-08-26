context("artifact")

test_that("artifact is recognized as valid", {
  r <- sample_repository()
  a <- read_artifacts(as_artifacts(r))
  map(a, function (x) {
    expect_true(artifact_assert_valid(x), info = x$id)
  })
})

test_that("parents are set correctly", {
  a <- read_artifacts(as_artifacts(sample_repository()))
  n <- map_int(a, function (x) length(x$parents))
  expect_equal(sum(n == 0), 1)
  expect_equal(sum(n == 1), 16)
})

test_that("expression is present", {
  a <- read_artifacts(as_artifacts(sample_repository()))
  e <- map_lgl(a, function(x) is.character(x$expression) && identical(length(x$expression), 1L))
  expect_true(all(e))
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

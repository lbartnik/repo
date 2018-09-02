context("artifact")

# load once, save time
a <- read_artifacts(as_artifacts(sample_repository()))

test_that("artifact is recognized as valid", {
  map(a, function (x) {
    expect_true(artifact_assert_valid(x), info = x$id)
  })
})

test_that("parents are set correctly", {
  n <- map_int(a, function (x) length(x$parents))
  expect_equal(sum(n == 0), 1)
  expect_equal(sum(n == 1), 16)
})

test_that("expression is present", {
  e <- map_lgl(a, function(x) is.character(x$expression) && identical(length(x$expression), 1L))
  expect_true(all(e))
})

test_that("plot is recognized as such", {
  a <- structure(list(class = 'plot'), class = 'artifact')
  expect_true(artifact_is(a, 'plot'))
})

test_that("data can be loaded", {
  r <- sample_repository()
  a <- new_artifact(nth(a, 2)$id, r$store)
  expect_true(is_artifact(a))

  d <- artifact_data(a)
  expect_s3_class(d, 'data.frame')
  expect_equal(ncol(d), 3)
  expect_equal(nrow(d), 26280)
})

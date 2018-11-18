context("artifact")

# load once, save time
a <- read_artifacts(as_artifacts(london_meters()))

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
  r <- london_meters()
  a <- new_artifact(sample_artifact_id(), r$store)
  expect_true(is_artifact(a))

  d <- artifact_data(a)
  expect_s3_class(d, 'data.frame')
  expect_equal(ncol(d), 3)
  expect_equal(nrow(d), 8760)
})



test_replot <- function (method, distance) {
  a <- as_artifacts(iris_model()) %>%
    filter(id == '0f1105f2e5992669196384b0a66536ef7dfc4111') %>%
    read_artifacts %>%
    (utilities::first)

  # rudimentary test: whether anything happens and there are no errors
  path <- tempfile(fileext = '.png')
  png(path, width = 800, height = 600)
  expect_silent(replot(a, method))
  dev.off()

  expect_true(file.size(path) > 0)

  # there is no good way to compare images - other than to use search::image_dist
  if (utilities::try_load(search) && utilities::try_load(imager)) {
    d <- image_dist(
      unwrap_image(load.image(path)),
      unwrap_image(load.image('expected-output/0f1105f2.png'))
    )
    expect_true(d < distance)
  }
}

test_that("re-evaluate a plot", test_replot('re-evaluate', 10))
test_that("replay a plot", test_replot('replay', .1))

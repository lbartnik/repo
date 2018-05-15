context("repository-impl")

test_that("auto tags generate class and time", {
  x <- auto_tags(1)
  expect_named(x, c("class", "time"))
})


test_that("auto tags honor presets", {
  x <- auto_tags(1)
  expect_equal(x$class, "numeric")

  x <- auto_tags(1, class = "xxx")
  expect_equal(x$class, "xxx")
})


test_that("parents are extracted", {
  e <- as.environment(list(a = 1, b = 2))
  p <- extract_parents(e, bquote(a <- b))
  expect_equal(p, 'b')

  e <- as.environment(list(a = 1, b = 2, c = 3))
  p <- extract_parents(e, bquote(a <- b + c))
  expect_equal(p, c('b', 'c'))

  e <- as.environment(list(a = 1, b = 2, c = 3, f = function(x)x**2))
  p <- extract_parents(e, bquote(a <- f(b + c)))
  expect_equal(p, c('b', 'c', 'f'))
})


test_that("object is stripped of environments", {
  m <- lm(Sepal.Length ~ Species, iris)
  n <- strip_object(m)

  this_env <- environment()
  expect_identical(attr(m$terms, '.Environment'), this_env)
  expect_identical(attr(n$terms, '.Environment'), emptyenv())
})


test_that("stripping preserves address", {
  skip_if_not_installed("data.table")

  stripped <- strip_object(iris)
  expect_identical(stripped, iris)
  expect_identical(data.table::address(stripped), data.table::address(iris))
})


# --- repository update ------------------------------------------------

test_that("updater processes objects", {
  r <- empty_repository()
  e <- list(a = 1)
  u <- repository_updater(r, as.environment(e), NULL, bquote(a <- 1))

  u$process_objects()

  expect_equal(u$expr, bquote(a <- 1))
  expect_null(u$plot)

  expect_equal(u$objects, list(a = 1))
  expect_equal(u$ids, list(a = storage::compute_id(1)))
  expect_named(u$tags, 'a')
  expect_named(u$tags$a, c('class', 'time', 'parents'), ignore.order = TRUE)

  expect_named(u$new, 'a')
})


test_that("updater identifies new object", {
  r <- single_repository()
  e <- list(a = 1, b = 2)
  u <- repository_updater(r, as.environment(e), NULL, bquote(b <- a + 1))

  u$process_objects()

  expect_named(u$objects, c('a', 'b'), ignore.order = TRUE)
  expect_named(u$tags, 'b')
  expect_named(u$new, 'b')
  expect_named(u$tags$b$parents, 'a')
})


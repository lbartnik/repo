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


test_that("regular environment is not stripped", {
  e <- as.environment(list(a = 1))
  expect_equal(strip_object(e), e)
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


test_that("updater identifies new plot", {
  r <- single_repository(last_plot = NULL)
  e <- list(a = 1)
  p <- dummy_plot()
  u <- repository_updater(r, as.environment(e), p, bquote(plot(a)))

  expect_null(u$last_plot)
  expect_equal(u$plot, p)
  u$process_plot()

  expect_not_null(u$plot_id)
  expect_named(u$plot_tags, c("class", "time", "parents"), ignore.order = TRUE)
  expect_named(u$plot_tags$parents, "a")
})


test_that("updater ignores repeated plot", {
  p <- dummy_plot()
  r <- single_repository(last_plot = plot_as_svg(p))
  e <- list(a = 1)
  u <- repository_updater(r, as.environment(e), p, bquote(plot(a)))

  expect_not_null(u$last_plot)
  expect_equal(u$plot, p)
  u$process_plot()

  expect_true(is.na(u$plot_id))
  expect_false(exists('plot_tags', envir = u))
})


test_that("updater recognizes changes among objects", {
  process_objects <- function (...) {
    u <- repository_updater(single_repository(), as.environment(list(...)),
                            NULL, bquote(a <- 1))
    u$process_objects()
    u$process_plot()
    u
  }

  # test a number of scenarios
  u <- process_objects(a = 1)
  expect_false(u$introduced_changes())

  u <- process_objects(a = 2)
  expect_true(u$introduced_changes())

  u <- process_objects(b = 1)
  expect_true(u$introduced_changes())
})


test_that("updater recognizes changes to plots", {
  process_plot <- function (plot, last_plot = NULL) {
    u <- repository_updater(single_repository(last_plot = last_plot),
                            as.environment(list(a = 1)), plot, bquote(plot(a)))
    u$process_objects()
    u$process_plot()
    u
  }

  # test a number of scenarios
  u <- process_plot(NULL)
  expect_false(u$introduced_changes())

  u <- process_plot(dummy_plot())
  expect_true(u$introduced_changes())

  u <- process_plot(dummy_plot(), plot_as_svg(dummy_plot()))
  expect_false(u$introduced_changes())

  # removing a plot should not trigger a new commit
  u <- process_plot(NULL, dummy_plot())
  expect_false(u$introduced_changes())
})


test_that("objects are written", {
  r <- single_repository()
  u <- repository_updater(r, as.environment(list(a = 2)), NULL, bquote(a <- 2))
  s <- r$store

  expect_length(s, 2)

  u$process_objects()
  u$process_plot()
  ct_id <- u$write()

  ids <- storage::os_list(s)
  expect_length(ids, 4)
  expect_true(storage::compute_id(2) %in% ids)

  x <- storage::os_read(s, ct_id)
  expect_named(x$tags, c("class", "parent", "time"), ignore.order = TRUE)
  expect_named(x$object, c("expr", "objects", "plot"), ignore.order = TRUE)
  expect_named(x$object$objects, "a")
  expect_equal(x$object$objects$a, storage::compute_id(2))
})


test_that("plot is written", {
  r <- single_repository()
  p <- dummy_plot()
  u <- repository_updater(r, as.environment(list(a = 1)), p, bquote(plot(a)))
  s <- r$store

  expect_length(s, 2)

  u$process_objects()
  u$process_plot()
  ct_id <- u$write()

  ids <- storage::os_list(s)
  expect_length(ids, 4)

  x <- storage::os_read(s, ct_id)
  expect_true(x$object$plot %in% ids)

  t <- storage::os_read_object(s, x$object$plot)
  expect_true(svg_equal(t, plot_as_svg(p)))
})


test_that("changes are synchronized into the repository", {
  r <- single_repository()
  u <- repository_updater(r, new.env(), NULL, bquote(plot(a)))

  u$last_commit_id <- 'last_commit_id'
  u$ids <- list(a = 'id1', b = 'id2')
  u$svg <- 'svg'

  u$sync_repo()

  expect_named(r$last_commit, c("id", "objects"), ignore.order = TRUE)
  expect_equal(r$last_commit$id, 'last_commit_id')
  expect_equal(r$last_commit$objects, u$ids)
  expect_equal(r$last_plot, u$svg)
})

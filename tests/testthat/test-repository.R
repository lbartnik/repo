context("repository")

test_that("auto tags generate class and time", {
  x <- auto_tags(1)
  expect_named(x, c("class", "time", "artifact"))
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
  expect_named(u$tags$a, c('class', 'time', 'parents', 'artifact'), ignore.order = TRUE)

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


test_that("parents not present", {
  # if parent object is not present in the parent commit, e.g. iris
  r <- single_repository()
  e <- new.env(parent = globalenv())

  e$x <- 100
  u <- repository_updater(r, e, NULL, bquote(x <- lm(a ~ b, iris)))
  expect_warning(u$process_objects())

  expect_length(u$tags$x$parents, 0)
  expect_type(u$tags$x$parents, "list")

  e$a <- 1
  u <- repository_updater(r, e, NULL, bquote(y <- a + lm(d ~ b, iris)))
  expect_warning(u$process_objects())

  p <- u$tags$x$parents
  expect_length(p, 1)
  expect_type(p, "list")
  expect_named(p, "a")
  expect_equal(first(p), storage::compute_id(1))
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
  expect_named(u$plot_tags, c("class", "time", "parents", "artifact"), ignore.order = TRUE)
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

  expect_equal(u$plot_id, character(0))
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
  o_id  <- storage::compute_id(2)

  ids <- storage::os_list(s)
  expect_length(ids, 4)
  expect_true(storage::compute_id(2) %in% ids)

  x <- storage::os_read(s, ct_id)
  expect_named(x$tags, c("class", "parent", "time", "artifact"), ignore.order = TRUE)
  expect_named(x$object, c("expr", "objects", "plot"), ignore.order = TRUE)
  expect_named(x$object$objects, "a")
  expect_equal(x$object$objects$a, o_id)

  y <- storage::os_read(s, o_id)
  expect_named(y$tags, c("artifact", "class", "names", "parents", "parent_commit", "time"),
               ignore.order = TRUE)
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


test_that("empty history", {
  r <- empty_repository()

  x <- repository_history(r, 'current')
  expect_s3_class(x, 'history')
  expect_length(x, 0)
})


test_that("full explanation", {
  r <- many_repository()

  x <- repository_explain(r)
  expect_length(x, 4)
  expect_named(x, letters[1:4])

  expect_node(x, 'a', parents = list(), children = 'c')
  expect_node(x, 'b', parents = list(), children = 'c')
  expect_node(x, 'c', parents = list(a = 'a', b = 'b'), children = 'd')
  expect_node(x, 'd', parents = list(c = 'c'), children = character())
})


test_that("explain object", {
  r <- many_repository()

  x <- repository_explain(r, 'd')
  expect_length(x, 4)

  x <- repository_explain(r, 'c')
  expect_length(x, 3)
  expect_node(x, 'a', parents = list(), children = 'c')
  expect_node(x, 'b', parents = list(), children = 'c')
  expect_node(x, 'c', parents = list(a = 'a', b = 'b'), children = character())

  x <- repository_explain(r, 'b')
  expect_length(x, 1)
  expect_node(x, 'b', parents = list(), children = character())
})


# --- commit -----------------------------------------------------------

test_that("commit returns its data", {
  r <- single_repository()
  c <- commit(r$store, 'p')

  d <- c$data
  expect_named(d, 'a')
  expect_equivalent(unlist(d), 1)
})

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


test_that("updater", {
  r <- repository(storage::memory())
  e <- list(a = 1)
  u <- repository_updater(r, as.environment(e), dummy_plot(), bquote(a <- 1))
})

context("meta")


test_that("tags for data.frame", {
  t <- auto_tags(iris)
  expect_named(t, c("nrow", "ncol", "colnames", "class", "time", "artifact", "session"),
               ignore.order = TRUE)
  expect_equal(t$nrow, 150)
  expect_equal(t$ncol, 5)
  expect_equal(t$class, "data.frame")
})


test_that("default describe ignores custom operators", {
  assign("[[.xyz", function (x, i) stop('this should not be called'), envir = globalenv())
  o <- structure(list(a = 1), class = 'xyz')

  p <- expect_silent(describe(o))
})


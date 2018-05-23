context("history")

test_that("reduce graph", {

})

test_that("find ancestors", {
  h <- sample_graph()

  x <- history_ancestors(h, "d")
  expect_named(x$data, c("a", "b", "d"))

  x <- history_ancestors(h, "g")
  expect_named(x$data, c("a", "c", "g"))

  x <- history_ancestors(h, "c")
  expect_named(x$data, c("a", "c"))
})



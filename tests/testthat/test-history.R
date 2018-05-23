context("history")

test_that("reduce graph", {

})

test_that("find ancestors", {
  h <- structure(list(data = list(
    a = list(parent = NA_character_, children = c("b", "c")),
    b = list(parent = "a", children = c("d", "e")),
    c = list(parent = "a", children = c("f", "g")),
    d = list(parent = "b", children = c()),
    e = list(parent = "b", children = c()),
    f = list(parent = "c", children = c()),
    g = list(parent = "c", children = c())
  )),
  class = c('history', 'graph'))

  x <- history_ancestors(h, "d")
  expect_named(x, c("a", "b", "d"))

  x <- history_ancestors(h, "g")
  expect_named(x, c("a", "c", "g"))

  x <- history_ancestors(h, "c")
  expect_named(x, c("a", "c"))
})



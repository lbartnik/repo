context("selection")

test_that("filter adds up", {
  r <- many_repository()

  q <- filter(r, x == 1)
  expect_equal(quos_text(q$filter), "x == 1")

  q <- filter(q, y == 2)
  expect_equal(quos_text(q$filter), c("x == 1", "y == 2"))
})


test_that("arrange adds up", {
  r <- many_repository()

  q <- arrange(r, x)
  expect_equal(quos_text(q$arrange), "x")

  q <- arrange(q, y)
  expect_equal(quos_text(q$arrange), c("x", "y"))
})


test_that("select subsets", {
  r <- many_repository()

  q <- select(r, x, y)
  expect_equal(quos_text(q$select), c("x", "y"))

  q <- select(q, x)
  expect_equal(quos_text(q$select), "x")

  expect_error(select(q, y), "selection reduced to an empty set")
})


test_that("execute runs the query", {
  r <- many_repository()

  x <- select(r, id) %>% execute
  expect_named(x, "id")
  expect_length(x$id, 4)
  expect_equal(x$id, letters[1:4])

  x <- select(r, id) %>% filter(class == "integer") %>% execute
  expect_equal(x$id, "b")
  x <- select(r, id) %>% filter(class == "numeric") %>% execute
  expect_equal(x$id, c("a", "c"))

  x <- select(r, id) %>% filter(class == "numeric") %>% arrange(desc(id)) %>% execute
  expect_equal(x$id, c("c", "a"))
})


test_that("various types of select", {
  r <- many_repository()

  # a single column
  x <- select(r, id) %>% execute
  expect_named(x, "id")
  expect_setequal(x$id, letters[1:4])

  # from character
  y <- select(r, "id") %>% execute
  expect_equal(x, y)

  # basically everything
  x <- select(r, -artifact) %>% execute
  expect_named(x, c("object", "id", "class", "parent_commit", "parents", "time"),
               ignore.order = TRUE)
  expect_equal(nrow(x), 4)
})





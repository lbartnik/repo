context("selection")


# --- filter -----------------------------------------------------------

test_that("filter adds up", {
  r <- as_query(many_repository())

  q <- filter(r, x == 1)
  expect_equal(rlang::quo_squash(first(q$filter)), bquote(x == 1))

  q <- filter(q, y == 2)
  expect_equal(rlang::quo_squash(first(q$filter)), bquote(x == 1))
  expect_equal(rlang::quo_squash(second(q$filter)), bquote(y == 2))
})

# --- arrange ----------------------------------------------------------

test_that("arrange adds up", {
  r <- as_query(many_repository())

  q <- arrange(r, x)
  expect_equal(rlang::quo_squash(first(q$arrange)), bquote(x))

  q <- arrange(q, y)
  expect_equal(rlang::quo_squash(first(q$arrange)), quote(x))
  expect_equal(rlang::quo_squash(second(q$arrange)), quote(y))
})

# --- top_n ------------------------------------------------------------

test_that("top_n chooses top n entries", {
  r <- as_query(many_repository())

  q <- top_n(r, 2)
  expect_equal(q$top, 2)

  expect_error(top_n(r, 0))
  expect_error(top_n(r, -1))
  expect_error(top_n(r, "a"))
  expect_error(top_n(r, 10, some_column))
})

# --- update -----------------------------------------------------------

test_that("update", {
  r <- many_repository()
  q <- filter(as_query(r), id == 'a')

  expect_tag <- function (tag, value) {
    expect_equal(nth(storage::os_read_tags(r$store, 'a'), tag), value, label = tag)
  }

  q %>% update(class = 'xyz')
  expect_tag('class', 'xyz')

  q %>% update(append(names, 'new_name'))
  expect_tag('names', c('a', 'new_name'))

  q %>% update(remove(names, 'new_name'))
  expect_tag('names', 'a')

  q %>% update(append(collections, 'new_col'))
  expect_tag('collections', 'new_col')
})



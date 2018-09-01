context("transient")

test_that("extract ancestor id", {
  id <- extract_ancestor_id(quo(ancestor_of('x')))
  expect_equal(id, 'x')
})

test_that("ancestor_of_impl", {
  r <- sample_repository()
  id <- '13b2c2164cb69ed8f1922d141cc0deca73e04f18'

  x <- ancestor_of_impl(id, commit_graph(r$store))
  expect_true(is.character(x))
  expect_length(x, 5)
})

test_that("no_children_impl", {
  s <- sample_repository()$store
  g <- commit_graph(s)

  x <- no_children_impl(g)
  expect_length(x, 3)
})

test_that("no_parents_impl", {
  s <- sample_repository()$store
  g <- commit_graph(s)

  x <- no_parents_impl(g)
  expect_length(x, 1)
})

test_that("extract data match", {
  exp <- list(d = storage::compute_id(1), e = storage::compute_id(2),
              f = storage::compute_id(3))

  x <- extract_data_match(quo(data_matches(d = 1)))
  expect_equal(x, exp[1])

  x <- extract_data_match(quo(data_matches(d = 1, e = 2)))
  expect_equal(x, exp[1:2])

  x <- extract_data_match(quo(data_matches(data = list(d = 1, e = 2))))
  expect_equal(x, exp[1:2])

  x <- extract_data_match(quo(data_matches(d = 1, data = list(e = 2, f = 3))))
  expect_equal(x, exp)
})

test_that("data_matches_impl", {
  r <- many_repository()
  m <- list(a = 'a', b = 'b', c = 'c')

  x <- data_matches_impl(m[1], r$store)
  expect_equal(x, 'p')

  x <- data_matches_impl(m[1:2], r$store)
  expect_equal(x, 'q')

  x <- data_matches_impl(m, r$store)
  expect_equal(x, c('r', 's'))

  x <- data_matches_impl(m[2:3], r$store)
  expect_length(x, 0)
})

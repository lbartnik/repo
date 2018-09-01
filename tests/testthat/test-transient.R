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

test_that("can find tips of branches", {
  skip("implement branch_tip")
  r <- sample_repository()
  h <- repository_history(r)

  x <- filter(h, branch_tip())
  expect_length(x, 3)
})

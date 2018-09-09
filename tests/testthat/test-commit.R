context("commit")

test_that("commit constructor", {
  s <- london_meters()$store
  c <- expect_silent(new_commit("13b2c2164cb69ed8f1922d141cc0deca73e04f18", s))

  expect_true(is_commit(c))
  expect_true(is_valid_commits(c))
  expect_equal(commit_store(c), s)
})

test_that("print commit", {
  c <- expect_silent(new_commit("13b2c2164cb69ed8f1922d141cc0deca73e04f18", london_meters()$store))
  expect_output(print(c), "<commit: x input m>")
})

test_that("commit data", {
  c <- expect_silent(new_commit('p', many_repository()$store))
  d <- commit_data(c)

  expect_equal(d, list(a = 1))
})

test_that("checkout commit", {
  c <- expect_silent(new_commit('p', many_repository()$store))
  e <- new.env()

  commit_checkout(c, e)
  expect_named(e, 'a')
  expect_equal(e$a, 1)
})

test_that("introduced", {
  h <- sample_graph()

  expect_equal(introduced(h, 'g'), 'x')
  expect_equal(introduced(h, 'd'), 'y')
  expect_equal(introduced(h, 'b'), character())
})

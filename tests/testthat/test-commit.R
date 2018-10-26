context("commit")

test_that("commit constructor", {
  s <- london_meters()$store
  c <- expect_silent(new_commit(sample_commit_id(), s))

  expect_true(is_commit(c))
  expect_true(is_valid_commits(c))
  expect_equal(commit_store(c), s)
})

test_that("print commit", {
  c <- expect_silent(new_commit(sample_commit_id(), london_meters()$store))
  expect_output(print(c), "<commit: x input meter_4929 hourly m>")
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

test_that("as environment", {
  c <- new_commit(sample_commit_id(), london_meters()$store)
  e <- as_environment(c)

  expect_named(e, c("hourly", "input", "m", "meter_4929", "x"), ignore.order = TRUE)
  expect_s3_class(e$m, "lm")
  expect(is.data.frame(e$x))
  expect(is.data.frame(e$input))
})

context("commit")

test_that("commit constructor", {
  c <- expect_silent(new_commit("13b2c2164cb69ed8f1922d141cc0deca73e04f18", london_meters()$store))
  expect_true(is_commit(c))
  expect_true(is_valid_commits(c))
})

test_that("print commit", {
  c <- expect_silent(new_commit("13b2c2164cb69ed8f1922d141cc0deca73e04f18", london_meters()$store))
  expect_output(print(c), "<commit: x input m>")
})

test_that("introduced", {
  h <- sample_graph()

  expect_equal(introduced(h, 'g'), 'x')
  expect_equal(introduced(h, 'd'), 'y')
  expect_equal(introduced(h, 'b'), character())
})

test_that("checkout commit", {
  skip("implement")
})

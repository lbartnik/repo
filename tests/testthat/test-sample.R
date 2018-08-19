context("sample")

test_that("simulation command is recognized", {
  rep <- empty_repository()
  rss <- R_session_simulator(rep)

  expect_message(rss$run(simulation_set('time', 1)),
                'simulation command: simulation_set\\("time", 1\\)')
  expect_equal(simulation$time, 1)
})

test_that("commit can be restored", {
  rep <- empty_repository()
  rss <- R_session_simulator(rep)

  rss$run(x <- 1)
  expect_equal(rss$contents(), list(x = 1))

  rss$run(simulation_commit_remember())

  rss$run(x <- 2)
  expect_equal(rss$contents(), list(x = 2))

  rss$run(simulation_commit_restore())
  expect_equal(rss$contents(), list(x = 1))
})

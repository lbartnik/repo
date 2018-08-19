context("sample")

test_that("simulation command is recognized", {
  rep <- empty_repository()
  rss <- R_session_simulator(rep)

  expect_message(rss$run_quoted(quote(simulation_set('time', 1))),
                'simulation command: simulation_set\\("time", 1\\)')
  expect_equal(simulation$time, 1)
})

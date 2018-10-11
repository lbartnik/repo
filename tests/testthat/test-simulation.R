context("sample")

test_that("meta command", {
  expect_true(is_meta_command(quote(meta::a(1))))
  expect_false(is_meta_command(quote(metaa::a(1))))
  expect_false(is_meta_command(quote(a(1))))
})

test_that("extract meta command", {
  expect_equal(extract_meta_command(quote(meta::a(1))), "simulation_meta_a")
})

test_that("simulation command is recognized", {
  rss <- session_simulator(empty_repository(), .silent = FALSE)

  tm <- current_time()

  expect_message(rss$run(meta::set('time', 1000)),
                'meta command: meta::set\\("time", 1000\\)')
  expect_equal(simulation_meta_state$time, 1000)

  expect_true(difftime(current_time(), tm, units = 'secs') >= 1000)
})

test_that("commit can be restored", {
  rss <- session_simulator(empty_repository())

  rss$run(x <- 1)
  expect_equal(rss$contents(), list(x = 1))

  rss$run(meta::commit_remember())

  rss$run(x <- 2)
  expect_equal(rss$contents(), list(x = 2))

  rss$run(meta::commit_restore())
  expect_equal(rss$contents(), list(x = 1))
})

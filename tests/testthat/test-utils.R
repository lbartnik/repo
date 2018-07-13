context("utils")

test_that("r session id does not change", {
  expect_equal(r_session_id(), r_session_id())
})


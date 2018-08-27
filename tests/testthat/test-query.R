context("query")

test_that("repository can be turned into a query", {
  r <- sample_repository()

  q <- expect_silent(as_query(r))
  expect_s3_class(q, 'query')
  expect_true(is_raw(q))

  q <- expect_silent(as_artifacts(r))
  expect_s3_class(q, 'query')
  expect_true(is_artifacts(q))

  q <- expect_silent(as_commits(r))
  expect_s3_class(q, 'query')
  expect_true(is_commits(q))
})

test_that("query type matches read type", {
  r <- sample_repository()

  expect_error(read_artifacts(as_query(r)))
  expect_error(read_artifacts(as_commits(r)))

  expect_error(read_commits(as_query(r)))
  expect_error(read_commits(as_artifacts(r)))

  x <- expect_silent(read_artifacts(as_artifacts(r)))
  expect_true(is_container(x))
  expect_length(x, 17)

  #x <- expect_silent(read_commits(as_commits(r)))
  #expect_true(is_container(x))
  # TODO expect_length(x, 10)
})

test_that("symbol is matched", {
  s <- quote(id)

  expect_true(expr_match(quote(id), s))
  expect_true(expr_match(quote(id == 1), s))
  expect_true(expr_match(quote(f(id)), s))
  expect_true(expr_match(quote(f(id) == 1), s))
  expect_true(expr_match(quote(f(a, b, id ** 2) == 1), s))

  expect_false(expr_match(quote(f(id = 2)), s))
  expect_false(expr_match(quote(id(1)), s))
})

test_that("symbol in quos", {
  q <- list(rlang::quo(id == 1), rlang::quo(f(z)))

  expect_equal(quos_match(q, quote(id)), c(T, F))
  expect_equal(quos_match(q, 'id'), c(T, F))
  expect_equal(quos_match(q, 'z'), c(F, T))

  expect_equal(quos_match(q, 'a'), c(F, F))
})

test_that("select by id", {
  r <- as_query(many_repository())

  # no filter
  x <- r %>% select_ids
  expect_equal(x, letters[1:4])

  # first special case
  x <- r %>% filter(id == 'a') %>% select_ids
  expect_equal(x, 'a')

  # first special case: variable
  i <- 'a'
  x <- r %>% filter(id == i) %>% select_ids
  expect_equal(x, 'a')

  # second special case
  x <- r %>% filter(id %in% c('a', 'b')) %>% select_ids
  expect_equal(x, letters[1:2])

  # general case
  x <- r %>% filter(id != 'a') %>% select_ids
  expect_equal(x, letters[2:4])
})

test_that("select by id and tags", {
  q <- as_query(many_repository())

  p <- q %>% filter(id == 'a', isTRUE(artifact))
  x <- expect_silent(select_ids(p))
  expect_equal(x, 'a')
})

test_that("select by multiple references to id", {
  q <- as_query(many_repository())

  p <- q %>% filter(id == 'a', id != 'b')
  x <- expect_silent(select_ids(p))
  expect_equal(x, 'a')
})

test_that("read tag names", {
  r <- many_repository()
  names <- read_tags_names('a', r$store)
  expect_true(setequal(names,
                       c("class", "parent_commit", "parents", "time", "artifact", "names")))
})




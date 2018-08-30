context("query-specialized")

test_that("repository can be turned into a specialized query", {
  r <- sample_repository()

  q <- expect_silent(as_artifacts(r))
  expect_s3_class(q, 'query')
  expect_true(is_artifacts(q))

  q <- expect_silent(as_commits(r))
  expect_s3_class(q, 'query')
  expect_true(is_commits(q))

  q <- expect_silent(as_tags(r))
  expect_s3_class(q, 'query')
  expect_true(is_tags(q))
})

test_that("query type matches read type", {
  r <- sample_repository()

  expect_error(read_artifacts(as_query(r)))
  expect_error(read_artifacts(as_commits(r)))
  expect_error(read_artifacts(as_tags(r)))

  expect_error(read_commits(as_query(r)))
  expect_error(read_commits(as_artifacts(r)))
  expect_error(read_commits(as_tags(r)))

  expect_error(read_tags(as_query(r)))
  expect_error(read_tags(as_artifacts(r)))
  expect_error(read_tags(as_commits(r)))

  x <- expect_silent(read_artifacts(as_artifacts(r)))
  expect_true(is_container(x))
  expect_length(x, 17)

  #x <- expect_silent(read_commits(as_commits(r)))
  #expect_true(is_container(x))
  # TODO expect_length(x, 10)

  x <- expect_silent(read_tags(as_tags(r)))
  expect_true(is_tibble(x))
  expect_equal(nrow(x), 17)
  expect_equal(ncol(x), 14)
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
  all <- c(letters[1:4], letters[16:19])

  # no filter
  x <- r %>% match_ids
  expect_equal(x, all)

  # first special case
  x <- r %>% filter(id == 'a') %>% match_ids
  expect_equal(x, 'a')

  # first special case: variable
  i <- 'a'
  x <- r %>% filter(id == i) %>% match_ids
  expect_equal(x, 'a')

  # second special case
  x <- r %>% filter(id %in% c('a', 'b')) %>% match_ids
  expect_equal(x, letters[1:2])

  # general case
  x <- r %>% filter(id != 'a') %>% match_ids
  expect_equal(x, all[-1])
})

test_that("select by id and tags", {
  q <- as_query(many_repository())

  p <- q %>% filter(id == 'a', isTRUE(artifact))
  x <- expect_silent(match_ids(p))
  expect_equal(x, 'a')
})

test_that("select by multiple references to id", {
  q <- as_query(many_repository())

  p <- q %>% filter(id == 'a', id != 'b')
  x <- expect_silent(match_ids(p))
  expect_equal(x, 'a')
})

test_that("select top tags", {
  q <- as_query(many_repository())

  p <- q %>% top_n(2)
  x <- expect_silent(match_ids(p))
  expect_equal(x, c('a', 'b'))
})

test_that("arrange and select top tags", {
  q <- as_query(many_repository())

  p <- q %>% arrange(desc(time)) %>% top_n(2)
  x <- expect_silent(match_ids(p))
  expect_equal(x, c('s', 'r'))
})

test_that("read tag names", {
  r <- many_repository()
  names <- read_tag_names('a', r$store)
  expect_true(setequal(names,
                       c("class", "parent_commit", "parents", "time", "artifact", "names")))
})

known_tags <- c("artifact", "class", "names", "parent_commit", "parents", "time")

test_that("all tag names", {
  r <- many_repository()

  n <- read_tag_names(letters[1:4], r$store)
  expect_equal(sort(n), known_tags)

  n <- read_tag_names('a', r$store)
  expect_equal(sort(n), known_tags)
})

test_that("all tag values", {
  r <- many_repository()

  n <- read_tag_values(letters[1:4], known_tags, r$store)
  expect_named(n, known_tags, ignore.order = TRUE)
  expect_equal(n$names, as.list(letters[1:4]))

  n <- read_tag_values('a', known_tags, r$store)
  expect_named(n, known_tags, ignore.order = TRUE)
  expect_equal(n$names, list('a'))
})

test_that("read_tags", {
  q <- as_tags(many_repository())

  # id column
  x <- read_tags(q, id)
  expect_named(x, "id")
  expect_setequal(x$id, letters[1:4])

  # id column from character
  y <- read_tags(q, "id")
  expect_equal(x, y)

  # everything but one tag
  x <- read_tags(q, -artifact)
  expect_named(x, c("id", "class", "names", "parent_commit", "parents", "time"),
               ignore.order = TRUE)
  expect_equal(nrow(x), 4)

  # a single actual tag
  y <- read_tags(q, names)
  expect_equal(y$names, letters[1:4])
})

test_that("all tag names for empty query", {
  q <- as_tags(many_repository())

  expect_length(read_tags(q), 7)

  p <- filter(q, TRUE)
  expect_length(read_tags(p), 7)

  q <- filter(q, FALSE)
  expect_error(read_tags(q), "query does not match any objects")
})

test_that("complex tag queries", {
  q <- as_tags(many_repository())

  x <- filter(q, class == "integer") %>% read_tags(id)
  expect_equal(x$id, "b")

  x <- filter(q, class == "numeric") %>% read_tags(id)
  expect_equal(x$id, c("a", "c"))

  x <- filter(q, class == "numeric") %>% arrange(desc(id)) %>% read_tags(id)
  expect_equal(x$id, c("c", "a"))

  x <- arrange(q, id) %>% top_n(1) %>% read_tags(id)
  expect_equal(x$id, "a")

  x <- arrange(q, desc(id)) %>% top_n(1) %>% read_tags(id)
  expect_equal(x$id, "d")
})

test_that("simplify tags", {
  r <- flatten_lists(list(x = list(1, 2, 3), y = list(1, NULL, 2)))
  expect_named(r, c("x", "y"))
  expect_equal(r$x, 1:3)
  expect_equal(r$y, c(1, NA_real_, 2))

  r <- flatten_lists(list(x = 1:4, y = list(c(1L, 2L), NULL, 3L, 4L)))
  expect_named(r, c("x", "y"))
  expect_equal(r$x, 1:4)
  expect_equal(r$y, list(1:2, NA_integer_, 3L, 4L))

  tm <- as.POSIXct(1:10, origin = '1970-01-01')
  r <- flatten_lists(list(x = as.list(tm)))
  expect_named(r, 'x')
  expect_equal(r$x, tm)
})

test_that("simple read_commits", {
  q <- as_commits(sample_repository())

  x <- read_commits(q)
  expect_s3_class(x, 'container')
  expect_length(x, 16)
  expect_true(all(map_chr(x, class) == 'commit'))
})

test_that("complex read_commits", {
  q <- as_commits(sample_repository())

  # requires full access to all elements of the same type (commits, artifacts)
  x <- q %>% filter(ancestor_of(x)) %>% read_commits()

  # double pass: (1) assign children, (2) filter
  q %>% filter(no_children()) %>% read_commits()

  # single pass, parents are stored in a tag
  q %>% filter(no_parents()) %>% read_commits()

  # single pass through commit objects
  q %>% filter(data_matches(x)) %>% read_commits()

  # what if there are other filters?
})

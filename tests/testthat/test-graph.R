context("graph")

test_that("graph of artifacts", {
  r <- many_repository()
  a <- many_artifacts(r)
  g <- connect_artifacts(a)

  expect_length(g, 4)
  names(g) <- utilities::map_chr(g, `[[`, 'id')

  expect_length(g$a$parents, 0)
  expect_length(g$b$parents, 0)
  expect_equal(g$c$parents, as_id(c('a', 'b')))
  expect_equal(g$d$parents, as_id('c'))

  expect_equal(g$a$children, as_id('c'))
  expect_equal(g$b$children, as_id('c'))
  expect_equal(g$c$children, as_id('d'))
  expect_length(g$d$children, 0)
})


test_that("subgraph of artifacts", {
  r <- many_repository()
  a <- many_artifacts(r)
  g <- connect_artifacts(as_container(a[2:3]))

  expect_length(g, 2)
  names(g) <- utilities::map_chr(g, `[[`, 'id')

  expect_named(g, c('b', 'c'), ignore.order = TRUE)

  expect_length(g$b$parents, 0)
  expect_equal(g$c$parents, as_id('b'))

  expect_equal(g$b$children, as_id('c'))
  expect_length(g$c$children, 0)
})

test_that("stratify sample graph", {
  h <- sample_graph()

  expect_names <- function (x)
    expect_named(x, c('id', 'children', 'objects', 'parents', 'time'), ignore.order = TRUE)

  x <- stratify(h)
  expect_length(x$children, 2)
  expect_names(x)

  b <- first(x$children)
  expect_length(b$children, 2)
  expect_names(b)

  c <- last(x$children)
  expect_length(c$children, 2)
  expect_names(c)

  lapply(b$children, expect_names)
  lapply(c$children, expect_names)
})

test_that("actual repo can be stratified", {
  a <- read_artifacts(as_artifacts(london_meters()))
  s <- stratify(connect_artifacts(a))

  # root
  expect_s3_class(s, 'artifact')
  expect_length(s$children, 1)

  # branching
  expect_length(s$children[[1]]$children, 3)
})

test_that("stratify, each element once", {
  a <- read_artifacts(as_artifacts(iris_model()))
  s <- stratify(connect_artifacts(a))

  # x <- iris
  x <- s
  expect_equal(x$name, "x")
  expect_length(x$children, 1)

  # x <- x %>% mutate(Sepal.Area = ...)
  x <- first(x$children)
  expect_equal(x$name, "x")
  expect_length(x$children, 1)

  # virginica <- x %>% mutate(Virginica = ...)
  #
  # here artifact has multiple children because another downstream "virginica"
  # comes from both this "virginica" data.frame and the lm model "m";
  # the expression that creates it is: virginica$predict <- predict(m, virginica)
  #
  # this test explicitly checks that the second "virginica" is assigned under
  # "m" (because it's the closer parent, so it makes more sense to show it there
  # as it reflects user's train of thought better), but also that it is assigned
  # only once (there was a bug where a child node would be assigned under each
  # parent)
  x <- first(x$children)
  expect_equal(x$name, "virginica")
  expect_length(x$children, 1)

  # m <- lm(Virginica ~ ...
  x <- first(x$children)
  expect_equal(x$name, "m")
  expect_length(x$children, 1)

  # make sure each child is extracted only once
  extract_children <- function (x) c(x$id, unlist(lapply(x$children, extract_children)))
  expect_setequal(extract_children(s), map_chr(a, `[[`, 'id'))
})


test_that("traverse parents", {
  g <- sample_graph()
  f <- function(id, graph) graph[[id]]$parents

  x <- traverse(g, 'g', f)
  expect_equal(x, c('g', 'c', 'a'))

  x <- traverse(g, 'd', f)
  expect_equal(x, c("d", "b", "a"))

  x <- traverse(g, 'c', f)
  expect_equal(x, c('c', 'a'))
})

test_that("traverse children", {
  g <- sample_graph()
  x <- traverse(g, 'a', function(id, graph) graph[[id]]$children)
  expect_equal(x, letters[1:7])
})

test_that("adjust ancestry", {
  g <- sample_graph()

  h <- g[c('a', 'c', 'g')]
  expect_equal(map_int(h, function(n)length(n$parents)), c(0, 1, 1))
  expect_equal(map_int(h, function(n)length(n$children)), c(2, 2, 0))

  x <- adjust_ancestry(as_graph(h))
  expect_true(is_graph(x))
  expect_length(x, 3)
  expect_equal(map_int(x, function(n)length(n$parents)), c(0, 1, 1))
  expect_equal(map_int(x, function(n)length(n$children)), c(1, 1, 0))
})

test_that("commit graph", {
  r <- london_meters()

  x <- commit_graph(r$store)
  expect_length(x, 16)

  len <- map_int(x, function(n)length(n$parents))
  expect_equivalent(as.numeric(table(len)), c(`0`=1, `1`=15))

  len <- map_int(x, function(n)length(n$children))
  expect_equivalent(as.numeric(table(len)), c(`0`=1, `1`=15))
})

test_that("artifact graph", {
  r <- london_meters()

  x <- artifact_graph(r$store)
  expect_length(x, 17)
})

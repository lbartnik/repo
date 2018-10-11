empty_repository <- function () {
  repository(storage::memory())
}

single_repository <- function (...) {
  r <- empty_repository()
  r$last_commit <- list(objects = list(a = storage::compute_id(1)), id = 'p')

  args <- list(...)
  stopifnot(utilities::is_all_named(args))
  utilities::imap(args, function (value, name) {
    assign(name, value, envir = r)
  })

  storage::os_write(r$store, 1, list(), storage::compute_id(1))
  storage::os_write(r$store, list(objects = r$last_commit$objects,
                                  expr = bquote(), plot = NA_character_),
                    list(parent = NA_character_, time = NA),
                    r$last_commit$id)

  r
}


# TODO rename to in-memory-repository
many_repository <- function () {
  # every time a new test repository is created, the meta-clock should be reset
  simulation_meta_state$time <- 0
  r <- empty_repository()

  add_object(r, 'a', 1, 'p', list())
  add_object(r, 'b', 2L, 'q', list())
  add_object(r, 'c', 3, 'r', list(a = 'a', b = 'b'))
  add_object(r, 'd', replot_as(dummy_plot(), 'svg'), 's', list(c = 'c'), cls = 'plot')

  add_commit(r, 'p', NA_character_, bquote(a <- 1), list(a = 'a'))
  add_commit(r, 'q', 'p', bquote(b <- 2L), list(a = 'a', b = 'b'))
  add_commit(r, 'r', 'q', bquote(c <- a + b), list(a = 'a', b = 'b', c = 'c'))
  add_commit(r, 's', 'r', bquote(plot(c)), list(a = 'a', b = 'b', c = 'c'), plot = 'd')

  r
}

add_object <- function (r, id, value, parent_commit, parents, cls) {
  if (missing(cls)) cls <- class(value)
  tags <- list(class = cls, parent_commit = parent_commit,
               parents = parents, time = Sys.time() + strtoi(charToRaw(id)), artifact = TRUE,
               names = id)
  storage::os_write(r$store, value, tags, id)
}


add_commit <- function (r, id, parent, expr, objects, plot = character()) {
  storage::os_write(r$store,
                    list(objects = objects, expr = expr, plot = plot),
                    list(parent = parent, time = Sys.time() + strtoi(charToRaw(id)), class = 'commit'),
                    id)
}


# follows many_repository(); returns artifacts for that repository
many_artifacts <- function (r) {
  a <- lapply(letters[1:4], function (id) {
    a <- list(id = id)
    class(a) <- 'artifact'
    attr(a, 'store') <- r$store
    a
  })
  structure(a, class = 'container')
}

sample_commit_id <- function () {
  "13b2c2164cb69ed8f1922d141cc0deca73e04f18"
}

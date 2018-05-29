empty_repository <- function () {
  repository(storage::memory())
}

single_repository <- function (...) {
  r <- empty_repository()
  r$last_commit <- list(objects = list(a = storage::compute_id(1)), id = 'p')

  args <- list(...)
  stopifnot(all_named(args))
  napply(args, function (name, value) {
    assign(name, value, envir = r)
  })

  storage::os_write(r$store, 1, list(), storage::compute_id(1))
  storage::os_write(r$store, list(objects = r$last_commit$objects,
                                  expr = bquote(), plot = NA_character_),
                    list(parent = NA_character_, time = NA),
                    r$last_commit$id)

  r
}

many_repository <- function () {
  r <- empty_repository()

  append_commit(r, 'a', NA_character_, bquote(a <- 1), a = 1)
  append_commit(r, 'b', 'a', bquote(b <- 2), a = 1, b = 2)
}


append_commit <- function (r, id, parent, expr, ..., plot = NA_character_) {
  objects <- lapply(list(...), function (obj) {
    id <- storage::compute_id(obj)
    storage::os_write(r$store, obj, list(time = Sys.time()), id)
  })

  storage::os_write(r$store, list(objects = objects, expr = expr, plot = plot),
                    list(parent = parent, time = Sys.time()), id)
}


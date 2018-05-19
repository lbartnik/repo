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
  storage::os_write(r$store, list(objects = r$last_commit$objects), list(),
                    r$last_commit$id)

  r
}


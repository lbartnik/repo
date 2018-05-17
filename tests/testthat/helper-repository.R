empty_repository <- function () {
  repository(storage::memory())
}

single_repository <- function (...) {
  r <- empty_repository()
  r$last_commit <- commit(r, list(a = storage::compute_id(1)),
                          NA_character_, bquote(a <- 1), NA_character_)

  args <- list(...)
  stopifnot(all_named(args))
  napply(args, function (name, value) {
    assign(name, value, envir = r)
  })

  storage::os_write(r$store, 1, list(), storage::compute_id(1))
  storage::os_write(r$store, list(objects = 'a'), list(), 'p')

  r
}


empty_repository <- function () {
  repository(storage::memory())
}

single_repository <- function () {
  r <- empty_repository()
  r$last_commit <- commit(r, list(a = storage::compute_id(1)),
                          NA_character_, bquote(a <- 1), NA_character_)

  storage::os_write(r$store, 1, list(), storage::compute_id(1))
  storage::os_write(r$store, list(objects = 'a'), list(), 'p')

  r
}


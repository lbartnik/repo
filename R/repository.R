#' Creates a new repository backed by the given storage.
#'
#' @param store Read and write objects into this storage.
#'
#' @import proto
#' @name repository
#' @export
repository <- function (store)
{
  stopifnot(storage::is_object_store(store))

  r <- proto::proto(expr = {
    store     = store
    last_plot = NULL
  })
  r$last_commit = commit(r, list(), NA_character_, bquote(), NA_character_)


  class(r) <- c('repository', class(r))
  r
}


is_repository <- function (x) inherits(x, 'repository')


#' @rdname repository_append
repository_update <-function (repo, env, plot, expr) {
  guard()

  updater <- repository_updater(repo, env, plot, expr)
  updater$process_objects()
  updater$process_plot()

  candidate <- updater$candidate_commit()

  # if there are new artifacts, store a new commit
  if (commit_changed(.$last_commit, candidate)) {
    updater$write()
    updater$sync()
  }
}


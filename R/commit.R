new_commit <- function (id, store) {
  both <- os_read(store, id)

  commit <- both$object
  commit$id <- id
  commit$parents <- both$tags$parent

  structure(commit, class = 'commit')
}

is_commit <- function (x) inherits(x, 'commit')

is_valid_commits <- function (x) {
  stop('define')
}

print.commit <- function (x, ...) {
  cat0("<commit: ", join(names(x$objects), ' '), '>\n')
  invisible(x)
}

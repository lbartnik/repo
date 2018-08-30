new_commit <- function (id, store) {
  commit <- os_read_object(store, id)
  commit$id <- id
  structure(commit, class = 'commit')
}

print.commit <- function (x, ...) {
  cat0("<commit: ", join(names(x$objects), ' '), '>\n')
  invisible(x)
}

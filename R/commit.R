new_commit <- function (id, store) {
  commit <- os_read_object(store, id)
  structure(commit, class = 'commit')
}

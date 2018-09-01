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

introduced <- function (commits, id) {
  stopifnot(id %in% names(commits))

  c <- commits[[id]]
  if (is.na(c$parent)) return(names(c$objects))

  p <- commits[[c$parent]]
  new_objs <- Filter(function (n) {
    is.na(match(n, names(p$objects))) || !identical(c$objects[[n]], p$objects[[n]])
  }, names(c$objects))

  # there is a plot (first condition) and it's different from
  # what was there before (second condition)
  if (!is.null(c$plot) && !identical(c$plot, p$plot)) {
    return(c(new_objs, '::plot::'))
  }

  new_objs
}

#' @rdname history
#'
#' @param commit as returned by [repository_history()].
#' @param env Environment to check-out artifacts to.
#'
#' @export
commit_checkout <- function (commit, env) {
  mapply(names(commit$data), commit$data, FUN = function (name, value) {
    assign(name, value, envir = env)
  })
  invisible()
}

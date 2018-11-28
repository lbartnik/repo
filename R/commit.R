#' Creating and manipulating commits.
#'
#' @param id commit identifier.
#' @param store object store, see [storage::object_store].
#'
#' @rdname commit-internal
new_commit <- function (id, store) {
  both <- os_read(store, id)

  commit <- list()
  commit$id <- id
  commit$parents    <- as_id(both$tags$parent)
  commit$time       <- both$tags$time
  commit$expression <- both$object$expr
  commit$plot       <- both$object$plot
  commit$objects    <- both$object$objects

  structure(commit, class = 'commit', store = store)
}


#' @param x commit object.
#' @rdname commit-internal
commit_store <- function (x) attr(x, 'store')


#' Commit API.
#'
#' @param x commit object.
#'
#' @name commit
#' @rdname commit
NULL

#' @export
#' @rdname commit
is_commit <- function (x) inherits(x, 'commit')

#' @rdname commit-internal
is_valid_commit <- function (x) {
  has_name(x, 'objects') &&
    has_name(x, "expression") &&
    has_name(x, "plot") &&
    has_name(x, "id") &&
    has_name(x, "parents") &&
    has_name(x, "time") &&
    !is.null(attr(x, 'store'))
}

#' @inheritDotParams base::print
#' @export
#' @rdname commit
print.commit <- function (x, ...) {
  cat0("<commit: ", join(names(x$objects), ' '), '>\n')
  invisible(x)
}

#' @rdname commit
commit_data <- function (x) {
  stopifnot(is_commit(x))
  lapply(x$objects, function (id) os_read_object(commit_store(x), as_id(id)))
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

#' Copy commit into environment.
#'
#' @param commit as returned by [read_commits].
#' @param env Environment to check-out artifacts to.
#'
#' @export
#' @rdname commit
commit_checkout <- function (commit, env) {
  data <- commit_data(commit)
  mapply(names(data), data, FUN = function (name, value) {
    assign(name, value, envir = env)
  })
  invisible()
}

#' @rdname commit-internal
#' @param parent parent environment for the newly created one.
as_environment <- function (x, parent = new.env()) {
  stopifnot(is_commit(x))

  objects <- lapply(x$objects, function (id) storage::os_read_object(commit_store(x), as_id(id)))
  e <- as.environment(objects)
  parent.env(e) <- parent

  e
}


# --- old code ---------------------------------------------------------

commit <- function (store, id) {
  stopifnot(storage::is_object_store(store))
  stopifnot(storage::is_id(id))

  data <- storage::os_read(store, id)

  raw  <- data$object
  tags <- data$tags

  stopifnot(has_name(raw, 'objects'), has_name(raw, 'expr'), has_name(raw, 'plot'))
  stopifnot(has_name(tags, 'parent'), has_name(tags, 'time'))

  raw <- as.environment(raw)

  raw$id       <- id
  raw$parent   <- tags$parent
  raw$children <- c()
  raw$time     <- tags$time

  attr(raw, 'store') <- store
  structure(raw, class = 'commit')
}

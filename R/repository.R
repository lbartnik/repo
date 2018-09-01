#' Creates a new repository backed by the given storage.
#'
#' @param store Read and write objects into this storage.
#'
#' @rdname repository
#'
#' @importFrom proto proto
#' @export
#'
repository <- function (store)
{
  stopifnot(storage::is_object_store(store))

  r <- proto::proto(expr = {
    store       = store
    last_png    = NULL
    last_commit = list(objects = list(), id = NA_character_)
  })

  class(r) <- c('repository', class(r))
  r
}


#' @description `is_repository` verifies whether `x` is a repository
#' object.
#'
#' @param x Object to be tested, converted or printed.
#'
#' @rdname repository
#' @export
#'
is_repository <- function (x) inherits(x, 'repository')


#' @param ... further arguments passed to or from other methods.
#'
#' @rdname repository
#' @export
#'
print.repository <- function (x, ...) {
  cat(toString(x), '\n')
  invisible(x)
}


#' @rdname repository
#' @export
#'
toString.repository <- function (x, ...) {
  paste0('<repository:', toString(x$store), '>')
}


#' @description `repository_update` appends a new commit to the repository.
#'
#' @param repo A repository object.
#' @param env Environment to create a commit from (e.g. [globalenv]).
#' @param plot A recorded plot (see [grDevices::recordPlot]).
#' @param expr The expression related to the most recent changed in `env`.
#'
#' @rdname repository
#' @export
#'
repository_update <- function (repo, env, plot, expr) {
  guard()
  stopifnot(is_repository(repo))

  updater <- repository_updater(repo, env, plot, expr)
  updater$process_objects()
  updater$process_plot()

  # if there are new artifacts, store a new commit
  if (updater$introduced_changes()) {
    updater$write()
    updater$sync_repo()
  }
}


#' @description `repository_explain` returns a graph that describes
#' the _origin_ of an artifact (an R object or a plot) with the given
#' identifier `id`. If no `id` is provided, an aggregated graph containing
#' all artifacts is returned.
#'
#' @param id List of artifact identifiers.
#' @param ancestors Retrieve ancestors at most that far in the ancestry
#'        tree from specified `id`s.
#'
#' @importFrom rlang abort
#'
#' @rdname repository
#' @export
#'
repository_explain <- function (repo, id = NULL, ancestors = "unlimited") {
  if (is.null(id) && !identical(ancestors, "unlimited")) {
    abort("cannot limit the number of ancestors if `id` is NULL")
  }

  if (is.null(id)) {
    commits <- all_commits(repo$store)
    objects <- unique(map_chr(commits, function (c) unlist(c$objects)))
    parents <- map_chr(objects, function (id) {
      storage::os_read_tags(repo$store, id)$parents
    })
    plots <- unique(map_chr(commits, function (c) c$plot))
    ids <- unique(c(objects, parents, plots))
  }
  else {
    if (identical(ancestors, "unlimited")) ancestors <- 0xDEADBEEF
    ids <- object_origin(repo, id, ancestors)
  }

  # annotate objects with information about: name, parent, commit, type, etc.
  objects <- lapply(ids, function (id) {
    tags <- storage::os_read_tags(repo$store, id)

    stopifnot(has_name(tags, c("class", "parents", "time", 'parent_commit')))

    tags$id <- id
    tags$commit <- tags$parent_commit
    tags$children <- character()
    tags$description <- description(tags)
    if (is_empty(tags$names)) tags$names <- character() # plots don't have names

    # remove
    tags$parent_commit <- NULL

    # read parent commit and assign expression
    cmt <- storage::os_read_object(repo$store, tags$commit)
    tags$expr <- cmt$expr

    # finally, add a S3 class for pretty-printing
    structure(tags, class = 'artifact.meta')
  })

  g <- connect_artifacts(as_container(objects))
  structure(g, class = c('origin', 'artifact.set', class(g)))
}



#' @description `repository_rewind` changes the internal pointer to the
#' _last commit_ and, if `id` denotes a historical commit, sets it to
#' that value. Subsequent commits will be recorded as descendants of
#' commit `id`.
#'
#' @rdname repository
#'
#' @export
#'
repository_rewind <- function (repo, id) {
  guard()
  stopifnot(is_repository(repo))

  tryCatch({
    tags <- storage::os_read_tags(repo$store, id)
    stopifnot(identical(tags$class, "commit"))
  }, error = function (e) {
    stop("cannot find commit matching id ", id, call. = FALSE)
  })

  repo$last_commit <- list(
    id = id,
    objects = storage::os_read_object(repo$store, id)$objects
  )

  invisible()
}

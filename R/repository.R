#' Creates a new repository backed by the given storage.
#'
#' @param store Read and write objects into this storage.
#'
#' @import proto
#' @name repository
#' @rdname repository
#'
#' @export
repository <- function (store)
{
  stopifnot(storage::is_object_store(store))

  r <- proto::proto(expr = {
    store       = store
    last_plot   = NULL
    last_commit = list(objects = list(), id = NA_character_)
  })

  class(r) <- c('repository', class(r))
  r
}


is_repository <- function (x) inherits(x, 'repository')


#' @rdname repository
#' @export
repository_update <-function (repo, env, plot, expr) {
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


#' @description `repository_history` returns a graph that describes the
#' execution of commands in R session, also know as the __time__ view.
#' Each node in that graph represents the state of R session (aka. a
#' __commit__) at a given point in time. Each edge represents a single
#' R command issued by the user. If the optional parameter `id` is
#' specified, the history is limited to the subtree of __commits__
#' with `id` as the root.
#'
#' @rdname repository
#' @export
#'
repository_history <- function (repo, mode = 'all') {
  guard()
  stopifnot(is_repository(repo))
  stopifnot(mode %in% c("all", "current"))

  query <- list(rlang::quo(class == 'commit'))
  ids   <- storage::os_find(repo$store, query)
  nodes <- map_lst(ids, function(id) commit(repo$store, id))

  # when all nodes are extracted, assign children
  lapply(nodes, function (node) {
    if (!is.na(node$parent)) {
      nodes[[node$parent]]$children <<- append(nodes[[node$parent]]$children, node$id)
    }
  })

  ans <- structure(nodes, class = c('history', 'graph'))

  # if only the current branch, filter out that branch
  if (identical(mode, 'current')) {
    return(filter(ans, ancestor_of(repo$last_commit$id)))
  }

  # else, return everything ("all")
  ans
  # wrap in a 'commits' object that
  # 1. can be turned into a 'stratified' object
  # 2. can be turned into a 'deltas' object
  # 3. can be turned into JSON
  # 4. can be iterated over
}


#' @description `repository_explain` returns a graph that describes
#' the __origin__ of an artifact (an R object or a plot) with the given
#' identifier `id`. If no `id` is provided, an aggregated graph containing
#' all artifacts is returned.
#'
#' @rdname repository
#' @export
repository_explain <- function (repo, id = NULL) {
  # 1. find object
  # 2. recursively find all parents

  # 3. wrap explanation in a 'origin' object that can be
  # a) turned into a 'stratified' object
  # b) turned into JSON
  # c) iterated over
}


#' @rdname repository
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

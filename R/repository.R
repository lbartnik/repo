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
#'
#' @export
#'
# TODO is it needed at all anymore?
# TODO if it stays, node keys need to be agreed with repository_explain
repository_history <- function (repo, mode = 'all') {
  guard()
  stopifnot(is_repository(repo))
  stopifnot(mode %in% c("all", "current"))

  nodes <- all_commits(repo$store)

  # assign children
  lapply(nodes, function (node) {
    if (!is.na(node$parent)) {
      nodes[[node$parent]]$children <<- append(nodes[[node$parent]]$children, node$id)
    }
    node$new <- introduced(nodes, node$id)
  })


  # if only the current branch, filter out that branch
  if (identical(mode, 'current')) {
    nodes <- if (is.na(repo$last_commit$id)) list()
             else filter(nodes, ancestor_of(repo$last_commit$id))
  }

  # assign class and keep store handy
  nodes <- structure(nodes, class = c('history', 'graph'), store = repo$store)

  # wrap in a 'commits' object that
  # 1. can be turned into a 'stratified' object
  # 2. can be turned into a 'deltas' object
  # 3. can be turned into JSON
  # 4. can be iterated over
}


#' @description `repository_explain` returns a graph that describes
#' the _origin_ of an artifact (an R object or a plot) with the given
#' identifier `id`. If no `id` is provided, an aggregated graph containing
#' all artifacts is returned.
#'
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
    raw_tags <- storage::os_read_tags(repo$store, id)

    tags_copy <- c("class", "parents", "time")
    stopifnot(has_name(raw$tags, c(tags_copy, 'parent_commit')))

    obj <- raw_tags[tags_copy]
    obj$id <- id
    obj$commit <- raw_tags$parent_commit
    obj$children <- character()
    obj$names <- raw_tags$names # plots don't have names
    obj$description <- description(raw_tags)

    # read parent commit and assign expression
    cmt <- storage::os_read_object(repo$store, obj$commit)
    obj$expr <- cmt$expr

    # finally, add a S3 class for pretty-printing
    structure(obj, class = 'artifact.meta')
  })
  names(objects) <- ids

  # add information about children
  lapply(objects, function (obj) {
    lapply(obj$parents, function (parent) {
      if (parent %in% names(objects)) {
        objects[[parent]]$children <<- append(objects[[parent]]$children, obj$id)
      }
    })
  })

  structure(objects, class = c('origin', 'artifact.set', 'graph'))

  # 3. wrap explanation in a 'origin' object that can be
  # a) turned into a 'stratified' object
  # b) turned into JSON
  # c) iterated over
}


#' @importFrom rlang warn
#' @export
print.artifact.set <- function (x, ..., style = 'by_time') {

  # this is the only currently supported method
  stopifnot(identical(style, 'by_time'))

  # if there is nothing to print
  if (!length(x)) {
    warn("origin object empty")
  }

  if (identical(style, 'by_time')) {
    # sort entries and then print them
    i <- order(map_dbl(x, `[[`, 'time'), decreasing = FALSE)
    x <- x[i]

    # insert \n between two printouts
    print(first(x), style = 'short')
    lapply(x[-1], function (y) { cat('\n'); print(y, style = 'short') })
  }

  if (identical(style, 'tree')) {
    fork <- '├'
    vert <- '│   '
    lead <- '── '
    last <- '└'
    print_level <- function (x, l) {
      cat(stri_paste(rep(vert, l), collapse = ''))
      cat0(fork, lead)
      print(x, style = 'line')
      i <- order(map_dbl(x$children, `[[`, 'time'), decreasing = FALSE)
      lapply(x$children[i], print_level, l = l + 1)
      invisible(x)
    }
    print_level(remove_class(graph_stratify(x), 'artifact.set'), 0)
  }

  invisible(x)
}


#' @description `print.explained` pretty-prints a description of an
#' artifact.
#'
#' @importFrom storage shorten
#'
#' @rdname repository
#' @export
print.artifact.meta <- function (x, ..., style = 'full') {

  stopifnot(style %in% c('full', 'short', 'line'))
  is_plot <- ('plot' %in% x$class)

  # full artifact description
  if (identical(style, 'full')) {
    # preamble
    ccat0(silver = "Artifact: ", green = shorten(x$id), silver = if (is_plot) ' (plot)', '\n')

    # expression that produced this artifact
    ccat0(silver = 'Expression:\n', format_expr(x$expr))

    # more meta-data
    if (!is_plot) ccat(silver = '\nName:   ', x$names)
    ccat(silver = '\nClass:  ', x$class)
    ccat(silver = '\nCreated:', x$time)
    ccat(silver = '\nSummary:', x$description)
    cat('\n')
  }

  # shortened artifact description
  if (identical(style, 'short')) {
    ccat0(green = storage::shorten(x$id))

    if (length(x$parents)) {
      ccat0(silver = '  parents:')
      imap(x$parents, function(id, name) {
        ccat0(' ', name, silver = ' (', yellow = storage::shorten(id), silver = ')')
      })
    }
    else {
      ccat0(silver = '  no parents')
    }

    ccat0('\n', format_expr(x$expr), '\n')
  }

  # a single line
  if (identical(style, 'line')) {
    if ('plot' %in% x$class)
      cat('<plot>\n')
    else
      cat0(first(x$names), ' ', first(x$class), '\n')
  }

  invisible(x)
}


#' @description `repository_rewind` changes the internal pointer to the
#' _last commit_ and, if `id` denotes a historical commit, sets it to
#' that value. Subsequent commits will be recorded as descendants of
#' commit `id`.
#'
#' @param repo Repository object.
#' @param id Commit identifier.
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


#' @description `as_deltas` converts a `history` object (a tree of
#' _commits_) into an equivalent tree of _artifacts_. Each node in the
#' tree of artifacts represents a single artifact introduced in a given
#' commit. It is still a representation of historical changes in R
#' session but at the level of a single artifact rather than at the
#' level of a snapshot of R session (a _commit_).
#'
#' @rdname repository
#' @export
#'
as_deltas <- function (x) {
  stopifnot(is_history(x))
  history_to_deltas(x)
}


#' @rdname repository
#' @export
#'
as_origin <- function (x) {
  stopifnot(is_history(x))
}


#' Tree-related operations.
#'
#' @rdname trees
#' @export
#'
stratify <- function (x) {
  stopifnot(is_graph(x))
  graph_stratify(x)
}


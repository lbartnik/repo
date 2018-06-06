all_commits <- function (store) {
  guard()
  query <- list(rlang::quo(class == 'commit'))
  ids   <- storage::os_find(store, query)
  map_lst(ids, function(id) commit(store, id))
}


print.commit <- function (x, ...) {
  cat("<commit: ", join(names(x$objects), ' '), '>\n')
}

# --- private API: update ------------------------------------------------------


repository_updater <- function (repo, env, plot, expr) {
  guard()
  stopifnot(is_repository(repo), is.environment(env))

  u <- repo$proto()
  u$env  <- env
  u$plot <- plot
  u$expr <- expr

  u$process_objects <- function (.) {
    .$objects <- lapply(as.list(env), strip_object)
    .$ids <- lapply(.$objects, storage::compute_id)
    .$new <- Filter(function (id) !storage::os_exists(.$store, id), .$ids)

    dbg("newly created: ", names(.$new))

    .$tags <- lapply(names(.$new), function (name) auto_tags(.$objects[[name]]))
    .$tags <- napply(with_names(.$tags, names(.$new)), function (name, tags) {
      names <- extract_parents(env, expr)
      dbg(name, " parents: ", paste(names, collapse = ", "))

      names2 <- intersect(names, names(.$last_commit$objects))
      if (!setequal(names, names2)) {
        warning("parents identified but not present in the previous commit ",
                name, ": ", paste(setdiff(names, names2), collapse = ", "),
                call. = FALSE)
      }

      tags$parents <- .$last_commit$objects[names2]
      tags
    })
  }

  u$process_plot <- function (.) {
    .$svg <- plot_as_svg(.$plot)

    # if the current plot looks the same as the last one, do not update at all
    if (is.null(.$svg) || svg_equal(.$svg, .$last_plot)) {
      .$plot_id <- character()
      return()
    }

    .$plot_id <- storage::compute_id(.$svg)
    if (storage::os_exists(.$store, .$plot_id)) {
      dbg("plot already present")
      return()
    }

    .$plot_tags <- auto_tags(.$svg, class = 'plot')
    names <- extract_parents(env, expr)
    .$plot_tags$parents <- .$last_commit$objects[names]
  }

  u$introduced_changes <- function (.) {
    ia <- .$ids
    ib <- .$last_commit$objects

    sorted_names <- function(x) if (is.null(names(x))) character() else sort(names(x))
    an <- sorted_names(ia)
    bn <- sorted_names(ib)

    return(!identical(ia[an], ib[bn]) || (!is.null(.$svg) && !svg_equal(.$svg, .$last_plot)))
  }

  u$write <- function (.) {
    # store list of object pointers + basic 'history' tags
    data <- list(expr = .$expr, objects = .$ids, plot = .$plot_id)
    tags <- auto_tags(NULL, class = 'commit', parent = .$last_commit$id)
    cid  <- storage::compute_id(list(data, tags))

    # this should never happen because hash is computed from both objects
    # and parent id; if it does happen, something is SERIOUSLY broken
    if (storage::os_exists(.$store, cid)) {
      stop("commit already exists, aborting")
    }

    # write the commit meta-data
    storage::os_write(.$store, data, tags, id = cid)

    # write objects, append the parent commit id to tags
    napply(.$new, function (name, id) {
      dbg("artifact `", name, "` not present, storing [", id, "]")
      storage::os_write(.$store, .$objects[[name]], id = id,
                        tags = c(.$tags[[name]], list(parent_commit = cid)))
    })

    if (length(.$plot_id)) {
      dbg("storing new plot [", id, "] with parents: ", paste(parents, collapse = ", "))
      storage::os_write(.$store, .$svg, id = .$plot_id,
                        tags = c(.$plot_tags, list(parent_commit = cid)))
    }

    .$last_commit_id <- cid
    invisible(cid)
  }

  u$sync_repo <- function (.) {
    .$.super$last_commit <- list(id = .$last_commit_id, objects = .$ids)
    .$.super$last_plot   <- .$svg
  }

  u
}


extract_parents <- function (env, expr)
{
  # add the "parent objects" tag using "defer"
  fn <- function(){}; body(fn) <- expr

  df <- defer::defer_(fn, .caller_env = env, .extract = TRUE)
  ev <- defer::extract_variables(df)
  ef <- defer::extract_functions(df)

  ef$entry <- NULL
  c(names(ev), names(ef))
}


# TODO could be turned into a S3 method
auto_tags <- function (obj, ...) {
  preset <- list(...)
  stopifnot(all_named(preset))

  combine(preset, list(class = class(obj), time = Sys.time(), artifact = TRUE))
}


#' Returns a base64-encoded, SVG plot.
#'
#' @param pl Plot recorded by [recordPlot()].
#' @return `character` string, base64-encoded SVG plot.
#' @import jsonlite
#'
plot_as_svg <- function (pl)
{
  guard()
  if (is.null(pl)) return(NULL)

  # TODO use svglite::stringSVG

  path <- tempfile(fileext = ".svg")

  # TODO if `pl` has been recorded without dev.control("enable"), the
  #      plot might be empty; it might be necessary to check for that

  svg(path)
  replayPlot(pl)
  dev.off()

  contents <- readBin(path, "raw", n = file.size(path))
  jsonlite::base64_enc(contents)
}



#' Compare two SVG images.
#'
#' SVG images are processed in this package as base64-encoded, XML text
#' data. When produced, certain differences are introduced in XML
#' attributes that have no effect on the final plots. That is why,
#' however, SVG plots need to be compared graphically and not textually.
#' This function produces a thumbnail of each SVG image and then
#' compares the raster graphic.
#'
#' @param a First SVG image.
#' @param b Second SVG image.
#' @return `TRUE` if SVGs are the same plot-wise.
#'
#' @import rsvg
#' @import jsonlite
#'
svg_equal <- function (a, b)
{
  if (is_empty(a)) return(is_empty(b))
  if (is_empty(b)) return(FALSE)

  a <- try(rsvg(base64_dec(a), 100, 100), silent = TRUE)
  b <- try(rsvg(base64_dec(b), 100, 100), silent = TRUE)
  if (is_error(a) && is_error(b)) return(TRUE)

  isTRUE(all.equal(a, b))
}


#' Removes references to environments.
#'
#' Some objects (e.g. formula, lm) store references to environments
#' in which they were created. This function replaces each such reference
#' with a reference to `emptyenv()`.
#'
#' As much as possible, this function tries not to make any copies of
#' the original data. This is because the address of the object might
#' be used to determine whether object's identifier needs to be computed
#' which might be a costly operation.
#'
#' @param obj Object to be processed.
#' @return `obj` with environment references replaced by `emptyenv()`
#'
strip_object <- function (obj, attr = FALSE)
{
  if (is.symbol(obj)) return(obj)
  if (inherits(obj, 'recordedplot')) return(obj)

  # TODO should we disregard any environment?
  if (is.environment(obj) && isTRUE(attr)) return(emptyenv())

  attrs <- if (!is.null(attributes(obj))) lapply(attributes(obj), strip_object, attr = TRUE)

  if (is.list(obj)) {
    obj_tmp <- lapply(obj, strip_object, attr = FALSE)
    # use stripped object only if stripping actually changed something
    obj_lst <- lapply(obj, function(x)x)
    if (!identical(obj_tmp, obj_lst)) {
      obj <- obj_tmp
    }
  }
  if (!identical(attributes(obj), attrs)) {
    attributes(obj) <- attrs
  }

  obj
}

# --- history ----------------------------------------------------------

commit <- function (store, id) {
  stopifnot(storage::is_object_store(store))

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


#' @export
`$.commit` <- function (x, i) {
  if (i %in% names(x)) return(x[[i]])
  if (identical(i, 'data')) {
    store <- attr(x, 'store')
    x$data <- map_lst(x$objects, function (id) storage::os_read_object(store, id))
    return(x[["data"]])
  }

  stop('unknown key ', i)
}


# --- explain ----------------------------------------------------------

object_origin <- function (repo, id) {
  black <- vector()
  grey  <- vector(id)
  while (grey$size()) {
    id <- grey$pop_front()
    lapply(storage::os_read_tags(repo$store, id)$parents, function (id) {
      if (!black$find(id)) grey$push_back(id)
    })
    black$push_back(id)
  }
  as.character(black$values)
}


print.origin <- function (x) {
  cat('<origin-graph>', length(x), 'node(s)\n')
}


# --- deltas -----------------------------------------------------------

#' Transform a graph of commits into a graph of deltas.
#'
#' A _delta_ is an introduction of a new artifact (object, plot, printout)
#' in the R session. Graph of deltas is easier to read for a person than
#' a graph of commits becase only the relevant (new) information is shown
#' in each node of the graph. Thus, translating from commits to deltas is
#' the first step to present the history of changes in R session recorded
#' in commits.
#'
#' @description `history_to_deltas` is the main function which orchestrates
#' the transformation.
#'
#' @param hist Object returned by [repository_history].
#' @return Object of S3 class `deltas`.
#'
#' @rdname deltas
#' @importFrom utils head tail
#'
history_to_deltas <- function (hist)
{
  stopifnot(is_history(hist))
  store <- attr(hist, 'store')

  nodes <- map()
  convert <- function (commit_id, parent_delta) {
    commit  <- hist[[commit_id]]
    new_ids <- commit$objects[introduced(hist, commit_id)]

    mapply(new_ids, c(parent_delta, head(new_ids, -1)), FUN = function (child, parent) {
      from_store <- storage::os_read(store, child)
      delta <- from_store$tags
      delta$id <- child
      delta$parent <- parent
      delta$description <- description(from_store$object)
      nodes$assign(delta$id, delta)
    })

    parent_delta <- last(new_ids)
    lapply(commit$children, function (commit_id) convert(commit_id, parent_delta))
  }

  roots <- names(filter(hist, no_parent()))
  lapply(roots, function (id) convert(id, NA_character_))

  # return the final "steps" structure
  structure(nodes$values, class = 'deltas')
}


#' @description `is_deltas` verifies if the given object is a valid
#' `deltas` structure.
#'
#' @rdname deltas
#'
is_deltas <- function (x) inherits(x, 'deltas')


#' Provide a summary of an object.
#'
#' @param object Object to be described.
#'
#' @import broom
#' @rdname internals
#'
description <- function (object)
{
  if (is_empty(object)) return(NA_character_)

  if (is.data.frame(object)) return(paste0('data.frame[', nrow(object), ', ', ncol(object), ']'))

  if (inherits(object, 'lm')) {
    g <- broom::glance(object)
    return(paste0('lm adjR2:', format(g$adj.r.squared, digits = 2),
                  ' AIC:', format(g$AIC, digits = 2),
                  ' df:', g$df))
  }

  paste(class(object), collapse = '::')
}

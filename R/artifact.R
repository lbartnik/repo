
#' Create an artifact DTO.
#'
#' Creates a DTO (data transfer object) that fully describes an artifact
#' from the repository. It is the central object for external (as opposed
#' to internal to this package) processing, printing, etc.
#'
#' Each artifact (a `list`) has the following attributes (names):
#'
#'   * `id` identifier in the object store; see [storage::object_store]
#'   * `name` original artifact name
#'   * `names` other names for the artifact
#'   * `class` one or more `character` values
#'   * `parents` zero or more identifiers of direct parent artifacts
#'   * `time` creation time
#'   * `description` type-specific text describing the artifact
#'   * `expression` pre-formatted expression that produced the artifact
#'
#' @param id artifact identifier in `store`.
#' @param store Object store; see [storage::object_store].
#' @return An `artifact` object.
#'
#' @importFrom rlang is_scalar_integer
#' @rdname artifact-internal
new_artifact <- function (id, store) {
  tags <- storage::os_read_tags(store, id)
  tags$id <- id

  # expression is stored with the commit
  stopifnot(storage::os_exists(store, tags$parent_commit))
  commit <- storage::os_read_object(store, tags$parent_commit)
  tags$expression <- commit$expr

  # original name as recorded upon time of creation
  if ('plot' %in% tags$class) {
    tags$name <- tags$names <- '<plot>'
  } else {
    i <- match(id, unlist(commit$objects), nomatch = NULL)
    stopifnot(is_scalar_integer(i))
    tags$name <- nth(names(commit$objects), i)
  }

  # cast tags as an artifact DTO
  dto <- as_artifact(tags)

  # attach the store; artifact_data() depends on it
  attr(dto, 'store') <- store
  dto
}


#' @param tags list of tag values that describe an artifact; typically
#'        read with [storage::os_read_tags()].
#'
#' @rdname artifact-internal
as_artifact <- function (tags) {
  stopifnot(utilities::has_name(tags, c('id', 'class', 'parents', 'expression', 'time', 'parent_commit')))

  structure(
    list(
      id          = tags$id,
      name        = tags$name,
      names       = tags$names,
      class       = tags$class,
      time        = tags$time,
      parents     = as.character(tags$parents),
      from        = tags$parent_commit,
      description = description(tags),
      expression  = format_expr(tags$expression, indent = '')
    ),
    class = 'artifact'
  )
}


#' @param x artifact to extract `store` from.
#' @rdname artifact-internal
artifact_store <- function (x) {
  stopifnot(is_artifact(x))
  attr(x, 'store')
}


#' Manipulating and processing artifacts.
#'
#' @param x object to be tested; `artifact` to be processed.
#'
#' @export
#' @rdname artifact
is_artifact <- function (x) inherits(x, 'artifact')


#' @importFrom rlang is_character is_scalar_character
#' @importFrom lubridate is.POSIXt
#' @rdname artifact
artifact_assert_valid <- function (x) {
  stopifnot(is_artifact(x))
  stopifnot(is_scalar_character(x$id))
  stopifnot(is_scalar_character(x$name))
  stopifnot(is_character(x$names))
  stopifnot(is_character(x$class))
  stopifnot(is_character(x$parents))
  stopifnot(is_character(x$description))
  stopifnot(is.POSIXt(x$time))
  stopifnot(is_character(x$from))
  TRUE
}


#' @rdname artifact
is_valid_artifact <- function (x) isTRUE(try(artifact_assert_valid(x), silent = TRUE))


#' @description `artifact_is` answers various questions about an
#' artifact.
#'
#' @param what property of an `artifact` to be tested.
#'
#' @export
#' @rdname artifact
#'
#' @examples
#' \dontrun{
#' artifact_is(a, 'plot')
#' }
artifact_is <- function (x, what) {
  stopifnot(is_artifact(x))

  if (identical(what, 'plot')) return('plot' %in% x$class)

  abort(glue("unsupported value of what: {what}"))
}


#' @description `artifact_data` loads the actual artifact object. The
#' output might be large and thus it is not loaded until requested.
#'
#' @export
#' @rdname artifact
artifact_data <- function (x) storage::os_read_object(artifact_store(x), x$id)


#' Re-plot an archived plot.
#'
#' There are two ways of re-creating the plot. One (`method == "replay"`)
#' is to call [grDevices:replayPlot]. The other (`method == "re-evaluate"`)
#' is to restore the state of R session at the time of plotting and re-run
#' the expression that created the original plot.
#'
#' @param x plot artifact, as returned by [read_artifacts()].
#' @param method `"replay"` or `"re-evaluate"`
#'
#' @importFrom rlang caller_env
#' @importFrom grDevices replayPlot
#'
#' @export
#' @rdname rerun
replot <- function (x, method = 'replay') {
  stopifnot(artifact_is(x, 'plot'))
  stopifnot(method %in% c("replay", "re-evaluate"))

  if (identical(method, "replay")) {
    d <- artifact_data(x)
    suppressMessages({
      replayPlot(d$recordedplot)
    })
  } else {
    parent <- caller_env()
    env <- as_environment(new_commit(x$from, artifact_store(x)), parent)

    expr <- parse(text = x$expression)
    eval(expr, env)
  }
}

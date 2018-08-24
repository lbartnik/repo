
#' Create an artifact DTO.
#'
#' Creates a DTO (data transfer object) that fully describes an artifact
#' from the repository. It is the central object for external (as opposed
#' to internal to this package) processing, printing, etc.
#'
#' @param tags list of tag values that describe an artifact; typically
#'        read with [storage::os_read_tags()].
#' @return An `artifact` object.
#'
#' @importFrom utilities has_name
#' @rdname artifact-internal
as_artifact <- function (tags) {
  stopifnot(has_name(tags, 'id'))

  structure(
    list(
      id    = tags$id,
      class = tags$class
    ),
    class = 'artifact'
  )
}


#' Manipulating and processing artifacts.
#'
#' @param x object to be tested; `artifact` to be processed.
#'
#' @export
#' @rdname artifact
is_artifact <- function (x) inherits(x, 'artifact')


#' @importFrom rlang is_character is_scalar_character
#' @rdname artifact
is_valid_artifact <- function (x) {
  is_artifact(x) &&
    is_scalar_character(x$id) &&
    is_character(x)
}


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

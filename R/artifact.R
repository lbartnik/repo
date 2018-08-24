
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
#' @rdname artifact
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


#' @rdname artifact
is_artifact <- function (x) inherits(x, 'artifact')


#' @importFrom rlang is_character is_scalar_character
#' @rdname artifact
is_valid_artifact <- function (x) {
  is_artifact(x) &&
    is_scalar_character(x$id) &&
    is_character(x)
}

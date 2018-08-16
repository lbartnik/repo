internals <- new.env()


crc32 <- function (x) digest::digest(x, 'crc32')

#' Generate and return an unique session id.
#'
#' Session id is stored via `options()` in order to preserve it regardless
#' of the changes in values coming as input, namely changes in the output
#' of `sessionInfo()`.
#'
#' @importFrom utils sessionInfo
#'
r_session_id <- function () {
  id <- getOption("repository.session_id", default = NULL)
  if (!is.null(id)) return(id)

  id <- crc32(list(Sys.getpid(), sessionInfo()))
  options(repository.session_id = id)

  return(id)
}


current_time <- function () {
  if (!is.null(internals$time_offset)) return(Sys.time() + internals$time_offset)
  Sys.time()
}

remove_class <- function (x, c) {
  class(x) <- setdiff(class(x), c)
  x
}

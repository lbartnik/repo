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
  if (!is.null(simulation$session_id)) return(simulation$session_id)
  crc32(list(Sys.getpid(), sessionInfo()))
}

current_time <- function () {
  offset <- if (!is.null(simulation$time)) simulation$time else 0
  Sys.time() + offset
}

crc32 <- function (x) digest::digest(x, 'crc32')

r_session_id <- function () {
  crc32(list(Sys.getpid(), sessionInfo()))
}


has_name <- function (x, name) isTRUE(all(name %in% names(x)))

# --- access -----------------------------------------------------------

nth <- function(x, n) {
  if (!length(x)) return(vector(mode = typeof(x)))
  x[[n]]
}

last <- function(x) nth(x, length(x))

first <- function(x) nth(x, 1)

second <- function(x) nth(x, 2)


# --- tests ------------------------------------------------------------

is_empty <- function (x) {
  if (is.environment(x)) return(!length(x))
  is.null(x) || is.na(x) || !length(x) || (is.character(x) && !nchar(x))
}

is_error <- function (x) inherits(x, c('error', 'try-error', 'simpleError'))

isFALSE <- function (x) identical(x, FALSE)

is_atomic_class <- function (x) isTRUE(x %in% c("numeric", "character", "integer", "logical", "complex"))

# --- vector -----------------------------------------------------------

choose_data <- function (..., data) {
  args <- list(...)
  if (length(args)) {
    if (length(data)) stop("both ... and data provided when constructing a container")
    return(args)
  }
  data
}

vector <- function (..., data = list()) {
  data <- choose_data(..., data = data)
  proto(expr = {
    values    <- data
    push_back <- function (., value) { .$values <- c(.$values, list(value)) }
    pop_front <- function (.) { ans <- first(.$values); .$values <- .$values[-1]; ans }
    erase     <- function (., value) { .$values <- Filter(function(x)!identical(x,value), .$values) }
    find      <- function (., value) as.logical(match(value, .$values, 0L, 0L))
    size      <- function (.) length(.$values)
    data      <- function (.) .$values
  })
}

map <- function (..., data = list()) {
  data <- choose_data(..., data = data)
  stopifnot(is_all_named(data))

  proto(expr = {
    values <- data
    assign <- function (., key, value) { .$values[[key]] <- value }
    erase  <- function (., key) { .$values[[key]] <- NULL }
    data      <- function (., key = NULL) if (is.null(key)) .$values else .$values[[key]]
  })
}

# --- lists ------------------------------------------------------------

with_names <- function (lst, names) {
  stopifnot(identical(length(lst), length(names)))
  names(lst) <- names
  lst
}

all_named <- function (lst) {
  nms <- names(lst)
  if (is.null(nms)) return(with_names(lst, rep("", length(lst))))
  lst
}

is_all_named <- function (x) {
  all(names(x) != "")
}

combine <- function (...) {
  lsts <- list(...)
  stopifnot(all(vapply(lsts, is.list, logical(1))),
            all(vapply(lsts, is_all_named, logical(1))))

  Reduce(x = lsts, init = list(), function (a, b) {
    c(a, b[setdiff(names(b), names(a))])
  })
}

# --- lapply -----------------------------------------------------------

map_lst <- function (x, f, ...) {
  ans <- lapply(x, f)
  if (!is.null(names(ans))) return(ans)
  names(ans) <- as.character(x)
  ans
}

map_chr <- function (x, f, ...) {
  ans <- lapply(x, f, ...)
  as.character(unlist(ans))
}

map_dbl <- function (x, f, ...) {
  ans <- lapply(x, f, ...)
  as.numeric(unlist(ans))
}

map_int <- function (x, f, ...) {
  ans <- lapply(x, f, ...)
  as.integer(unlist(ans))
}

map_lgl <- function (x, f, ...) {
  ans <- lapply(x, f, ...)
  as.logical(unlist(ans))
}

imap <- function (lst, f, ...) {
  if (!length(lst)) return(list())
  lst <- as.list(lst)

  if (is.null(names(lst))) {
    nms <- seq_along(lst)
  }
  else {
    nms <- names(lst)
  }

  ans <- mapply(value = lst, name = nms, function (value, name) f(value, name, ...),
                SIMPLIFY = FALSE, USE.NAMES = FALSE)
  names(ans) <- nms
  ans
}


not <- function (f) {
  stopifnot(is.function(f))
  function(...) !f(...)
}


# --- ccat -------------------------------------------------------------

#' @importFrom stringi stri_paste
cpaste <- function (..., sep = ' ', default = 'default')
{
  cat_chunk <- function (color, chunk, sep) {
    if (identical(color, 'default') || identical(color, '')) {
      color <- default
    } else {
      color <- get_color(color)
    }
    stri_paste(color(chunk), sep, sep = '')
  }

  grey_style <- crayon::make_style(grDevices::grey(.6), grey = TRUE)
  grey <- function(...) crayon::style(paste0(...), grey_style)

  get_color <- function (color) {
    if (identical(color, "grey")) return(grey)
    get(color, envir = asNamespace("crayon"), inherits = FALSE)
  }

  default <- if (identical(default, 'default')) as.character else get_color(default)
  chunks <- lapply(list(...), stri_paste, collapse = sep)
  if (!length(names(chunks))) names(chunks) <- rep("", length(chunks))

  chunks <- Map(cat_chunk, names(chunks), chunks, c(rep(sep, length(chunks)-1), ''))
  stri_paste(chunks, collapse = '')
}


cat0 <- function (..., sep = '') cat(..., sep = sep)

ccat <- function (..., sep = ' ', default = 'default') cat(cpaste(..., sep = sep, default = default))

ccat0 <- function (..., default = 'default') ccat(..., sep = '', default = default)

#' @importFrom rlang inform
cinform <- function (..., sep = ' ', default = 'default') inform(cpaste(..., sep = sep, default = default))

cinform0 <- function (..., default = 'default') cinform(..., sep = '', default = default)


# --- string -----------------------------------------------------------

join <- function (x, sep) {
  paste(x, collapse = sep)
}

# --- R session --------------------------------------------------------

crc32 <- function (x) digest::digest(x, 'crc32')

r_session_id <- function () {
  crc32(list(Sys.getpid(), sessionInfo()))
}

# --- log & debug ------------------------------------------------------

log <- function (level, ...) {
  ccat0("red", '[', level, '] ', ..., '\n')
}

dbg <- function (...) {
  if (isTRUE(getOption("repository.debug"))) log("DEBUG", ...)
}

guard <- function () {
  x <- sys.call(-1)[[1]]
  fname <- if (is.symbol(x)) deparse(x) else '<unnamed>'
  dbg("-> ", fname, '()')

  parent <- sys.frame(sys.parent(1))
  expr <- substitute(dbg(x), list(x = paste0('<- ', fname, '()')))
  do.call(on.exit, list(expr = expr, add = TRUE), envir = parent)

  invisible()
}

stopif <- function (...) {
  i <- which(map_lgl(list(...), function(x)isTRUE(as.logical(x))))
  if (!length(i)) return(invisible(FALSE))
  mc <- match.call()
  lb <- map_chr(mc[i+1], deparse)
  stop('following conditions are true: ', join(lb, ', '), call. = FALSE)
}

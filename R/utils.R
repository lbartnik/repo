
has_name <- function (x, name) isTRUE(name %in% names(x))

# --- access -----------------------------------------------------------

nth <- function(x, n) {
  if (!length(x)) return(vector(mode = typeof(x)))
  x[[n]]
}

last <- function (x) nth(x, length(x))

first <- function(x) nth(x, 1)


# --- tests ------------------------------------------------------------

is_empty <- function (x) {
  if (is.environment(x)) return(!length(x))
  is.null(x) || is.na(x) || !length(x) || (is.character(x) && !nchar(x))
}

is_error <- function (x) inherits(x, 'try-error') || inherits(x, 'simpleError')

isFALSE <- function (x) identical(x, FALSE)


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
  stopifnot(all_named(data))

  proto(expr = {
    values <- data
    assign <- function (., key, value) { .$values[[key]] <- value }
    erase  <- function (., key) { .$values[[key]] <- NULL }
    data      <- function (., key = NULL) if (is.null(key)) .$values else .$values[[key]]
  })
}

# --- lists ------------------------------------------------------------

all_named <- function (x) {
  all(names(x) != "")
}

combine <- function (...) {
  lsts <- list(...)
  stopifnot(all(vapply(lsts, is.list, logical(1))),
            all(vapply(lsts, all_named, logical(1))))

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

# TODO rename to map_names
napply <- function (lst, f, ...) {
  if (!length(lst)) return(list())

  stopifnot(is.list(lst), all_named(lst))

  ans <- mapply(name = names(lst), value = lst, function (name, value) f(name, value, ...),
                SIMPLIFY = FALSE, USE.NAMES = FALSE)
  names(ans) <- names(lst)
  ans
}

with_names <- function (lst, names) {
  stopifnot(identical(length(lst), length(names)))
  names(lst) <- names
  lst
}

not <- function (f) {
  stopifnot(is.function(f))
  function(...) !f(...)
}


# --- ccat -------------------------------------------------------------

cat0 <- function (..., sep = '') cat(..., sep = sep)

ccat <- function (color, ..., sep = ' ')
{
  if (identical(color, 'default'))
    cat(..., sep = sep)
  else {
    color <- get(color, envir = asNamespace("crayon"), inherits = FALSE)
    cat(color(paste(..., sep = sep)))
  }
}

ccat0 <- function (color, ...) ccat(color, ..., sep = '')

ccat_ <- function (chunks, sep = ' ')
{
  mapply(color = names(chunks), chunk = chunks,
         function (color, chunk)
         {
           ccat0(color, paste(chunk, collapse = sep))
         })
}


# --- string -----------------------------------------------------------

join <- function (x, sep) {
  paste(x, collapse = sep)
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


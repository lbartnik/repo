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
      tags$parents <- .$last_commit$ids()[names]
      tags
    })
  }

  u$process_plot <- function (.) {
    .$svg <- plot_as_svg(.$plot)

    # if the current plot looks the same as the last one, do not update at all
    if (is.null(.$svg) || svg_equal(.$svg, .$last_plot)) {
      .$plot_id <- NA_character_
      return()
    }

    .$plot_id <- storage::compute_id(.$svg)
    if (storage::os_exists(.$store, .$plot_id)) {
      dbg("plot already present")
      return()
    }

    .$plot_tags <- auto_tags(.$svg, class = 'plot')
    names <- extract_parents(env, expr)
    .$plot_tags$parents <- .$last_commit$ids()[names]
  }

  u$introduced_changes <- function (.) {
    ia <- .$ids
    ib <- .$last_commit$ids()

    sorted_names <- function(x) if (is.null(names(x))) character() else sort(names(x))
    an <- sorted_names(ia)
    bn <- sorted_names(ib)

    return(!identical(ia[an], ib[bn]) || (!is.null(.$svg) && !svg_equal(.$svg, .$last_plot)))
  }

  u$write <- function (.) {
    napply(new, function (name, id) {
      dbg("artifact `", name, "` not present, storing [", id, "]")
      storage::os_write(.$store, .$objects[[name]], id = id, tags = .$tags[[name]])
    })

    if (!is.na(.$plot_id)) {
      dbg("storing new plot [", id, "] with parents: ", paste(parents, collapse = ", "))
      storage::os_write(.$store, .$svg, id = .$plot_id, tags = .$plot_tags)
    }

    # TODO write commit
  }

  u$sync_repo <- function (.) {
    # TODO synchronize changes into repo (.super)
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

  combine(preset, list(class = class(obj), time = Sys.time()))
}


#' Returns a base64-encoded, SVG plot.
#'
#' @param pl Plot recorded by [recordPlot()].
#' @return `character` string, base64-encoded SVG plot.
#' @import jsonlite
#'
plot_as_svg <- function (pl)
{
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
#' @rdname repository_append
#'
strip_object <- function (obj)
{
  if (is.symbol(obj)) return(obj)

  # TODO should we disregard any environment?
  if (is.environment(obj)) return(emptyenv())

  attrs <- if (!is.null(attributes(obj))) lapply(attributes(obj), strip_object)

  if (is.list(obj)) {
    obj_tmp <- lapply(obj, strip_object)
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


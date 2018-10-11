#' Work with a sample artifact repository.
#'
#' @description Obtain a handle to a read/write copy of an artifact
#' repository installed with the package.
#'
#' @return a [repository] object.
#'
#' @name samples
#' @rdname samples
NULL

#' @details `iris_models` shows a simple modeling excercise with the
#' [datasets::iris] data set.
#'
#' @export
#' @rdname samples
iris_models <- function () {
  source_path <- system.file('iris-models/', package = 'repository')
  rw_copy(source_path)
}

#' @details `london_meters` documents a simple analysis of a subset of
#' the __London meters__ data; see the
#' [Kaggle website](https://www.kaggle.com/jeanmidev/smart-meters-in-london)
#' for more details on this data set.
#'
#' @export
#' @rdname samples
london_meters <- function () {
  source_path <- system.file('london-meters/', package = 'repository')
  rw_copy(source_path)
}


rw_copy <- function (source_path) {
  target_path <- file.path(tempdir(TRUE), basename(source_path))

  if (!dir.exists(target_path)) {
    dir.create(dirname(target_path), recursive = TRUE, showWarnings = FALSE)
    file.copy(source_path, dirname(target_path), recursive = TRUE)
  }

  repository(storage::filesystem(target_path, create = FALSE))
}



simulate_iris <- function (repo, .silent = TRUE) {
  run_simulation(system.file('scripts/iris.R', package = 'repository'), repo, .silent)
}

simulate_london_meters <- function (repo, .silent = TRUE) {
  run_simulation(system.file('scripts/london-meters.R', package = 'repository'), repo, .silent)
}

#' @importFrom rlang parse_exprs
run_simulation <- function (script_path, repo, .silent) {
  workspace <- session_simulator(repo, .silent = .silent)
  exprs <- parse_exprs(file(script_path))

  lapply(exprs, function (expr) {
    workspace$run_quoted(expr)
    simulation_offset_time(60)
  })

  invisible(NULL)
}




# --- simulation & meta API --------------------------------------------

#' @import proto
#' @importFrom grDevices dev.off recordPlot
#' @importFrom utils capture.output
#' @importFrom rlang inform
#'
session_simulator <- function (repo, .silent = TRUE) {

  try(dev.off(), silent = TRUE)
  parent_env <- parent.frame(1)

  g <- proto(expr = {
    repo = NULL
    silent = TRUE
    session = new.env(parent = parent_env)
  })

  g$repo <- repo
  g$silent <- .silent

  g$inform <- function (., msg) {
    if (isFALSE(.silent)) rlang::inform(msg)
  }

  g$contents <- function (.) {
    as.list(.$session)
  }

  g$run <- function (., expr) {
    expr <- substitute(expr)
    .$run_quoted(expr)
  }

  g$run_quoted <- function (., expr) {
    if (is_meta_command(expr)) {
      .$inform(glue("meta command: {first(deparse(expr))}"))

      # replace function name and add `repo` and `env` arguments
      expr[[1]] <- as.symbol(extract_meta_command(expr))
      expr$repo <- .$repo
      expr$env  <- .$session

      eval(expr)
    } else {
      inform(glue("evaluating: {first(deparse(expr))}"))

      # print is necessary for graphics, but we don't want to see the
      # output on the console, thus - print and capture at the same time
      eval_expr <- substitute(print(expr), list(expr = expr))

      if (isTRUE(.silent)) {
        capture.output(eval(eval_expr, .$session, enclos = baseenv()))
      } else {
        eval(eval_expr, .$session, enclos = baseenv())
      }

      plot <- tryCatch(recordPlot(), error = function(e)'error')
      if (identical(plot, 'error')) plot <- NULL

      repository_update(repo, .$session, plot, expr)
    }
  }

  g
}

is_meta_command <- function (expr) {
  if (!is.call(expr)) return(FALSE)

  fun_spec <- first(expr)
  is.call(fun_spec) && identical(length(fun_spec), 3L) &&
    identical(first(fun_spec), quote(`::`)) && identical(second(fun_spec), quote(meta))
}

extract_meta_command <- function (expr) {
  paste0('simulation_meta_', as.character(nth(first(expr), 3)))
}


simulation_meta_state <- new.env()

simulation_meta_set <- function (name, value, ...) {
  assign(name, value, envir = simulation_meta_state)
}

simulation_meta_get <- function (name, ...) {
  get(name, envir = simulation_meta_state)
}

simulation_meta_unset <- function (names, ...) {
  rm(list = names, envir = simulation_meta_state)
}

simulation_meta_commit_remember <- function (repo, ...) {
  simulation_meta_state$last_commit_id <- repo$last_commit$id
}

simulation_meta_commit_restore <- function (repo, env, ...) {
  repository_rewind(repo, simulation_meta_state$last_commit_id)
  commit_checkout(commit(repo$store, simulation_meta_state$last_commit_id), env)
}

simulation_meta_offset_time <- function (value, ...) {
  time <- simulation_meta_get('time')
  if (!is.null(time)) {
    value <- value + time
  }
  simulation_meta_set('time', value)
}

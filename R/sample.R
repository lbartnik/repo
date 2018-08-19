simulation <- new.env()

simulation_set <- function (name, value, ...) {
  assign(name, value, envir = simulation)
}

simulation_get <- function (name, ...) {
  get(name, envir = simulation)
}

simulation_unset <- function (names, ...) {
  rm(list = names, envir = simulation)
}

simulation_commit_remember <- function (repo, ...) {
  simulation$last_commit_id <- repo$last_commit$id
}

simulation_commit_restore <- function (repo, env, ...) {
  repository_rewind(repo, simulation$last_commit_id)
  commit_checkout(commit(repo$store, simulation$last_commit_id), env)
}

simulation_offset_time <- function (value, ...) {
  time <- simulation_get('time')
  if (!is.null(time)) {
    value <- value + time
  }
  simulation_set('time', value)
}


#' @importFrom stringi stri_startswith_fixed
is_simulation_cmd <- function (expr) {
  if (!is.call(expr)) return(FALSE)
  name <- as.character(first(expr))
  stri_startswith_fixed(name, 'simulation_')
}



#' @import proto
#' @importFrom grDevices dev.off recordPlot
#' @importFrom utils capture.output
#' @importFrom rlang inform
#'
R_session_simulator <- function (repo, .silent = TRUE) {

  try(dev.off(), silent = TRUE)
  parent_env <- parent.frame(1)

  g <- proto(expr = {
    repo = NULL
    session = new.env(parent = parent_env)
  })
  g$repo <- repo

  g$run <- function (., expr) {
    expr <- substitute(expr)
    .$run_quoted(expr)
  }

  g$run_quoted <- function (., expr) {
    if (is_simulation_cmd(expr)) {
      inform(sprintf("simulation command: %s", first(deparse(expr))))
      .$run_simulation_cmd(expr)
    } else {
      inform(sprintf("evaluating: %s", first(deparse(expr))))

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

      repository::repository_update(repo, .$session, plot, expr)
    }
  }

  g$run_simulation_cmd <- function (., expr) {
    expr$repo <- .$repo
    expr$env  <- .$session
    eval(expr)
  }

  g$contents <- function (.) {
    return(as.list(.$session))
  }

  g
}


# --- simple sequence --------------------------------------------------

# Suppress checks in `simulate_modelling`.
utils::globalVariables(c('iris'))


#' @param repo Repository to write to.
#'
#' @rdname simulations
generate_simple <- function (repo)
{
  workspace <- R_session_simulator(repo)

  workspace$run(x <- stats::lm(Sepal.Width ~ Sepal.Length, iris))
  workspace$run(iris2 <- iris)
  workspace$run(iris2$Sepal.Length <- iris2$Sepal.Length ** 2)
  workspace$run(y <- stats::lm(Sepal.Width ~ Sepal.Length, iris2))
}


# --- London meters sequece --------------------------------------------


#' Simulations and examples.
#'
#' These functions populate sessions' history cache with a complete
#' history of data exploration.
#'
#' @description `simulate_london_meters` loads and examines a subset
#' of __London meters__ data; see the [Kaggle website](https://www.kaggle.com/jeanmidev/smart-meters-in-london)
#' for this data set and the introductory vignette.
#'
#' @rdname simulations
#' @importFrom rlang parse_exprs
#' @import utilities
#'
simulate_london_meters <- function (repo, .silent = TRUE)
{
  workspace <- R_session_simulator(repo, .silent = .silent)
  exprs <- parse_exprs(file(system.file('scripts/london-meters.R', package = 'repository')))

  lapply(exprs, function (expr) {
    workspace$run_quoted(expr)
    simulation_offset_time(60)
  })

  # TODO split between two sessions

  invisible()
}


#' Work with a sample artifact repository.
#'
#' Returns a handle to a read/write copy of an artifact repository
#' installed with the package.
#'
#' @export
sample_repository <- function () {
  source_path <- system.file('sample-repository/', package = 'repository')
  target_path <- file.path(tempdir(TRUE), basename(source_path))

  if (!dir.exists(target_path)) {
    dir.create(dirname(target_path), recursive = TRUE, showWarnings = FALSE)
    file.copy(source_path, dirname(target_path), recursive = TRUE)
  }

  repository(storage::filesystem(target_path, create = FALSE))
}

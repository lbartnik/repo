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


#' @import proto
#' @importFrom grDevices dev.off recordPlot
#' @importFrom utils capture.output
#'
R_session_simulator <- function (repo) {

  try(dev.off(), silent = TRUE)
  parent_env <- parent.frame(1)

  g <- proto(expr = {
    repo = NULL
    session = new.env(parent = parent_env)
  })
  g$repo <- repo

  g$run <- function (., expr) {
    expr <- substitute(expr)
    cat("evaluating: ", deparse(expr)[[1]], "...\n")

    # print is necessary for graphics, but we don't want to see the
    # output on the console, thus - print and capture at the same time
    eval_expr <- substitute(print(expr), list(expr = expr))
    capture.output(eval(eval_expr, .$session, enclos = baseenv()))

    plot <- tryCatch(recordPlot(), error = function(e)'error')
    if (identical(plot, 'error')) plot <- NULL

    repository::repository_update(repo, .$session, plot, expr)
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

# Suppress checks in `simulate_london_meters`.
utils::globalVariables(c('LCLid', 'tstp', 'energy_kWh', 'meter', 'timestamp', 'usage', 'dow', 'hour'))

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
#'
simulate_london_meters <- function (repo)
{
  old_id <- options("repository.session_id")
  on.exit(options(old_id), add = TRUE)
  on.exit(internals$time_offset <- NULL)

  workspace <- R_session_simulator(repo)

}

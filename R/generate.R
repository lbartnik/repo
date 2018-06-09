#' @import proto
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


# Suppress checks in `simulate_modelling`.
utils::globalVariables(c('iris'))


generate_simple <- function (repo)
{
  workspace <- R_session_simulator(repo)

  workspace$run(x <- stats::lm(Sepal.Width ~ Sepal.Length, iris))
  workspace$run(iris2 <- iris)
  workspace$run(iris2$Sepal.Length <- iris2$Sepal.Length ** 2)
  workspace$run(y <- stats::lm(Sepal.Width ~ Sepal.Length, iris2))
}

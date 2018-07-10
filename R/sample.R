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
#' of __London meters__ data; see [energy] and the introductory vignette.
#'
#' @param overwrite If current stash contains objects, setting `overwrite`
#'        to `TRUE` will remove them prior to running simulation.
#'
#' @rdname simulations
#'
simulate_london_meters <- function (repo)
{
  requireNamespace('dplyr', quietly = TRUE)
  requireNamespace('lubridate', quietly = TRUE)
  requireNamespace('magrittr', quietly = TRUE)
  requireNamespace('ggplot2', quietly = TRUE)
  requireNamespace('stats', quietly = TRUE)
  requireNamespace('readr', quietly = TRUE)

  # silence R CMD check
  `%<>%` <- magrittr::`%<>%`
  `%>%` <- magrittr::`%>%`

  workspace <- R_session_simulator(repo)

  workspace$run(
    input <-
      system.file("extdata/block_62.csv", package = "repository") %>%
      readr::read_csv(na = 'Null') %>%
      dplyr::rename(meter = LCLid, timestamp = tstp, usage = `energy_kWh`) %>%
      dplyr::filter(meter %in% c("MAC004929", "MAC000010", "MAC004391"),
                    lubridate::year(timestamp) == 2013)
  )

  # remember the commit id so that later we can come back to this point in history
  go_back <- '2b67f4934da0aa3baecfdd3001008539217d5719'
  workspace$run(
    input %<>%
      dplyr::mutate(timestamp = lubridate::floor_date(timestamp, 'hours')) %>%
      dplyr::group_by(meter, timestamp) %>%
      dplyr::summarise(usage = sum(usage))
  )

  # dplyr adds attributes to objects when filter is called
  # it's probably some kind of smart pre-computed cache but
  # it messes up object tracking
  #
  # if filter is not a separate step, use subset() instead of
  # filter() to maintain the same object id between commits
  workspace$run(
    input %<>% dplyr::filter(meter == "MAC004929")
  )

  workspace$run(
    with(input, plot(timestamp, usage, type = 'p', pch = '.'))
  )

  workspace$run(
    x <-
      input %>%
      dplyr::mutate(hour = lubridate::hour(timestamp),
                    dow  = lubridate::wday(timestamp, label = TRUE)) %>%
      dplyr::mutate_at(dplyr::vars(hour, dow), dplyr::funs(as.factor)) %>%
      dplyr::group_by(hour, dow) %>%
      dplyr::summarise(usage = mean(usage, na.rm = TRUE))
  )

  workspace$run(
    with(x, plot(hour, usage))
  )

  workspace$run(
    ggplot2::ggplot(x) +
      ggplot2::geom_point(ggplot2::aes(x = hour, y = usage)) +
      ggplot2::facet_wrap(~dow)
  )

  workspace$run(
    x <-
      input %>%
      dplyr::mutate(hour = lubridate::hour(timestamp),
                    dow  = lubridate::wday(timestamp)) %>%
      dplyr::mutate_at(dplyr::vars(hour, dow), dplyr::funs(as.factor))
  )

  workspace$run(
    ggplot2::ggplot(x) +
      ggplot2::geom_boxplot(ggplot2::aes(x = hour, y = usage)) +
      ggplot2::facet_wrap(~dow)
  )

  workspace$run(
    m <- stats::lm(usage ~ hour:dow, x)
  )

  res <- repo %>% select(object) %>% filter(id == go_back) %>% execute
  message('Restoring object ', go_back)
  workspace$session$input <- res$object[[1]]

  # now try a different house
  workspace$run(
    input %<>% dplyr::filter(meter == "MAC000010")
  )

  workspace$run(
    x <-
      input %>%
      dplyr::mutate(hour = lubridate::hour(timestamp),
                    dow  = lubridate::wday(timestamp)) %>%
      dplyr::mutate_at(dplyr::vars(hour, dow), dplyr::funs(as.factor))
  )

  workspace$run(
    ggplot2::ggplot(x) +
      ggplot2::geom_boxplot(ggplot2::aes(x = hour, y = usage)) +
      ggplot2::facet_wrap(~dow)
  )

  # go back again, and try the third house
  message('Restoring object ', go_back)
  workspace$session$input <- res$object[[1]]

  workspace$run(
    input %<>% dplyr::filter(meter == "MAC004391")
  )

  workspace$run(
    x <-
      input %>%
      dplyr::mutate(hour = lubridate::hour(timestamp),
                    dow  = lubridate::wday(timestamp)) %>%
      dplyr::mutate_at(dplyr::vars(hour, dow), dplyr::funs(as.factor))
  )

  workspace$run(
    ggplot2::ggplot(x) +
      ggplot2::geom_boxplot(ggplot2::aes(x = hour, y = usage)) +
      ggplot2::facet_wrap(~dow)
  )
}

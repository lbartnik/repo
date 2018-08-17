requireNamespace('dplyr', quietly = TRUE)
requireNamespace('lubridate', quietly = TRUE)
requireNamespace('magrittr', quietly = TRUE)
requireNamespace('ggplot2', quietly = TRUE)
requireNamespace('stats', quietly = TRUE)
requireNamespace('readr', quietly = TRUE)

# silence R CMD check
`%<>%` <- magrittr::`%<>%`
`%>%` <- magrittr::`%>%`

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
internals$time_offset <- 60

# dplyr adds attributes to objects when filter is called
# it's probably some kind of smart pre-computed cache but
# it messes up object tracking
#
# if filter is not a separate step, use subset() instead of
# filter() to maintain the same object id between commits
workspace$run(
  input %<>% dplyr::filter(meter == "MAC004929")
)
internals$time_offset <- 120

workspace$run(
  with(input, plot(timestamp, usage, type = 'p', pch = '.'))
)
internals$time_offset <- 180

workspace$run(
  x <-
    input %>%
    dplyr::mutate(hour = lubridate::hour(timestamp),
                  dow  = lubridate::wday(timestamp, label = TRUE)) %>%
    dplyr::mutate_at(dplyr::vars(hour, dow), dplyr::funs(as.factor)) %>%
    dplyr::group_by(hour, dow) %>%
    dplyr::summarise(usage = mean(usage, na.rm = TRUE))
)
internals$time_offset <- 240

workspace$run(
  with(x, plot(hour, usage))
)
internals$time_offset <- 300

workspace$run(
  ggplot2::ggplot(x) +
    ggplot2::geom_point(ggplot2::aes(x = hour, y = usage)) +
    ggplot2::facet_wrap(~dow)
)
internals$time_offset <- 360

workspace$run(
  x <-
    input %>%
    dplyr::mutate(hour = lubridate::hour(timestamp),
                  dow  = lubridate::wday(timestamp)) %>%
    dplyr::mutate_at(dplyr::vars(hour, dow), dplyr::funs(as.factor))
)
internals$time_offset <- 420

workspace$run(
  ggplot2::ggplot(x) +
    ggplot2::geom_boxplot(ggplot2::aes(x = hour, y = usage)) +
    ggplot2::facet_wrap(~dow)
)
internals$time_offset <- 480

workspace$run(
  m <- stats::lm(usage ~ hour:dow, x)
)
internals$time_offset <- 540

# new R session
options(repository.session_id = crc32("new R session"))
internals$time_offset <- 7200 # 2 hours later

res <- repo %>% select(object) %>% filter(id == go_back) %>% execute
message('Restoring object ', go_back)
workspace$session$input <- res$object[[1]]

# now try a different house
workspace$run(
  input %<>% dplyr::filter(meter == "MAC000010")
)
internals$time_offset <- 7260

workspace$run(
  x <-
    input %>%
    dplyr::mutate(hour = lubridate::hour(timestamp),
                  dow  = lubridate::wday(timestamp)) %>%
    dplyr::mutate_at(dplyr::vars(hour, dow), dplyr::funs(as.factor))
)
internals$time_offset <- 7320

workspace$run(
  ggplot2::ggplot(x) +
    ggplot2::geom_boxplot(ggplot2::aes(x = hour, y = usage)) +
    ggplot2::facet_wrap(~dow)
)
internals$time_offset <- 7380

# go back again, and try the third house
message('Restoring object ', go_back)
workspace$session$input <- res$object[[1]]

workspace$run(
  input %<>% dplyr::filter(meter == "MAC004391")
)
internals$time_offset <- 7440

workspace$run(
  x <-
    input %>%
    dplyr::mutate(hour = lubridate::hour(timestamp),
                  dow  = lubridate::wday(timestamp)) %>%
    dplyr::mutate_at(dplyr::vars(hour, dow), dplyr::funs(as.factor))
)
internals$time_offset <- 7500

workspace$run(
  ggplot2::ggplot(x) +
    ggplot2::geom_boxplot(ggplot2::aes(x = hour, y = usage)) +
    ggplot2::facet_wrap(~dow)
)

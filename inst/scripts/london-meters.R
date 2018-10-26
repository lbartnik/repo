# predefined session id
meta::set('session_id', crc32("1st R session"))

library(dplyr)
library(lubridate)
library(magrittr)
library(ggplot2)

# 6.
library(stats)
library(readr)

input <-
  system.file("extdata/block_62.csv", package = "repository") %>%
  readr::read_csv(na = 'Null') %>%
  dplyr::rename(meter = LCLid, timestamp = tstp, usage = `energy_kWh`) %>%
  dplyr::filter(meter %in% c("MAC004929", "MAC000010", "MAC004391"),
                lubridate::year(timestamp) == 2013)

hourly <- input %>%
  dplyr::mutate(timestamp = lubridate::floor_date(timestamp, 'hours')) %>%
  dplyr::group_by(meter, timestamp) %>%
  dplyr::summarise(usage = sum(usage))


# dplyr adds attributes to objects when filter is called
# it's probably some kind of smart pre-computed cache but
# it messes up object tracking
#
# if filter is not a separate step, use subset() instead of
# filter() to maintain the same object id between commits

# 11.
meter_4929 <- hourly %>% subset(meter == "MAC004929")

with(meter_4929, plot(timestamp, usage, type = 'p', pch = '.'))

x <-
  meter_4929 %>%
  dplyr::mutate(hour = lubridate::hour(timestamp),
                dow  = lubridate::wday(timestamp, label = TRUE)) %>%
  dplyr::mutate_at(dplyr::vars(hour, dow), dplyr::funs(as.factor)) %>%
  dplyr::group_by(hour, dow) %>%
  dplyr::summarise(usage = mean(usage, na.rm = TRUE))

# also calling plot triggers dplyr's C++ code and alters the tibble,
# in this case: x
with(x, plot(hour, usage))

ggplot2::ggplot(x) +
  ggplot2::geom_point(ggplot2::aes(x = hour, y = usage)) +
  ggplot2::facet_wrap(~dow)

x <-
  meter_4929 %>%
  dplyr::mutate(hour = lubridate::hour(timestamp),
                dow  = lubridate::wday(timestamp)) %>%
  dplyr::mutate_at(dplyr::vars(hour, dow), dplyr::funs(as.factor))

# 16.
ggplot2::ggplot(x) +
  ggplot2::geom_boxplot(ggplot2::aes(x = hour, y = usage)) +
  ggplot2::facet_wrap(~dow)

m <- stats::lm(usage ~ hour:dow, x)

meta::set('session_id', crc32("2nd R session"))
meta::offset_time(7200)

# 21.
meter_0010 <- hourly %>% subset(meter == "MAC000010")

x <-
  meter_0010 %>%
  mutate(hour = hour(timestamp),
         dow  = wday(timestamp)) %>%
  mutate_at(vars(hour, dow), funs(as.factor))

ggplot(x) +
  geom_boxplot(aes(x = hour, y = usage)) +
  facet_wrap(~dow)


meter_4391 <- hourly %>% subset(meter == "MAC004391")

# 26.
x <-
  meter_4391 %>%
  mutate(hour = hour(timestamp),
         dow  = wday(timestamp)) %>%
  mutate_at(vars(hour, dow), funs(as.factor))

ggplot(x) +
  geom_boxplot(aes(x = hour, y = usage)) +
  facet_wrap(~dow)

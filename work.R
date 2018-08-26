library(repository)

options(repository.debug = TRUE)
options(repository.debug = FALSE)

path  <- file.path(getwd(), 'tmp-repo')

path  <- file.path(getwd(), '../ui/repository/')
unlink(path, recursive = TRUE)

store <- storage::filesystem(path, create = TRUE)
repo  <- repository::repository(store)

generate_simple(repo)
simulate_london_meters(repo)

repo %>% select(id, object) %>% filter('input' %in% names) %>% execute

repo %>% select(object) %% filter(id == '2b67f4934da0aa3baecfdd3001008539217d5719')


repo <- sample_repository()
repo %>% select(-artifact) %>% filter(isTRUE(artifact)) %>% execute


a <- read_artifacts(as_artifacts(sample_repository()))


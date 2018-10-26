library(repository)

options(repository.debug = TRUE)
options(repository.debug = FALSE)
options(ui.track = FALSE)
options(ui.pick_branch = FALSE)

path  <- file.path(getwd(), 'tmp-repo')

path  <- file.path(getwd(), '../ui/repository/')
unlink(path, recursive = TRUE)

store <- storage::filesystem(path, create = TRUE)
repo  <- repository::repository(store)

simulate_iris(repo)
simulate_london_meters(repo)

x <- read_commits(as_commits(repo))

new_commit('13b2c2164cb69ed8f1922d141cc0deca73e04f18', store)

x <- read_commits(filter(as_commits(repo), ancestor_of('13b2c216')))


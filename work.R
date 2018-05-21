options(repository.debug = TRUE)

path  <- file.path(getwd(), 'repository')
store <- storage::filesystem(path, create = TRUE)
repo  <- repository::repository(store)

plot <- tryCatch(recordPlot(), error = function(e)'error')


repository_update(repo, globalenv(), plot, bquote(a <- 1))

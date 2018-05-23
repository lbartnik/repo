options(repository.debug = TRUE)

path  <- file.path(getwd(), 'tmp-repo')
store <- storage::filesystem(path, create = FALSE)
repo  <- repository::repository(store)

#plot <- tryCatch(recordPlot(), error = function(e)'error')
#repository_update(repo, globalenv(), plot, bquote(a <- 1))

h <- repository_history(repo)

storage::os_read_tags(repo$store, 'd8db7af5fca4437d45c7b5d09705cb077614caf9')

zzz <- lapply(ancestors(h, 'd8db7af5fca4437d45c7b5d09705cb077614caf9'), function (commit) {
  cat(deparse(commit$expr), "\n")
})

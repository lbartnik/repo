options(repository.debug = TRUE)

path  <- file.path(getwd(), 'tmp-repo')
store <- storage::filesystem(path, create = FALSE)
repo  <- repository::repository(store)

#plot <- tryCatch(recordPlot(), error = function(e)'error')
#repository_update(repo, globalenv(), plot, bquote(a <- 1))

h <- repository_history(repo)

step_over(h, function (commit) {
  cat(deparse(commit$expr), "\n")
})

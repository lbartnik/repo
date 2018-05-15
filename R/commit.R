commit <- function (repo, object_ids, plot_id, expr, parent_id)
{
  c <- proto::proto(expr = {
    repo        = repo
    object_data = list()
    object_ids  = object_ids
    plot_id     = plot_id
    expr        = expr
    parent_id   = parent_id

    ids     = function (.) .$object_ids
    objects = commit_objects
    write   = commit_write
  })

}


commit_objects <- function (.)
{
  if (!identical(length(.$object_data), length(.$object_ids))) {
    .$object_data <- lapply(.$object_ids, function (id) {
      storage::os_read_object(.$repo$store, id)
    })
  }

  .$object_data
}



#' Write commit to an object store.
#'
#' An assumption is made that all objects are already stored and only
#' the commit itself needs to be written to the object store.
#'
#' @param store An object store, e.g. [storage::filesystem].
#' @param commit A [commit] object.
#'
commit_write <- function (.)
{
  if (!is.na(.$id)) return(.$id)

  hash_data <- list(objects = .$object_ids, plot_id = .$plot_id, expr = .$expr,
                    parent_id = .@parent_id)

  .$id <- storage::compute_id(hash_data)

  # this should never happen because hash is computed from both objects
  # and parent id; if it does happen, something is SERIOUSLY broken
  if (storage::os_exists(.$store, .$id)) {
    stop("commit already exists, aborting")
  }

  # update objects' tags: set parent commit id
  lapply(.$object_ids, function (id) {
    tags <- storage::os_read_tags(.$store, id)
    if ('commit' %nin% names(tags)) {
      tags$commit <- .$id
      storage::os_update_tags(.$store, id, tags)
    }
  })

  # store list of object pointers + basic 'history' tags
  storage::os_write(store, list(objects = .$object_ids, plot = .$plot_id, expr = .$expr),
                    tags = list(class = 'commit', parent = .$parent_id),
                    id = commit$id)
}



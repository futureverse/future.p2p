#' Gets the default name of the P2P cluster
#'
#' @param users Users to have access to the cluster. This controls whether
#' the default cluster names should be "personal" or "friends".
#'
#' @param \ldots Passed as-is to [pico_username()].
#'
#' @return
#' `p2p_cluster_name()` returns R option `future.p2p.cluster`, if set.
#' If not set, it returns `{pico_name}/personal` if `length(users) == 0`,
#' otherwise `{pico_name}/friends`.
#' 
#' @export
p2p_cluster_name <- function(users = character(0), ...) {
  users <- unique(users)
  name <- getOption("future.p2p.cluster")
  if (is.null(name)) {
    users <- setdiff(users, pico_username(...))
    name <- if (length(users) == 0) "personal" else "friends"
    name <- sprintf("%s/%s", pico_username(...), name)
  }
  name
}


#' Gets the identifier of the current P2P client or P2P worker
#'
#' @return
#' `p2p_client_id()` and `p2p_worker_id()` return the client and worker
#' identifier, which both have format`"{username}@{hostname}:{pid}"`.
#'
#' @keywords internal
p2p_client_id <- local({
  name <- NULL
  function(...) {
    if (is.null(name)) {
      user <- pico_username(...)
      hostname <- Sys.info()[["nodename"]]
      pid <- Sys.getpid()
      name <<- sprintf("%s@%s:%d", user, hostname, pid)
    }
    name
  }
})

#' @rdname p2p_client_id
#' @keywords internal
p2p_worker_id <- p2p_client_id

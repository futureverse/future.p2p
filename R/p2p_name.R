#' Gets the default name of the P2P cluster
#'
#' @param users Users to have access to the cluster. This controls whether
#' the default cluster names should be "personal" or "friends".
#'
#' @return
#' `p2p_cluster()` returns R option `future.p2p.cluster`, if set.
#' If not set, it returns `{pico_name}/personal` if `length(users) == 0`,
#' otherwise `{pico_name}/friends`.
#' 
#' @export
p2p_cluster <- function(users = character(0)) {
  users <- unique(users)
  name <- getOption("future.p2p.cluster")
  if (is.null(name)) {
    users <- setdiff(users, pico_username())
    name <- if (length(users) == 0) "personal" else "friends"
    name <- sprintf("%s/%s", pico_username(), name)
  }
  name
}


#' Gets the default name of the client or worker on the P2P cluster
#'
#' @return
#' `p2p_name()` returns the client or worker name of format
#' `"{username}@{hostname}:{pid}"`.
#'
#' @rdname p2p_cluster
#' @export
p2p_name <- local({
  name <- NULL
  function() {
    if (is.null(name)) {
      user <- pico_username()
      hostname <- Sys.info()[["nodename"]]
      pid <- Sys.getpid()
      name <<- sprintf("%s@%s:%d", user, hostname, pid)
    }
    name
  }
})

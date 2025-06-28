#' Gets the default name of the P2P cluster
#'
#' @return
#' `p2p_cluster()` return the cluster name, if set, otherwise `"p2p"`.
#' 
#' @export
p2p_cluster <- function() {
  name <- getOption("future.p2p.cluster")
  if (is.null(name)) {
    name <- "p2p"
  }
  name
}


#' Gets the default name of the client or worker on the P2P cluster
#'
#' @return
#' `p2p_name()` returns the client or worker name, if set, otherwise
#' `"{username}@{hostname}:{pid}"`.
#'
#' @rdname p2p_cluster
#' @export
p2p_name <- function() {
  name <- getOption("future.p2p.name")
  if (is.null(name)) {
    user <- Sys.info()[["user"]]
    hostname <- Sys.info()[["nodename"]]
    pid <- Sys.getpid()
    name <- sprintf("%s@%s:%d", user, hostname, pid)
  }
}

#' Gets the default name of the client or worker on the P2P cluster
#'
#' @return
#' Returns `{username}@{hostname}:{pid}`.
#' 
#' @export
p2p_name <- function() {
  user <- Sys.info()[["user"]]
  hostname <- Sys.info()[["nodename"]]
  pid <- Sys.getpid()
  sprintf("%s@%s:%d", user, hostname, pid)
}

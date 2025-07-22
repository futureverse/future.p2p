#' Gets the default name of the P2P cluster
#'
#' @return
#' `p2p_cluster()` return R option `future.p2p.cluster`, if set,
#' otherwise `{pico_name}/mycluster`.
#' 
#' @export
p2p_cluster <- function() {
  name <- getOption("future.p2p.cluster")
  if (is.null(name)) {
    name <- sprintf("%s/%s", pico_username(), "mycluster")
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

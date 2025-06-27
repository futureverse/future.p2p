#' Launches a P2P worker and adds it to a P2P cluster
#'
#' @param channel The p2p cluster to contribute to.
#'
#' @param user The name of the worker as publicized on the P2P cluster.
#' The default name is `{username}@{hostname}:{pid}`.
#'
#' @examplesIf interactive()
#' pico_p2p_worker()
#'
#' @section Sequential, single-core processing by default:
#' A P2P worker runs sequentially (`plan(sequential)`) and is configured
#' to with a single CPU core to prevent nested parallelization.
#'
#' @importFrom future resolve plan sequential
#' @export
pico_p2p_worker <- function(channel = "chat", user = pico_user()) {
  old_opts <- options(parallelly.availableCores.fallback = 1L)
  on.exit(options(old_opts))
  with(plan(sequential), local = TRUE)

  message(sprintf("[worker] connect worker %s to p2p cluster %s", sQuote(user), sQuote(channel)))
  p <- pico_pipe(channel, user = user)

  message("[worker] hello")
  m <- pico_hello(p, type = "worker")

  repeat {
    message("[worker] wait for request")
    m <- pico_wait_for(p, type = "request")
    
    message("[worker] offer to work")
    pico_take_on_future(p, to = m$from, future = m$future)

    message("[worker] wait for accept")
    m <- pico_wait_for(p, type = "accept", futures = m$future)

    if (m[["to"]] == user) {
      message("[worker] receive future from worker")
      res <- pico_receive_future(p, via = m$via)
      f <- res[["future"]]
  
      message(sprintf("[worker] processing future"))
      r <- tryCatch(result(f), error = identity)
  
      message(sprintf("[worker] send future result"))
      res <- pico_send_result(p, future = f, via = m$via)
    }
 } ## repeat()
} ## pico_p2p_worker()

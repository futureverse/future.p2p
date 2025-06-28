#' Launches a P2P worker and adds it to a P2P cluster
#'
#' @param cluster The p2p cluster to contribute to.
#'
#' @param name The name of the worker as publicized on the P2P cluster.
#'
#' @param duration Duration (in seconds) to offer working on futures.
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
pico_p2p_worker <- function(cluster = p2p_cluster(), name = p2p_name(), duration = 60*60) {
  old_opts <- options(parallelly.availableCores.fallback = 1L)
  on.exit(options(old_opts))
  with(plan(sequential), local = TRUE)

  now <- Sys.time()
  duration <- as.numeric(duration)
  expires <- now + duration
  duration <- difftime(duration, 0)

  info("connect worker %s to p2p cluster %s for %s until %s", sQuote(name), sQuote(cluster), format(duration), expires)
  p <- pico_pipe(cluster, user = name)

  repeat {
    now <- Sys.time()
    if (now > expires) {
      info("times out")
      break
    }
    
    info("hello")
    m <- pico_hello(p, type = "worker", expires = expires)
  
    info("wait for request")
    m <- pico_wait_for(p, type = "request")
    client <- m$from
    
    info("offer to work for %s", sQuote(client))
    pico_take_on_future(p, to = client, future = m$future)

    info("wait for accept")
    m <- pico_wait_for(p, type = "accept", futures = m$future)

    if (m[["to"]] == name) {
      info("receive future from %s", sQuote(client))
      res <- pico_receive_future(p, via = m$via)
      f <- res[["future"]]
  
      info("process future %s:%s", sQuote(client), sQuoteLabel(f))
      dt <- system.time({
        r <- tryCatch(result(f), error = identity)
      })
      dt <- difftime(dt[3], 0)

      info("send future result to %s after %s processing", sQuote(client), format(dt))
      res <- pico_send_result(p, future = f, via = m$via)
    }
 } ## repeat()
 info("exiting ...")
} ## pico_p2p_worker()


## Expose function on the CLI
cli_fcn(pico_p2p_worker) <- c("--(cluster)=(.*)", "--(name)=(.*)", "--(duration)=([[:digit:]]+)")


info <- function(fmtstr, ..., time = Sys.time(), timefmt = "%T", from = c("worker", "client")) {
  from <- match.arg(from)
  msg <- sprintf(fmtstr, ...)
  msg <- sprintf("%s [%s] %s", format(time, format = timefmt), from, msg)
  message(msg)
}


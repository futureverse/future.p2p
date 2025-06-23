#' Launches a Pico P2P Worker
#'
#' @examplesIf interactive()
#' pico_p2p_worker()
#'
#' @importFrom future resolve plan sequential
#' @export
pico_p2p_worker <- function(channel = "chat", user = pico_user()) {
  with(plan(sequential), local = TRUE)

  message("[worker] connect to pico message queue")
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
    str(m)
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

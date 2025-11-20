#' Gets your pico.sh username
#'
#' @inheritParams pico_pipe
#'
#' @param timeout (numeric scalar) Maximum number of seconds before giving up.
#'
#' @return
#' A character string or a timeout error.
#'
#' @export
pico_username <- local({
  username <- NULL
  
  function(host = "pico.sh", ssh_args = NULL, timeout = 10.0) {
    if (is.null(username)) {
      debug <- isTRUE(getOption("future.p2p.debug"))
      if (debug) {
        mdebug_push("pico_username() ...")
        mdebugf("Host: %s", sQuote(host))
        mdebugf("Timeout: %s seconds", timeout)
        mstr(list(ssh_args = ssh_args))
        mstr(list(ssh_args = ssh_args))
        on.exit({
          mdebug_pop()
        })
      }

      ## Special case
      if (grepl("^pipe[.]", host)) {
        host <- sub("^pipe[.]", "", host)
      }
      
      ssh_config <- list(options = ssh_args, host = host)
      args <- c(ssh_config[["options"]], ssh_config[["host"]], "user")
      if (debug) {
        mdebug("SSH call:")
        mstr(list(args = args))
      }

      out <- system2("ssh", args = args, stdout = TRUE, stderr = TRUE, timeout = timeout)
      if (debug) {
        mdebug("SSH result:")
        mstr(list(out = out))
      }
      status <- attr(out, "status")
      if (!is.null(status)) {
        stop(sprintf("Failed to infer pico.sh username. Exit code %s", status))
      }
      if (length(out) < 3) {
        stop(sprintf("pico_username(): Received unexpected results: [n=%s]\n%s", length(out), paste(out, collapse = "\n")))
      }
      username <<- structure(out[1], id = out[2], created_on = as_POSIXct(sub("T", " ", out[3])), class = "pico_username")
    }

    username
  }
})

## Expose function on the CLI
cli_fcn(pico_username) <- character(0L)


#' @export
print.pico_username <- function(x, ...) {
  cat(x, "\n", sep = "")
}

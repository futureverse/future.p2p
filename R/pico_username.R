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
  
  function(host = "pipe.pico.sh", ssh_args = NULL, timeout = 10.0) {
    if (is.null(username)) {
      ## (1) Launch a temporary pico pipe with a random name
      topic <- session_uuid()
      p_pipe <- pico_pipe(topic, command = "pipe", args = "-r", host = host, ssh_args = ssh_args)
      on.exit(tryCatch(pico_terminate(p_pipe), error = identity))
      p <- p_pipe$process
      msg <- sprintf("msg=%s", topic)
      
      ## (2) Wait for pico pipe to be up
      t_max <- proc.time()[3] + timeout
      pico_send_message(p_pipe, message = msg)
      repeat {
        bfr <- pico_receive_message(p_pipe)
        if (length(bfr) > 0L) {
          if (any(bfr == msg)) break
        }
        if (proc.time()[3] > t_max) {
          stop("Failed to identity pico.sh username - echo failed")
        }
        Sys.sleep(0.1)
      }
      
      ## (3) Get info pico pipe
      t_max <- proc.time()[3] + timeout
      pattern <- sprintf(".*[[:blank:]]([^/]+)/%s:.*", topic)
      while (is.null(username)) local({
        p_ls <- pico_pipe(command = "ls", host = host, ssh_args = ssh_args)
        on.exit(tryCatch(pico_terminate(p_ls), error = identity))
        p <- p_ls$process
        if (length(bfr <- p$read_all_output_lines()) > 1L) {
          line <- grep(pattern, bfr, value = TRUE)
          if (length(line) == 1L) {
            username <<- gsub(pattern, "\\1", line)
            return(username)
          }
        }
        if (proc.time()[3] > t_max) {
          stop("Failed to identity pico.sh username - ls failed")
        }
        Sys.sleep(0.1)
      })
    }

    structure(username, class = "pico_username")
  }
})

## Expose function on the CLI
cli_fcn(pico_username) <- character(0L)


#' @export
print.pico_username <- function(x, ...) {
  cat(x, "\n", sep = "")
}

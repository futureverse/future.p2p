#' Connect to a pico pipe
#'
#' @param topic (character string) The topic to connect to. If connecting
#' to another users topic, prefix with their username and a forward slash,
#' e.g. `alice/topic`.
#'
#' @param command (character string) Type of pipe or command.
#'
#' @param args (character vector) Optional arguments.
#'
#' @param host (character string) The hostname serving the pico service.
#'
#' @param p A `pico_pipe` object.
#'
#' @param message (character string) A message to send.
#'
#' @param newline (logical) If TRUE, a newline (LF; `\n`) will be appended
#' to the message, otherwise not.
#'
#' @param n (integer) Number of messages to read.
#'
#' @param ... (character vector; optional) Named attributes that are recorded
#' with the returned object.
#'
#' @return
#' A [processx::process] of clas `pico_pipe`.
#'
#' @seealso
#' This function relies on the <https://pico.sh> services.
#'
#' @importFrom processx process
#' @export
pico_pipe <- function(topic = NULL, command = c("pipe", "pub", "sub", "ls", "help"), args = c(), host = "pipe.pico.sh", ...) {
  command <- match.arg(command)
  if (command %in% c("pipe", "pub", "sub")) {
    stopifnot(length(topic) == 1L, is.character(topic), !is.na(topic), nzchar(topic))
  }

  attrs <- list(...)
  nattrs <- length(attrs)
  if (nattrs > 0) {
    names <- names(attrs)
    if (is.null(names) || !all(nzchar(names))) {
      stop("All argumnets must be named")
    }
  }
  
  args <- c(host, command, topic, args)
  env <- new.env(parent = emptyenv())
  for (name in names) {
    env[[name]] <- attrs[[name]]
  }
  env$process <- process$new("ssh", args = args, stdin = "|", stdout = "|", stderr = "|")
  class(env) <- c("pico_pipe", "pico", class(env))
  env
}

#' @export
print.pico <- function(x, ...) {
  cat(sprintf("%s:\n", class(x)[1]))
  names <- names(x)
  names <- setdiff(names, "process")
  for (name in names) {
    cat(sprintf("  %s=%s\n", name, x[[name]]))
  }
  print(x$process)
}

#' @rdname pico_pipe
#' @export
pico_terminate <- function(p, ...) {
  stopifnot(inherits(p, "pico_pipe"))
  p$process$kill()
}

#' @rdname pico_pipe
#' @export
pico_send_message <- function(p, message, newline = TRUE, ...) {
  stopifnot(inherits(p, "pico_pipe"))
  stopifnot(length(message) == 1L, is.character(message), !is.na(message))
  if (newline) message <- paste(message, "\n", sep = "")
  p$process$write_input(message)
}

#' @rdname pico_pipe
#' @export
pico_receive_message <- function(p, n = 1L, ...) {
  stopifnot(inherits(p, "pico_pipe"))
  stopifnot(length(n) == 1L, is.numeric(n), !is.na(n), n > 0L)
  p$process$read_output_lines(n)
}

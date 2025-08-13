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
#' @param ssh_args (character vector) Optional SSH arguments.
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
#' @param \ldots (character vector; optional) Named attributes that are recorded
#' with the returned object.
#'
#' @return
#' A [processx::process] of class `pico_pipe`.
#'
#' @seealso
#' This function relies on the <https://pico.sh> services.
#'
#' @importFrom processx process
pico_pipe <- function(topic = NULL, command = c("pipe", "pub", "sub", "ls", "help"), args = c(), host = "pipe.pico.sh", ssh_args = NULL, ...) {
  command <- match.arg(command)
  if (command %in% c("pipe", "pub", "sub")) {
    stopifnot(length(topic) == 1L, is.character(topic), !is.na(topic), nzchar(topic))
  }
  
  debug <- isTRUE(getOption("future.p2p.debug"))
  if (debug) {
    mdebug_push("pico_pipe() ...")
    mdebugf("Topic: %s", sQuote(topic))
    mdebugf("Command: %s", sQuote(command))
    mdebugf("Host: %s", sQuote(host))
    mstr(list(args = args))
    mstr(list(ssh_args = ssh_args))
    on.exit({
      mdebug_pop()
    })
  }
  
  attrs <- list(...)
  nattrs <- length(attrs)
  names <- names(attrs)
  if (nattrs > 0) {
    if (is.null(names) || !all(nzchar(names))) {
      stop("All arguments must be named")
    }
  }

  ssh_config <- list(options = ssh_args, host = host, command = command, topic = topic, args = args)
  
  env <- new.env(parent = emptyenv())
  for (name in names) {
    env[[name]] <- attrs[[name]]
  }

  args <- c(ssh_config[["options"]], ssh_config[["host"]], ssh_config[["command"]], ssh_config[["topic"]], ssh_config[["args"]])
  
  if (debug) {
    mdebug("SSH call:")
    mstr(list(args = args))
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
pico_terminate <- function(p, ...) {
  stopifnot(inherits(p, "pico_pipe"))
  p$process$kill()
}

#' @rdname pico_pipe
pico_send_message <- function(p, message, newline = TRUE, ...) {
  stopifnot(inherits(p, "pico_pipe"))
  stopifnot(length(message) == 1L, is.character(message), !is.na(message))
  if (newline) message <- paste(message, "\n", sep = "")
  p$process$write_input(message)
}


#' @param df (data.frame) Data frame to send as a message.
#'
#' @rdname pico_pipe
pico_send_message_dataframe <- function(p, df) {
  msg <- unlist(df, use.names = TRUE)
  msg <- sprintf("%s=%s", names(msg), msg)
  msg <- paste(msg, collapse = ",")
  pico_send_message(p, msg)
  attr(df, "message") <- msg
  invisible(df)
}


#' @rdname pico_pipe
pico_receive_message <- function(p, n = 1L, ...) {
  stopifnot(inherits(p, "pico_pipe"))
  stopifnot(length(n) == 1L, is.numeric(n), !is.na(n), n > 0L)
  p$process$read_output_lines(n)
}


#' @param pattern (character string; optional) A regular expression so scan for.
#'
#' @rdname pico_pipe
pico_receive_message_dataframe <- function(p, ..., pattern = NULL) {
  msg <- pico_receive_message(p, ...)
  
  ## Filter lines by regular expression?
  if (!is.null(pattern)) msg <- grep(pattern, msg, value = TRUE)
  
  ## No matching message recieved?
  if (length(msg) == 0) return(NULL)

  ## Parse as a dataframe
  parts <- strsplit(msg, split = ",", fixed = TRUE)
  parts <- lapply(parts, FUN = function(x) {
    x <- unlist(strsplit(x, split = "=", fixed = TRUE))
    names <- x[seq(from = 1L, to  = length(x), by = 2L)]
    value <- x[seq(from = 2L, to  = length(x), by = 2L)]
    names(value) <- names
    as.data.frame(as.list(value))
  })
  
  do.call(rbind, parts)  
}


#' @param timeout (numeric scalar) Timeout (in seconds).
#'
#' @rdname pico_pipe
pico_hosted_channels <- function(host = "pipe.pico.sh", ssh_args = NULL, timeout = 10.0) {
  username <- pico_username()
  t_max <- proc.time()[3] + timeout
  pattern_1 <- sprintf(".*[[:blank:]]%s/([^:]+):", username)
  pattern_2 <- sprintf(".*[[:blank:]]%s/([^:]+):[[:blank:]]+[(]Access List:[[:blank:]]+(.*)[)]", username)
  channels <- NULL
  while (is.null(channels)) local({
    p_ls <- pico_pipe(command = "ls", host = host, ssh_args = ssh_args)
    on.exit(tryCatch(pico_terminate(p_ls), error = identity))
    p <- p_ls$process
    bfr <- p$read_all_output_lines()
    if (length(bfr) >= 1L) {
      if (length(bfr) == 1L && bfr == "no pubsub channels found") {
        channels <<- data.frame(name = character(0L), users = character(0L))
        return(channels)
      } else {
        lines <- grep(pattern_2, bfr, value = TRUE)
        if (length(lines) >= 1L) {
          names <- gsub(pattern_2, "\\1", lines)
          users <- gsub(pattern_2, "\\2", lines)
          users <- gsub("[[:blank:]]+", "", users)
          channels <<- data.frame(name = names, users = users)
          return(channels)
        } else {
          lines <- grep(pattern_1, bfr, value = TRUE)
          if (length(lines) >= 1L) {
            names <- gsub(pattern_1, "\\1", lines)
            users <- ""
            channels <<- data.frame(name = names, users = users)
            return(channels)
          }
        }
      }
      if (proc.time()[3] > t_max) {
        stop(sprintf("Failed to identity %s channels", host))
      }
      Sys.sleep(0.1)
    }
  })
  channels
}

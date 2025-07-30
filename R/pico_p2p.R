pico_time <- function(time = Sys.time(), delta = 0) {
  as.integer(time + delta)
}

now_str <- function(when = pico_time()) {
  when <- as.POSIXct(when)
  format(when, format = "%FT%T")
}

pico_send_message_dataframe <- function(p, df) {
  msg <- unlist(df, use.names = TRUE)
  msg <- sprintf("%s=%s", names(msg), msg)
  msg <- paste(msg, collapse = ",")
  pico_send_message(p, msg)
  attr(df, "message") <- msg
  invisible(df)
}

#' @export
future_id <- function(future, ...) {
  if (inherits(future, "Future")) {
    id <- paste(future[["uuid"]], collapse = "-")
  } else if (is.character(future)) {
    id <- attr(future, "future_id")
  }
  stopifnot(length(id) == 1L, is.character(id), nzchar(id))
  id
}

#' @export
pico_next_message <- function(p, ...) {
  msg <- pico_receive_message(p)
  ## ignore messages not starting with 'when=...'
  msg <- grep("^when=", msg, value = TRUE)
  if (length(msg) == 0) return(NULL)
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

#' @export
pico_hello <- function(p, from = p$user, type = c("worker", "client", "cluster"), expires = NULL, duration = 60*60, ...) {
  type <- match.arg(type)
  
  debug <- isTRUE(getOption("future.p2p.debug"))
  if (debug) {
    mdebug_push("pico_hello() ...")
    mdebugf("Type: %s", type)
    mdebugf("Duration: %g seconds", duration)
    on.exit({
      mdebug_pop()
    })
  }

  if (is.null(expires)) {
    expires <- Sys.time() + duration
  }

  m <- data.frame(
    when = now_str(),
    expires = pico_time(expires),
    type = type,
    from = from
  )
  pico_send_message_dataframe(p, m)
}

pico_expired <- function() {
  data.frame(
    when = now_str(),
    type = "expired"
  )
}

#' @export
pico_wait_for <- function(p, type, futures = NULL, expires = NULL, duration = 60, delay = 0.1, ...) {
  if (is.null(expires)) {
    expires <- Sys.time() + duration
  }
  
  debug <- isTRUE(getOption("future.p2p.debug"))
  if (debug) {
    mdebugf_push("pico_wait_for(type = '%s') ...", type)
    mdebugf("Pooling frequency: %g seconds", delay)
    mdebugf("Waiting for futures: [n=%d] %s", length(futures), commaq(futures))
    mdebugf("Expires: %s", now_str(expires))
    on.exit({
      mdebug_pop()
    })
  }
  
  repeat {
    m <- pico_next_message(p)
    
    ## Expired?
    if (Sys.time() > expires) {
      if (debug) mdebug("Times out")
      return(pico_expired())
    }
    
    if (!is.null(m) && m$type == type) {
      if (is.null(futures) || m$future %in% futures) break
    }
    Sys.sleep(0.1)
  }
  
  m
}


#' @importFrom utils file_test
#' @export
pico_have_future <- function(p, future, duration = getOption("future.p2p.duration.request", 60), from = p$user, ...) {
  debug <- isTRUE(getOption("future.p2p.debug"))
  if (debug) {
    mdebug_push("pico_have_future() ...")
    mdebugf("Future: %s", future_id(future))
    mdebugf("Duration: %g seconds", duration)
    on.exit({
      mdebug_pop()
    })
  }
  
  if (inherits(future, "Future")) {
    file <- tempfile(fileext = ".rds")
    on.exit(file.remove(file), add = TRUE)
    saveRDS(future, file = file)
  } else if (is.character(future)) {
    stopifnot(file_test("-f", future))
    file <- future
  } else {
    stop("Unknown type of argument 'future': ", sQuote(typeof(future)))
  }

  stopifnot(length(from) == 1L, is.character(from), nzchar(from))

  size <- file.size(file)
  if (debug) mdebugf("Size: %g bytes", size)
  
  m <- data.frame(
    when = now_str(),
    expires = pico_time(delta = duration),
    type = "request",
    from = from,
    future = future_id(future),
    size = size
  )
  
  pico_send_message_dataframe(p, m)
}

#' @export
pico_take_on_future <- function(p, to, future, duration = 60, from = p$user, ...) {
  debug <- isTRUE(getOption("future.p2p.debug"))
  if (debug) {
    mdebug_push("pico_take_on_future() ...")
    mdebugf("Future: %s", future)
    mdebugf("Duration: %g seconds", duration)
    on.exit({
      mdebug_pop()
    })
  }
  
  stopifnot(length(future) == 1L, is.character(future), nzchar(future))
  stopifnot(length(to) == 1L, is.character(to), nzchar(to))
  stopifnot(length(from) == 1L, is.character(from), nzchar(from))

  m <- data.frame(
    when = now_str(),
    expires = pico_time(delta = duration),
    type = "offer",
    from = from,
    to = to,
    future = future
  )

  pico_send_message_dataframe(p, m)
}

via_channel <- function() {
  digits <- sample.int(16L, size = 17L, replace = TRUE) %% 16
  digits[1:4] <- digits[1:4] %% 10
  digits <- as.hexmode(digits)
  digits <- as.character(digits)
  digits[5] <- "-"
  paste(digits, collapse = "")
}


#' @export
pico_send_future <- function(p, future, to, via = via_channel(), duration = 60, from = p$user, ...) {
  debug <- isTRUE(getOption("future.p2p.debug"))
  if (debug) {
    mdebug_push("pico_send_future() ...")
    mdebugf("Future: %s", future_id(future))
    mdebugf("To: %s", to)
    mdebugf("Via: %s", via)
    mdebugf("Duration: %g seconds", duration)
    on.exit({
      mdebug_pop()
    })
  }

  if (inherits(future, "Future")) {
    file <- file.path(tempdir(), sprintf("%s-Future.rds", m$future))
    saveRDS(future, file = file)
    on.exit(file.remove(file))
  } else if (is.character(future)) {
    stopifnot(file_test("-f", future))
    file <- future
  } else {
    stop("Unknown type of argument 'future': ", sQuote(typeof(future)))
  }

  stopifnot(length(to) == 1L, is.character(to), nzchar(to))
  stopifnot(length(via) == 1L, is.character(via), nzchar(via))
  stopifnot(length(from) == 1L, is.character(from), nzchar(from))

  m <- data.frame(
    when = now_str(),
    expires = pico_time(delta = duration),
    type = "accept",
    from = from,
    to = to,
    future = future_id(future),
    via = via
  )

  m_res <- pico_send_message_dataframe(p, m)
  w_res <- wormhole_send(file, code = sprintf("%s-f", m$via))
  m_res
}


#' @export
pico_receive_future <- function(p, via, duration = 60) {
  debug <- isTRUE(getOption("future.p2p.debug"))
  if (debug) {
    mdebug_push("pico_receive_future() ...")
    mdebugf("Via: %s", via)
    mdebugf("Duration: %g seconds", duration)
    on.exit({
      mdebug_pop()
    })
  }
  
  stopifnot(length(via) == 1, is.character(via), !is.na(via), nzchar(via))
  code <- sprintf("%s-f", via)
  tf <- wormhole_receive(code)
  future <- readRDS(tf)
  on.exit(file.remove(tf))
  list(
    future = future,
    via = via
  )
}  


#' @importFrom future result
#' @export
pico_send_result <- function(p, future, via, duration = 60) {
  debug <- isTRUE(getOption("future.p2p.debug"))
  if (debug) {
    mdebug_push("pico_send_result() ...")
    mdebugf("Future: %s", future_id(future))
    mdebugf("Via: %s", via)
    mdebugf("Duration: %g seconds", duration)
    on.exit({
      mdebug_pop()
    })
  }
  
  file <- file.path(tempdir(), sprintf("%s-FutureResult.rds", future_id(future)))
  r <- result(future)
  saveRDS(r, file = file)
  code <- sprintf("%s-r", via)
  res <- wormhole_send(file, code = code)
  invisible(res)
}


#' @export
pico_receive_result <- function(p, via, duration = 60, path = tempdir()) {
  debug <- isTRUE(getOption("future.p2p.debug"))
  if (debug) {
    mdebug_push("pico_receive_result() ...")
    mdebugf("Via: %s", via)
    mdebugf("Duration: %g seconds", duration)
    mdebugf("Path: %s", path)
    on.exit({
      mdebug_pop()
    })
  }
  
  stopifnot(length(via) == 1, is.character(via), !is.na(via), nzchar(via))
  code <- sprintf("%s-r", via)
  file <- wormhole_receive(code, path = path)
  file
}


#' @export
pico_hosted_channels <- function(host = "pipe.pico.sh", ssh_args = NULL, timeout = 10.0) {
  username <- pico_username()
  t_max <- proc.time()[3] + timeout
  pattern <- sprintf(".*[[:blank:]]%s/([^:]+):[[:blank:]]+[(]Access List:[[:blank:]]+(.*)[)]", username)
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
        lines <- grep(pattern, bfr, value = TRUE)
        if (length(lines) >= 1L) {
          names <- gsub(pattern, "\\1", lines)
          users <- gsub(pattern, "\\2", lines)
          users <- gsub("[[:blank:]]+", "", users)
          channels <<- data.frame(name = names, users = users)
          return(channels)
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


#' @export
pico_hosted_clusters <- function(host = "pipe.pico.sh", ssh_args = NULL, timeout = 10.0) {
  clusters <- pico_hosted_channels(host, ssh_args = ssh_args, timeout = timeout)
  keep <- grep("/future.p2p$", clusters$name)
  clusters <- clusters[keep, ]
  clusters$name <- sub("/future.p2p$", "", clusters$name)
  clusters
}

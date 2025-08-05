pico_p2p_next_message <- function(p, ...) {
  pico_receive_message_dataframe(p, ..., pattern = "^when=")
}


pico_p2p_time <- function(time = Sys.time(), delta = 0) {
  as.integer(time + delta)
}

now_str <- function(when = pico_p2p_time()) {
  when <- as.POSIXct(when)
  format(when, format = "%FT%T")
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
pico_p2p_hello <- function(p, from = p$user, type = c("worker", "client", "cluster"), expires = NULL, duration = 60*60, ...) {
  type <- match.arg(type)
  
  debug <- isTRUE(getOption("future.p2p.debug"))
  if (debug) {
    mdebug_push("pico_p2p_hello() ...")
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
    expires = pico_p2p_time(expires),
    type = type,
    from = from
  )
  pico_send_message_dataframe(p, m)
}

pico_p2p_expired <- function() {
  data.frame(
    when = now_str(),
    type = "expired"
  )
}

#' @export
pico_p2p_wait_for <- function(p, type, futures = NULL, expires = NULL, duration = 60, delay = 0.1, ...) {
  if (is.null(expires)) {
    expires <- Sys.time() + duration
  }
  
  debug <- isTRUE(getOption("future.p2p.debug"))
  if (debug) {
    mdebugf_push("pico_p2p_wait_for(type = '%s') ...", type)
    mdebugf("Pooling frequency: %g seconds", delay)
    mdebugf("Waiting for futures: [n=%d] %s", length(futures), commaq(futures))
    mdebugf("Expires: %s", now_str(expires))
    on.exit({
      mdebug_pop()
    })
  }
  
  repeat {
    m <- pico_p2p_next_message(p)
    
    ## Expired?
    if (Sys.time() > expires) {
      if (debug) mdebug("Times out")
      return(pico_p2p_expired())
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
pico_p2p_have_future <- function(p, future, duration = getOption("future.p2p.duration.request", 60), from = p$user, ...) {
  debug <- isTRUE(getOption("future.p2p.debug"))
  if (debug) {
    mdebug_push("pico_p2p_have_future() ...")
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
    expires = pico_p2p_time(delta = duration),
    type = "request",
    from = from,
    future = future_id(future),
    size = size,
    protocols = paste(transfer_protocols(), collapse = ";")
  )
  
  pico_send_message_dataframe(p, m)
}

#' @export
pico_p2p_take_on_future <- function(p, to, future, duration = 60, from = p$user, ...) {
  debug <- isTRUE(getOption("future.p2p.debug"))
  if (debug) {
    mdebug_push("pico_p2p_take_on_future() ...")
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
    expires = pico_p2p_time(delta = duration),
    type = "offer",
    from = from,
    to = to,
    future = future,
    protocols = paste(transfer_protocols(), collapse = ";")
  )

  pico_send_message_dataframe(p, m)
}

#' @export
pico_p2p_send_future <- function(p, future, to, via = via_transfer_uri(), duration = 60, from = p$user, ...) {
  debug <- isTRUE(getOption("future.p2p.debug"))
  if (debug) {
    mdebug_push("pico_p2p_send_future() ...")
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
  stopifnot(length(from) == 1L, is.character(from), nzchar(from))
  stopifnot(length(via) == 1L, is.character(via), nzchar(via))

  m <- data.frame(
    when = now_str(),
    expires = pico_p2p_time(delta = duration),
    type = "accept",
    from = from,
    to = to,
    future = future_id(future),
    via = via
  )

  m_res <- pico_send_message_dataframe(p, m)

  uri <- parse_transfer_uri(via)
  if (uri$protocol == "wormhole") {
    w_res <- wormhole_send(file, code = sprintf("%s-f", uri$path))
  }
  
  m_res
}


#' @export
pico_p2p_receive_future <- function(p, via, duration = 60) {
  debug <- isTRUE(getOption("future.p2p.debug"))
  if (debug) {
    mdebug_push("pico_p2p_receive_future() ...")
    mdebugf("Via: %s", via)
    mdebugf("Duration: %g seconds", duration)
    on.exit({
      mdebug_pop()
    })
  }
  
  stopifnot(length(via) == 1, is.character(via), !is.na(via), nzchar(via))
  
  uri <- parse_transfer_uri(via)
  if (uri$protocol == "wormhole") {
    tf <- wormhole_receive(code = sprintf("%s-f", uri$path))
  }

  future <- readRDS(tf)
  on.exit(file.remove(tf))
  list(
    future = future,
    via = via
  )
}  


#' @importFrom future result
#' @export
pico_p2p_send_result <- function(p, future, via, duration = 60) {
  debug <- isTRUE(getOption("future.p2p.debug"))
  if (debug) {
    mdebug_push("pico_p2p_send_result() ...")
    mdebugf("Future: %s", future_id(future))
    mdebugf("Via: %s", via)
    mdebugf("Duration: %g seconds", duration)
    on.exit({
      mdebug_pop()
    })
  }

  uri <- parse_transfer_uri(via)
  if (uri$protocol == "wormhole") {
    file <- file.path(tempdir(), sprintf("%s-FutureResult.rds", future_id(future)))
    r <- result(future)
    saveRDS(r, file = file)
    res <- wormhole_send(file, code = sprintf("%s-r", uri$path))
  }

  invisible(res)
}


#' @export
pico_p2p_receive_result <- function(p, via, duration = 60, path = tempdir()) {
  debug <- isTRUE(getOption("future.p2p.debug"))
  if (debug) {
    mdebug_push("pico_p2p_receive_result() ...")
    mdebugf("Via: %s", via)
    mdebugf("Duration: %g seconds", duration)
    mdebugf("Path: %s", path)
    on.exit({
      mdebug_pop()
    })
  }
  
  uri <- parse_transfer_uri(via)
  if (uri$protocol == "wormhole") {
    file <- wormhole_receive(code = sprintf("%s-r", uri$path), path = path)
  }
  
  file
}


#' @export
pico_p2p_hosted_clusters <- function(host = "pipe.pico.sh", ssh_args = NULL, timeout = 10.0) {
  clusters <- pico_hosted_channels(host, ssh_args = ssh_args, timeout = timeout)
  keep <- grep("/future.p2p$", clusters$name)
  clusters <- clusters[keep, ]
  clusters$name <- sub("/future.p2p$", "", clusters$name)
  clusters
}

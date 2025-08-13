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

future_id <- function(future, ...) {
  if (inherits(future, "Future")) {
    id <- paste(future[["uuid"]], collapse = "-")
  } else if (is.character(future)) {
    id <- attr(future, "future_id")
  }
  stopifnot(length(id) == 1L, is.character(id), nzchar(id))
  id
}


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
    from = from,
    protocols = paste(supported_transfer_protocols(), collapse = ";")
  )
  pico_send_message_dataframe(p, m)
}

pico_p2p_expired <- function() {
  data.frame(
    when = now_str(),
    type = "expired"
  )
}


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
    protocols = paste(supported_transfer_protocols(), collapse = ";")
  )
  
  pico_send_message_dataframe(p, m)
}

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
    protocols = paste(supported_transfer_protocols(), collapse = ";")
  )

  pico_send_message_dataframe(p, m)
}

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


pico_p2p_hosted_clusters <- function(host = "pipe.pico.sh", ssh_args = NULL, timeout = 10.0) {
  clusters <- pico_hosted_channels(host, ssh_args = ssh_args, timeout = timeout)
  keep <- grep("/future.p2p$", clusters$name)
  clusters <- clusters[keep, ]
  clusters$name <- sub("/future.p2p$", "", clusters$name)
  clusters
}



#' @importFrom callr r_bg
#' @importFrom utils file_test
pico_p2p_dispatch_future <- function(future) {
  send_future <- function(topic, name, host = host, ssh_args = ssh_args, future_id, file, to, via, duration) {
    pico <- future.p2p:::pico_pipe(topic, user = name, host = host, ssh_args = ssh_args)
    m <- future.p2p:::pico_p2p_hello(pico, type = "client")

    ## 2. Announce future
    repeat {
      m1 <- future.p2p:::pico_p2p_have_future(pico, future = file, duration = duration)
      m2 <- future.p2p:::pico_p2p_wait_for(pico, type = "offer", futures = m1[["future"]], expires = m1[["expires"]])
      if (m2[["type"]] != "expired") break
    }

    ## 3. Send future to workers
    worker <- m2[["from"]]
    stopifnot(is.character(worker), nzchar(worker))
    m3 <- future.p2p:::pico_p2p_send_future(pico, future = file, to = worker, via = via)

    ## 4. Remove temporary file
    file.remove(file)

    ## 5. Wait for and receive FutureResult file
    path <- file.path(dirname(dirname(file)), "results")
    tryCatch({
      file <- future.p2p:::pico_p2p_receive_result(pico, via = via, path = path)
    }, interrupt = function(int) {
      cat(file = "foo.log", "interrupted\n")
    })

    invisible(file)
  }

  debug <- isTRUE(getOption("future.p2p.debug"))
  if (debug) {
    mdebug_push("dispatch_future()...")
    on.exit(mdebugf_pop())
  }

  ## Get backend
  backend <- future[["backend"]]
  stopifnot(inherits(backend, "FutureBackend"))

  cluster <- backend[["cluster"]]
  name <- backend[["name"]]
  host <- backend[["host"]]
  ssh_args <- backend[["ssh_args"]]
  via <- via_transfer_uri()
  
  ## 1. Put future on the dispatcher queue
  void <- p2p_dir("results")
  future[["file"]] <- saveFuture(future, path = p2p_dir("queued"))
  
  if (debug) mdebugf("File: %s", sQuote(future[["file"]]))

  ## 1. Connect to pico and say hello
  cluster_owner <- dirname(cluster)
  if (cluster_owner == pico_username()) {
    topic <- sprintf("%s/future.p2p", basename(cluster))
  } else {
    topic <- sprintf("%s/future.p2p", cluster)
  }

  args <- list(
    topic = topic,
    name = name,
    host = host,
    ssh_args = ssh_args,
    future_id = future_id(future),
    file = future[["file"]],
    via = via,
    duration = getOption("future.p2p.duration.request", 10.0)
  )
  if (debug) {
    mstr(args)
  }

  rx <- r_bg(send_future, args = args, supervise = TRUE)
  future[["rx"]] <- rx

  future[["pico_via"]] <- via

  ## Update future state
  future[["state"]] <- "running"
  
  invisible(future)
}

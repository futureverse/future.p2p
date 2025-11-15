#' Launches a P2P worker and adds it to a P2P cluster
#'
#' @inheritParams pico_pipe
#'
#' @param cluster The p2p cluster to contribute to.
#'
#' @param duration Duration (in seconds) to offer working on futures.
#'
#' @examplesIf interactive()
#' ## Start a P2P cluster worker
#' future.p2p::worker()
#'
#' @section Sequential, single-core processing by default:
#' A P2P worker runs sequentially (`plan(sequential)`) and is configured
#' to with a single CPU core to prevent nested parallelization.
#'
#' @importFrom processx poll
#' @importFrom utils head
#' @export
worker <- function(cluster = p2p_cluster_name(host = host, ssh_args = ssh_args), host = "pipe.pico.sh", ssh_args = NULL, duration = 60*60) {
  parts <- strsplit(cluster, split = "/", fixed = TRUE)[[1]]
  if (length(parts) != 2L) {
    stop(sprintf("Argument 'cluster' must be of format '{owner}/{name}': %s", sQuote(cluster)))
  }
  
  debug <- isTRUE(getOption("future.p2p.debug"))
  if (debug) {
    mdebug_push("future.p2p::worker() ...")
    on.exit({
      mdebugf_pop()
    })
  }

  options(future.p2p.info.from = "worker coordinator")
  
  if (inherits(duration, "ssh_args")) {
    ## e.g. ssh_args = "-J dt1"
    ssh_args <- strsplit(ssh_args, split = " ", fixed = TRUE)[[1]]
  }

  if (inherits(duration, "cmd_arg")) {
    duration <- as.numeric(duration)
  }

  info("install 'wormhole', if missing")
  bin <- find_wormhole()

  info("assert connection to p2p cluster %s", sQuote(cluster))
  worker_id <- p2p_worker_id()
  if (!p2p_can_connect(cluster, name = worker_id, host = host, ssh_args = ssh_args)) {
    stop(sprintf("Cannot connect to P2P cluster %s - make sure they have given you (%s) access", sQuote(cluster), sQuote(pico_username(host = host, ssh_args = ssh_args))))
  }

  channel_prefix <- sprintf("%s_%s", .packageName, session_uuid())
  channels <- c(
    tx = tempfile(pattern = channel_prefix, fileext = ".tx")
  )
  lapply(channels, FUN = file.create, showWarnings = FALSE)
  on.exit({
    stop_if_not(is.character(channels))
    file.remove(channels)
  }, add = TRUE)

  tx_worker <- function(msg, channel = channels[["tx"]]) {
    writeLines(msg, con = channel)
  } ## tx_worker()


  now <- pico_p2p_time()

  expires <- pico_p2p_time(delta = duration)

  args <- list(
    cluster = cluster,
    worker_id = worker_id,
    host = host,
    ssh_args = ssh_args,
    duration = duration,
    channels = channels
  )

  info("connect worker %s to p2p cluster %s", sQuote(worker_id), sQuote(cluster))
  cluster_owner <- dirname(cluster)
  if (cluster_owner == pico_username(host = host, ssh_args = ssh_args)) {
    topic <- sprintf("%s/future.p2p", basename(cluster))
  } else {
    topic <- sprintf("%s/future.p2p", cluster)
  }
  p <- pico_pipe(topic, user = worker_id, host = host, ssh_args = ssh_args)
  on.exit({
    ## FIXME: Update the P2P message board that we're disconnecting
  }, add = TRUE)

  info("launching background worker process")
  rx <- r_bg(run_worker, args = args, supervise = TRUE, package = TRUE)
  attr(rx, "channels") <- args[["channels"]]
  info("worker process started")

  ## Announce we're available
  info("announcing to p2p message board we are joining as a worker")
  m <- pico_p2p_hello(p, type = "worker", expires = expires)

  ## Main loop monitoring the P2P message board and the background worker
  state <- "waiting"
  future <- NULL
  client <- NULL
  offer_expires <- Inf
  
  ## List of known requests
  requests <- data.frame(
    future = character(0L),
    expires = as.POSIXct(NA_real_)[FALSE],
    client = character(0L)
  )
  stop_if_not(is.data.frame(requests))
  
  info(sprintf("waiting for request [state=%s]", state))
  
  repeat tryCatch({
    ## Is the worker process still alive
    if (!rx$is_alive()) {
      info("terminated")
      ## FIXME: Update the P2P message board
      break
    }

    worker_status <- process_worker_messages(rx, debug = debug)
    
    if (state == "exit") {
      info("Terminating worker")
      break
    }

    ## Handle worker status updates
    if (length(worker_status) > 0) {
      info("Status update received from worker: [n=%d] %s", length(worker_status), commaq(worker_status))
      
      if (!is.null(future)) {
        if ("resolved" %in% worker_status) {
          state <- "resolved"
          info("Future %s has been resolved", sQuote(future))
        }
  
        if ("uploading" %in% worker_status) {
          state <- "uploading"
          info("Future results %s are being sent to client %s", sQuote(future), sQuote(client))
        }
      }
      
      if ("ready" %in% worker_status) {
        info("worker process is ready")
        ## Drop future from list of requests 
        if (!is.null(future)) {
         drop <- future
         requests <- subset(requests, future != drop)
         stop_if_not(is.data.frame(requests))
        }
        state <- "waiting"
        offer_expires <- Inf
        future <- NULL
        client <- NULL
        info("Future %s has been resolved and results have been sent to client %s", sQuote(future), sQuote(client))
        info(sprintf("waiting for request [state=%s]", state))
      }
      
      if ("interrupted" %in% worker_status) {
        signalCondition(worker_interrupt())
      }
    } ## if (length(worker_status) > 0)

    ## Expired?
    now <- Sys.time()
    if (now > expires) {
      info("expired")
      signalCondition(future_withdraw("worker expired; terminating", future = future))
    } else if (state == "offer" && now > offer_expires) {
      info("work offer expired")
      signalCondition(future_withdraw("worker offer expired", future = future))
    }

    ## New message message?
    m <- pico_p2p_next_message(p) ## This is non-block; may return NULL

    if (length(m) > 0) {
      type <- m[["type"]]

      ## A request?
      if (type == "request") {
        ## A new request?
        if (! m[["future"]] %in% requests[["future"]]) {
          request <- data.frame(
            future = m[["future"]],
            expires = as.POSIXct(as.numeric(m[["expires"]])),
            client = m[["from"]]
          )
          requests <- rbind(requests, request)
          stop_if_not(is.data.frame(requests))
        }
      }
      
      ## Request accepted by another worker
      if (m[["type"]] == "accept" && m[["to"]] != worker_id) {
        if (state %in% "offer") {
          info("withdraw offer for future %s, because client %s accepted another worker's offer", sQuote(m[["future"]]), sQuote(m[["from"]]))
          signalCondition(future_withdraw(sprintf("another worker took on the future (state %s)", sQuote(state)), future = m[["future"]]))
        } else {
          info("drop request for future %s, because accepted by another worker", sQuote(m[["future"]]))
          signalCondition(future_withdraw(sprintf("drop request for future (state %s)", sQuote(state)), future = m[["future"]]))
        }
      }

      ## Withdrawal of future?
      if (type == "withdraw") {
        signalCondition(future_withdraw(future = m[["future"]]))
        next
      }
    } ## if (length(m) > 0)

    ## Drop expired requests
    requests <- subset(requests, expires >= now)
    stop_if_not(is.data.frame(requests))
    
    if (nrow(requests) > 0) {
      if (debug) {
        mdebugf("Known requests: [n=%d] (state %s)", nrow(requests), sQuote(state))
        mprint(requests)
      }
    }

    ## Are we read to offer to do work?
    if (state == "waiting" && nrow(requests) > 0) {
      stop_if_not(is.null(future), is.null(client))
      request <- head(requests, 1L)
      future <- request[["future"]]
      client <- request[["client"]]
      
      ## Make a work offer for 15 seconds
      duration <- 15.0
      info("offer to process future %s for client %s (valid for %g seconds)", sQuote(future), sQuote(client), duration)
        
      state <- "offer"
      m0 <- pico_p2p_take_on_future(p, to = client, future = future, duration = duration)
      offer_expires <- m0[["expires"]]
      
      ## Drop from known requests
      requests <- subset(requests, future != request[["future"]])
      next
    }

    if (length(m) > 0) {
      if (state == "offer" && future %in% m[["future"]]) {
        info("waiting for acceptance of our work offer")
        if (m[["type"]] == "accept" && m[["to"]] == worker_id) {
          info("client %s accepted our offer to process future %s", sQuote(client), sQuote(future))
  
          ## Do we support the file transfer protocol?
          via <- m[["via"]]
          uri <- parse_transfer_uri(via)
          if (!uri[["protocol"]] %in% supported_transfer_protocols()) {
            info("non-supported protocol")
            signalCondition(future_withdraw(sprintf("non-supported file-transfer protocol: %s", uri[["protocol"]]), future = m[["future"]]))
            state <- "waiting"
            next
          }

          ## Drop future from list of requests
          if (!is.null(m[["future"]])) {
            requests <- subset(requests, future != m[["future"]])
            stop_if_not(is.data.frame(requests))
          }
          
          state <- "working"
          info("downloading")
            
          ## Tell worker to receive future from client
          tx_worker(sprintf("download=%s,via=%s,from=%s", future, via, client))

          ## Wait for worker to *start* download future
          repeat {
            worker_status <- process_worker_messages(rx, debug = debug)
            if ("downloading" %in% worker_status) {
              ## FIXME: Acknowledge to work on future
              break
            }
            Sys.sleep(0.1)
          }
        }
      }
    } ## if (length(m) > 0)
  }, future_withdraw = function(c) {
    msg <- conditionMessage(c)
    info <- sprintf("state %s", sQuote(state))
    if (!is.null(client)) info <- c(info, sprintf("client %s", sQuote(client)))
    if (!is.null(future)) info <- c(info, sprintf("future %s", sQuote(future)))
    info <- paste(info, collapse = ", ")
    msg <- sprintf("%s [%s]", msg, info)
    info(msg)

    ## Client withdrew a future we're either work on or offered to work on
    if (!is.null(future) && (future %in% c[["future"]])) {
      if (state == "offer") {
        ## FIXME: Decline work offer (although we can just ignore it
        ## because the client did not respect what we support)
      } else if (state %in% c("working", "resolved")) {
        info(sprintf("Interrupting worker [state %s]", state))
        rx$interrupt()
      } else {
        stop(FutureError(sprintf("Internal error: state %s", sQuote(state))))
      }
      state <<- "waiting"
      offer_expires <<- Inf
      future <<- NULL
      client <<- NULL
      
      info(sprintf("waiting for request [state=%s]", state))
    }

    ## Drop future from list of requests, in case it's there
    if (!is.null(c[["future"]])) {
      requests <<- subset(requests, future != c[["future"]])
      stop_if_not(is.data.frame(requests))
    }
    ## FIXME: Acknowledge withdrawal of future
  }, worker_interrupt = function(c) {
    info("Worker process was interrupted")
    state <<- "waiting"
    offer_expires <<- Inf
    future <<- NULL
    client <<- NULL
    info(sprintf("waiting for request [state=%s]", state))
    ## FIXME: Acknowledge withdrawal of future
  }, interrupt = function(c) {
    info("interrupted")
    state <<- "exit"
    offer_expires <<- Inf
    future <<- NULL
    client <<- NULL
    
    ## Interrupt worker
    info("interrupting worker")
    rx$interrupt()
    
    ## FIXME: Update the P2P message board
    info("exiting")
  }) ## repeat tryCatch({ ... })

  info("Waiting 5 seconds before killing the worker process and its children ...")
  Sys.sleep(5.0)
  rx$kill_tree()

  info("wait for the worker process to terminate")
  rx$wait()

  info("finalize worker process")
  rx$finalize()
  
  invisible(result)
} ## worker()


#' @importFrom future resolve plan sequential
run_worker <- function(cluster, worker_id, host, ssh_args, duration, channels) {
  old_opts <- options(
    parallelly.availableCores.fallback = 1L,
    future.p2p.info.from = "worker"
  )
  on.exit(options(old_opts))
  with(plan(sequential), local = TRUE)

  now <- pico_p2p_time()

  expires <- pico_p2p_time(delta = duration)
  duration <- difftime(duration, 0)

  info("get 'wormhole'")
  bin <- find_wormhole()

  info("assert connection to p2p cluster %s", sQuote(cluster))
  if (!p2p_can_connect(cluster, name = worker_id, host = host, ssh_args = ssh_args)) {
    stop(sprintf("Cannot connect to P2P cluster %s - make sure they have given you (%s) access", sQuote(cluster), sQuote(pico_username(host = host, ssh_args = ssh_args))))
  }

  info("connect background worker process %s to p2p cluster %s for %s until %s", sQuote(worker_id), sQuote(cluster), format(duration), expires)
  cluster_owner <- dirname(cluster)
  if (cluster_owner == pico_username(host = host, ssh_args = ssh_args)) {
    topic <- sprintf("%s/future.p2p", basename(cluster))
  } else {
    topic <- sprintf("%s/future.p2p", cluster)
  }
  p <- pico_pipe(topic, user = worker_id, host = host, ssh_args = ssh_args)

  tx <- channels[["tx"]]

  rx_parent <- function(channel = channels[["tx"]], clear = TRUE) {
    ## Read everything available
    bfr <- readLines(channel, n = 1e6, warn = FALSE)
    ## Consume communication channel
    if (clear) file.create(channel)
    bfr
  } ## rx_parent()

  tx_parent <- function(msg) {
    cat(sprintf("worker_status=%s\n", msg), file = stdout())
    flush(stdout())
  } ## tx_parent()

  ## Tell parent that worker is ready
  tx_parent("ready")

  state <- "ready"
  repeat tryCatch({
    ## Wait for instructions from parent
    action <- rx_parent()
    if (length(action) == 0) {
      Sys.sleep(0.1)
      next
    }

    ## Download and process future?
    pattern <- "^download=([^,]+),via=([^,]+),from=([^,]+)$"
    if (grepl(pattern, action)) {
      future <- sub(pattern, "\\1", action)
      via <- sub(pattern, "\\2", action)
      client <- sub(pattern, "\\3", action)
      info("download future %s via %s from %s", sQuote(future), sQuote(via), sQuote(client))
      stop_if_not(
        nzchar(future), !grepl("[,=]", future),
        nzchar(via), !grepl("[,=]", via)
      )
      tx_parent("downloading")
      state <- "downloading"
      dt <- system.time({
        res <- pico_p2p_receive_future(p, via = via)
      })
      dt <- difftime(dt[3], 0)
      info("Future %s received in %s", sQuote(future), format(dt))
      
      f <- res[["future"]]
      stop_if_not(paste(f[["uuid"]], collapse = "-") == future)

      info("process future %s", sQuoteLabel(f))
      state <- "processing"
      dt <- system.time({
        r <- tryCatch({ result(f) }, error = identity)  ## Note, result() handles 'interrupt':s
      })
      dt <- difftime(dt[3], 0)
      info("Future %s resolved after %s", sQuote(future), format(dt))
      tx_parent("resolved")
      
      info("sending future result %s via %s", sQuote(future), sQuote(via))
      state <- "uploading"
      ## NOTE, this may be interrupted
      dt <- system.time({
        pico_p2p_send_result(p, future = f, to = client, via = via)
      })
      dt <- difftime(dt[3], 0)
      info("future result %s sent in %s", sQuote(future), format(dt))
      tx_parent("ready")
      state <- "ready"
    }
  }, interrupt = function(c) {
    info(sprintf("interrupted [state %s]", sQuote(state)))
    tx_parent("interrupted")
    state <<- "ready"
    tx_parent("ready")
  }) ## repeat tryCatch({ ... })
  
  info("bye")
} ## run_worker()


## Expose function on the CLI
cli_fcn(worker) <- c("--(cluster)=(.*)", "--(host)=(.*)", "--(ssh_args)=(.*)", "--(duration)=([[:digit:]]+)")


future_withdraw <- function(message = "future withdrawn by client", call = NULL, future = NULL) {
  cond <- simpleCondition(message = message, call = call)
  cond[["future"]] <- future
  class(cond) <- c("future_withdraw", class(cond))
  cond
}


worker_interrupt <- function(message = "worker process interrupted", call = NULL) {
  cond <- simpleCondition(message = message, call = call)
  class(cond) <- c("worker_interrupt", class(cond))
  cond
}


process_worker_messages <- function(rx, debug = FALSE) {
  if (debug && isTRUE(getOption("future.debug"))) {
    mdebug_push("process_worker_messages() ...")
    on.exit({
      mdebugf("worker_status: [n=%d] %s", length(worker_status), commaq((worker_status)))
      mdebug_pop()
    })
  }
  
  ## Any updates from worker, e.g. output to be relayed?
  res <- poll(list(rx), ms = 100)[[1]]

  worker_status <- NULL
    
  ## Relay stdout?
  if ("ready" %in% res[["output"]]) {
    out <- rx$read_output_lines()

    ## Parse special messages
    pattern <- "^worker_status="
    is_special <- grepl(pattern, out)
    worker_status <- sub(pattern, "", out[is_special])
    out <- out[!is_special]
    
    out <- sprintf("  %s", out)
    writeLines(out, con = stdout())
  }
    
  ## Relay stderr?
  if ("ready" %in% res[["error"]]) {
    err <- rx$read_error_lines()
    err <- sprintf("  %s", err)
    writeLines(err, con = stderr())
  }

  ## Return new worker status, if received
  worker_status
} ## process_worker_messages()

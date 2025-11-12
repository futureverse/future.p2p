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
    tx = tempfile(pattern = channel_prefix, fileext = ".tx"),
    rx = tempfile(pattern = channel_prefix, fileext = ".rx")
  )
  lapply(channels, FUN = file.create, showWarnings = FALSE)
  on.exit({
    lapply(channels, FUN = file.remove, showWarnings = FALSE)
  }, add = TRUE)

  rx_worker <- function(channel = channels[["rx"]], clear = TRUE) {
    if (file.size(channel) == 0L) return(character(0L))
    if (debug) {
      mdebugf_push("rx_worker(clear = %s) ...", clear)
      mdebug_pop()
    }
    ## Read everything available
    bfr <- readLines(channel, n = 1e6, warn = FALSE)
    if (debug) {
      mdebugf("messages from worker process: [n=%d] %s", length(bfr), commaq(bfr))
    }
    
    ## Consume communication channel 'rx'?
    if (clear) file.create(channel)

    ## Was the worker interrupted?
    if ("interrupted" %in% bfr) {
      signalCondition(worker_interrupt())
    }

    bfr
  } ## rx_worker()

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

  info("waiting for request")
  
  repeat tryCatch({
    ## Is the worker process still alive
    if (!rx$is_alive()) {
      info("terminated")
      ## FIXME: Update the P2P message board
      break
    }

    ## Any updates from worker, e.g. output to be relayed?
    res <- poll(list(rx), ms = 100)[[1]]

    worker_status <- NULL
    
    ## Relay stdout?
    if ("ready" %in% res[["output"]]) {
      out <- rx$read_output_lines()
      is_special <- grepl("^worker_status=", out)
      worker_status <- out[is_special]
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

    if (state == "exit") {
      info("Terminating worker")
      break
    }

    ## Handle worker status updates
    if (length(worker_status) > 0) {
      worker_status <- sub("^worker_status=", "", worker_status)
      info("Status update received from worker: [n=%d] %s", length(worker_status), commaq(worker_status))
      
      if ("ready" %in% worker_status) {
       info("worker process is ready")
      }
      if ("interrupted" %in% worker_status) {
        signalCondition(worker_interrupt())
      }
    }

    ## Expired?
    if (Sys.time() > expires) {
      info("time is out")
      ## FIXME: Update the P2P message board
      rx$interrupt()
      future <- NULL
      client <- NULL
      break
    }

    ## Was worker process interrupted?
    info <- rx_worker()
    if ("interrupted" %in% info) {
       state <- "waiting"
       future <- NULL
       client <- NULL
       ## FIXME: Update client via P2P message board
       next
    }

    ## Any messages from the P2P message board?
#    res <- poll(list(p), ms = 100)[[1]]
#    if (!"ready" %in% res[["output"]]) next

    ## Read next message?
    m <- pico_p2p_next_message(p)
    
    ## Expired?
    now <- Sys.time()
    if (now > expires) {
      info("expired")
      signalCondition(future_withdraw("worker expired; terminating"))
      next
    } else if (state == "offer" && now > offer_expires) {
      info("work offer expired")
      signalCondition(future_withdraw("worker offer expired"))
      next
    }
    
    ## Process request?
    if (length(m) > 0) {
      ## Are we read to offer to do work?
      if (state == "waiting" && m[["type"]] == "request") {
        stop_if_not(is.null(future), is.null(client))
        future <- m[["future"]]
        client <- m[["from"]]
        
        ## Make a work offer for 15 seconds
        duration <- 15.0
        info("offer to process future %s for client %s (valid for %g seconds)", sQuote(future), sQuote(client), duration)
        
        state <- "offer"
        m0 <- pico_p2p_take_on_future(p, to = client, future = future, duration = duration)
        offer_expires <- m0[["expires"]]
      } else if (state == "offer" && future %in% m[["future"]]) {
        info("waiting for acceptance of our work offer")
        if (m[["type"]] == "accept") {
          if (m[["to"]] == worker_id) {
            info("client %s accepted our offer to process future %s", sQuote(client), sQuote(future))
  
            ## Do we support the file transfer protocol?
            via <- m[["via"]]
            uri <- parse_transfer_uri(via)
            if (!uri[["protocol"]] %in% supported_transfer_protocols()) {
              info("non-supported protocol")
              signalCondition(future_withdraw(sprintf("non-supported file-transfer protocol: %s", uri[["protocol"]])))
            }
            
            state <- "working"
            
            ## Tell worker to receive future from client
            tx_worker(sprintf("download=%s,via=%s", future, via))
            
            ## Wait for worker to *start* download future
            repeat {
              info <- rx_worker()
              if (length(info) == 0) {
                Sys.sleep(0.1)
                next
              }
              
              if ("downloading" %in% info) {
                ## FIXME: Acknowledge to work on future
                break
              }
            }
          } else {
            info("withdraw offer for future %s, because client %s accepted another worker's offer", sQuote(future), sQuote(client))
            signalCondition(future_withdraw("another worker took on the future"))
          }
        } else if (m[["type"]] == "withdraw") {
          signalCondition(future_withdraw())
        }
      } else if (state == "working") {
        ## Withdrawal of future?
        if (m[["type"]] == "withdraw" && future %in% m[["future"]]) {
          signalCondition(future_withdraw())
        }
      }
    } ## if (length(m) > 0)
    
    if (state == "working") {
      ## Check if worker is done
      if ("resolved" %in% info) {
        state <- "resolved"
        info("Future %s has been resolved and results will be sent to client %s", sQuote(future), sQuote(client))
        ## FIXME: Inform client that future has been resolved
      }
    } else if (state == "resolved") {
      ## Check if future results have been transferred
      if ("ready" %in% info) {
        state <- "waiting"
        offer_expires <- Inf
        future <- NULL
        client <- NULL
        info("Future %s has been resolved and results have been sent to client %s", sQuote(future), sQuote(client))
        info("waiting for request")
      }
    }
  }, future_withdraw = function(c) {
    msg <- conditionMessage(c)
    info <- sprintf("state %s", sQuote(state))
    if (!is.null(client)) info <- c(info, sprintf("client %s", sQuote(client)))
    if (!is.null(future)) info <- c(info, sprintf("future %s", sQuote(future)))
    info <- paste(info, collapse = ", ")
    msg <- sprintf("%s [%s]", msg, info)
    info(msg)
    
    ## Client withdrew future
    if (state == "waiting") {
      state <<- "waiting"
    } else if (state == "offer") {
      ## FIXME: Decline work offer (although we can just ignore it
      ## because the client did not respect what we support)
      state <<- "waiting"
    } else if (state == "working") {
      state <<- "interrupt"
      rx$interrupt()
    }
    
    offer_expires <<- Inf
    future <<- NULL
    client <<- NULL
    info("waiting for request")
    ## FIXME: Acknowledge withdrawal of future
  }, worker_interrupt = function(c) {
    info("Worker process was interrupted")
    state <<- "waiting"
    offer_expires <<- Inf
    future <<- NULL
    client <<- NULL
    info("waiting for request")
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

  rx <- channels[["rx"]]
  tx <- channels[["tx"]]

  rx_parent <- function(channel = channels[["tx"]], clear = TRUE) {
    ## Read everything available
    bfr <- readLines(channel, n = 1e6, warn = FALSE)
    ## Consume communication channel 'rx'?
    if (clear) file.create(channel)
    bfr
  } ## rx_parent()

  tx_parent <- function(msg, channel = channels[["rx"]]) {
    cat(sprintf("worker_status=%s\n", msg), file = stdout())
    flush(stdout())
    writeLines(msg, con = channel)
  } ## tx_parent()

  ## Tell parent that worker is ready
  tx_parent("ready")

  repeat tryCatch({
    ## Wait for instructions from parent
    action <- rx_parent()
    if (length(action) == 0) {
      Sys.sleep(0.1)
      next
    }

    ## Download and process future?
    pattern <- "^download=([^,]+),via=(.*)$"
    if (grepl(pattern, action)) {
      future <- sub(pattern, "\\1", action)
      via <- sub(pattern, "\\2", action)
      info("download future %s via %s", sQuote(future), sQuote(via))
      stop_if_not(
        nzchar(future), !grepl("[,=]", future),
        nzchar(via), !grepl("[,=]", via)
      )
      tx_parent("downloading")
      dt <- system.time({
        res <- pico_p2p_receive_future(p, via = via)
      })
      dt <- difftime(dt[3], 0)
      info("Future %s received in %s", sQuote(future), format(dt))
      
      f <- res[["future"]]
      stop_if_not(paste(f[["uuid"]], collapse = "-") == future)

      info("process future %s", sQuoteLabel(f))
      dt <- system.time({
        r <- tryCatch(result(f), error = identity)
      })
      dt <- difftime(dt[3], 0)
      info("Future %s resolved after %s", sQuote(future), format(dt))
      tx_parent("resolved")
      
      info("sending future result %s via %s", sQuote(future), sQuote(via))
      dt <- system.time({
        res <- pico_p2p_send_result(p, future = f, via = via)
      })
      dt <- difftime(dt[3], 0)
      info("future result %s sent in %s", sQuote(future), format(dt))
      tx_parent("ready")
    }
  }, interrupt = function(c) {
    info("interrupted")
    tx_parent("interrupted")
  }) ## repeat tryCatch({ ... })
  
  info("bye")
} ## run_worker()


## Expose function on the CLI
cli_fcn(worker) <- c("--(cluster)=(.*)", "--(host)=(.*)", "--(ssh_args)=(.*)", "--(duration)=([[:digit:]]+)")


future_withdraw <- function(message = "future withdrawn by client", call = NULL) {
  cond <- simpleCondition(message = message, call = call)
  class(cond) <- c("future_withdraw", class(cond))
  cond
}


worker_interrupt <- function(message = "worker process interrupted", call = NULL) {
  cond <- simpleCondition(message = message, call = call)
  class(cond) <- c("worker_interrupt", class(cond))
  cond
}


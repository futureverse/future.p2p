#' pico_p2p futures
#'
#' _WARNING: This function must never be called.
#'  It may only be used with [future::plan()]_
#'
#' A 'pico_p2p' future is an asynchronous multiprocess
#' future that will be evaluated in a background R session.
#'
#' @inheritParams pico_pipe
#'
#' @param cluster The p2p cluster to connect to.
#'
#' @param name The name of the client as publicized on the P2P cluster.
#'
#' @param \ldots Not used.
#'
#' @return An object of class `PicoP2PFuture`.
#'
#' @details
#' The Pico P2P future backend relies on Pico (1) to
#' distribute futures among a peer-to-peer (P2P) cluster of R workers.
#' Users with a Pico account can join the P2P cluster by
#' being invited to a shared folder.
#'
#' Users who wish to contribute their compute power to the P2P cluster
#' should call [pico_p2p_worker()].
#'
#' Users who wish to take advantage of the compute power of the
#' P2P cluster should use `plan(pico_p2p)`.
#'
#' @examplesIf interactive()
#' ## Futures are pushed to the Pico P2P cluster and 
#' ## results are collected from there
#' plan(future.p2p::pico_p2p, .init = FALSE)
#' 
#' ## Create future
#' a <- 42
#' f <- future({ 2 * a })
#' 
#' ## Get results
#' v <- value(f)
#' print(v)
#'
#' @references
#' 1. pico.sh, The ultimate ssh powered services for developers,
#' <https://pico.sh/>.
#'
#' @importFrom future future
#' @export
pico_p2p <- function(cluster = p2p_cluster(), name = p2p_name(), host = "pipe.pico.sh", ssh_args = NULL, ...) {
  stop("INTERNAL ERROR: The future.p2p::pico_p2p() must never be called directly")
}
class(pico_p2p) <- c("pico_p2p", "multiprocess", "future", "function")
attr(pico_p2p, "init") <- TRUE


#' A Pico P2P future is resolved through a Peer-to-Peer (P2P) workers communicating via pico.sh and Wormhole
#'
#' @inheritParams pico_p2p
#'
#' @param \ldots Additional arguments passed to [future::FutureBackend()].
#'
#' @return A PicoP2PFutureBackend object
#'
#'
#' @importFrom future FutureBackend
#' @keywords internal
#' @export
PicoP2PFutureBackend <- function(cluster = p2p_cluster(), name = p2p_name(), host = "pipe.pico.sh", ssh_args = NULL, ...) {
  parts <- strsplit(cluster, split = "/", fixed = TRUE)[[1]]
  if (length(parts) != 2L) {
    stop(sprintf("Argument 'cluster' must be of format '{owner}/{name}': %s", sQuote(cluster)))
  }

  args <- list(...)

  ## Argument 'workers' will most likely be removed at some point
  workers <- args[["workers"]]
  if (is.null(workers)) {
    workers <- availablePicoP2PWorkers()
  } else if (is.function(workers)) {
    workers <- workers()
  }
  stop_if_not(length(workers) == 1L)
  if (is.numeric(workers)) {
    workers <- as.integer(workers)
    stop_if_not(!anyNA(workers), all(workers >= 1))
  } else {
    stop("Argument 'workers' should be numeric: ", mode(workers))
  }

  core <- FutureBackend(
    cluster = cluster,
    name = name,
    host = host,
    ssh_args = ssh_args,
    reg = "workers-pico-p2p",
    workers = workers,
    ...
  )
  core[["futureClasses"]] <- c("PicoP2PFuture", core[["futureClasses"]])
  core <- structure(core, class = c("PicoP2PFutureBackend", "MultiprocessFutureBackend", "FutureBackend", class(core)))

  if (!p2p_can_connect(cluster, name = name, host = host, ssh_args = ssh_args)) {
    stop(sprintf("Cannot connect to P2P cluster %s - make sure they have given you (%s) access", sQuote(cluster), sQuote(pico_username())))
  }

  core
}
attr(pico_p2p, "factory") <- PicoP2PFutureBackend



#' @importFrom future run FutureError
#' @keywords internal
#' @importFrom future launchFuture
#' @export
launchFuture.PicoP2PFutureBackend <- function(backend, future, ...) {
  debug <- isTRUE(getOption("future.debug"))
  if (debug) {
    mdebugf_push("launchFuture() for %s ...", class(backend)[1])
    on.exit(mdebugf_pop())
  }

  if (future[["state"]] != "created") {
    label <- sQuoteLabel(future)
    msg <- sprintf("A future ('%s') can only be launched once", label)
    stop(FutureError(msg, future = future))
  }

  ## Assert that the process that created the future is
  ## also the one that evaluates/resolves/queries it.
  assertOwner(future)

  ## 1. Record backend for now
  future[["backend"]] <- backend

  ## 2. Allocate future to worker
  reg <- backend[["reg"]]
  FutureRegistry(reg, action = "add", future = future, earlySignal = FALSE)

  ## 3. Launch future, i.e. submit it to the Pico P2P cluster dispatcher
  future <- dispatch_future(future)

  invisible(future)
} ## launchFuture()


#' @importFrom future nbrOfWorkers
#' @export
nbrOfWorkers.PicoP2PFutureBackend <- function(evaluator) {
  backend <- evaluator
  workers <- backend[["workers"]]
  stop_if_not(length(workers) == 1L, !is.na(workers), workers >= 0L, is.finite(workers))
  workers
}


#' @importFrom future nbrOfFreeWorkers
#' @export
nbrOfFreeWorkers.PicoP2PFutureBackend <- function(evaluator = NULL, background = FALSE, ...) {
  backend <- evaluator
  workers <- backend[["workers"]]
  reg <- backend[["reg"]]
  usedWorkers <- length(FutureRegistry(reg, action = "list",
                        earlySignal = FALSE))
  workers <- workers - usedWorkers
  stop_if_not(length(workers) == 1L, !is.na(workers), workers >= 
      0L, is.finite(workers))
  workers
}



#' @return
#' `availablePicoP2PWorkers()` returns the number of registered workers on the P2P cluster.
#' It will always return at least one worker, which is yourself.
#' _WARNING: This is currently hardcoded to 10 workers, regardless of the number._
#' 
#' @rdname pico_p2p
#' @export
availablePicoP2PWorkers <- function() {
  nworkers <- 10L
  nworkers <- max(1L, nworkers, na.rm = TRUE)
  as.integer(nworkers)
}


## FIXME: To be implemented
waitForWorker <- function(...) {
}


#' @importFrom callr r_bg
#' @importFrom utils file_test
dispatch_future <- function(future) {
  send_future <- function(topic, name, host = host, ssh_args = ssh_args, future_id, file, to, via, duration) {
    pico <- future.p2p::pico_pipe(topic, user = name, host = host, ssh_args = ssh_args)
    m <- future.p2p::pico_p2p_hello(pico, type = "client")

    ## 2. Announce future
    repeat {
      m1 <- future.p2p::pico_p2p_have_future(pico, future = file, duration = duration)
      m2 <- future.p2p::pico_p2p_wait_for(pico, type = "offer", futures = m1[["future"]], expires = m1[["expires"]])
      if (m2[["type"]] != "expired") break
    }

    ## 3. Send future to workers
    worker <- m2[["from"]]
    stopifnot(is.character(worker), nzchar(worker))
    m3 <- future.p2p::pico_p2p_send_future(pico, future = file, to = worker, via = via)

    ## 4. Remove temporary file
    file.remove(file)

    ## 5. Wait for and receive FutureResult file
    path <- file.path(dirname(dirname(file)), "results")
    tryCatch({
      file <- future.p2p::pico_p2p_receive_result(pico, via = via, path = path)
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
  via <- via_channel()
  
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


#' @importFrom utils file_test
p2p_dir <- function(dir = c("queued", "running", "results")) {
  dir <- match.arg(dir)
  path <- file.path(tempdir(), dir)
  if (!file_test("-d", path)) dir.create(path, recursive = TRUE)
  path
}



#' @export
print.PicoP2PFutureBackend <- function(x, ...) {
  NextMethod()
  backend <- x

  cat(sprintf("P2P cluster: %s\n", sQuote(backend[["cluster"]])))
  cat(sprintf("P2P client ID: %s\n", sQuote(backend[["name"]])))

  clusters <- pico_p2p_hosted_clusters(backend[["host"]], backend[["ssh_args"]])
  cat(sprintf("P2P clusters you are hosting: [n=%d]\n", nrow(clusters)))
  for (kk in seq_len(nrow(clusters))) {
    cluster <- clusters[kk, ]
    cat(sprintf(" %2d. %s (%s)\n", kk, cluster$name, cluster$users))
  }

  cat("Message board:\n")
  cat(sprintf(" - Server: %s\n", backend[["host"]]))
  username <- pico_username(backend[["host"]], backend[["ssh_args"]])
  cat(sprintf(" - Username: %s\n", sQuote(username)))

  cat("Data transfer tools:\n")
  info <- tryCatch({
    bin <- find_wormhole()
    attr(bin, "version-string")
  }, error = function(ex) "<please install wormhole>")
  cat(sprintf(" %2d. %s (%s)\n", 1L, "wormhole", info))

  invisible(backend)
}


p2p_can_connect <- function(cluster, name = name, host = "pipe.pico.sh", ssh_args = NULL, timeout = 10.0) {
  cluster_owner <- dirname(cluster)
  if (cluster_owner == pico_username()) {
    topic <- sprintf("%s/future.p2p", basename(cluster))
  } else {
    topic <- sprintf("%s/future.p2p", cluster)
  }

  ## (1) Attempt to connect
  before <- pico_p2p_hosted_clusters(host = host, ssh_args = ssh_args)
  p_pipe <- pico_pipe(topic, args = "-r", user = name, host = host, ssh_args = ssh_args)
  on.exit(pico_terminate(p_pipe))
  p <- p_pipe$process
  msg <- sprintf("msg=%s", topic)
      
  ## (2) Test connection
  t_max <- proc.time()[3] + timeout
  m <- pico_p2p_hello(p_pipe, type = "client")
  msg <- attr(m, "message")
  repeat {
    bfr <- pico_receive_message(p_pipe)
    if (length(bfr) > 0L) {
      if (any(bfr == msg)) break
    }
    if (proc.time()[3] > t_max) {
      stop("Failed to connect to pico.sh pipe - echo failed")
    }
    Sys.sleep(0.1)
  }

  ## (3) Check if we created our own cluster on-the-fly
  after <- pico_p2p_hosted_clusters(host = host, ssh_args = ssh_args)
  delta <- setdiff(after$name, before$name)
  for (name in delta) {
    parts <- strsplit(name, split = "/", fixed = TRUE)[[1]]
    if (length(parts) > 1) return(FALSE)
  }
  
  TRUE
}

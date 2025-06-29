#' pico_p2p futures
#'
#' _WARNING: This function must never be called.
#'  It may only be used with [future::plan()]_
#'
#' A 'pico_p2p' future is an asynchronous multiprocess
#' future that will be evaluated in a background R session.
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
pico_p2p <- function(cluster = p2p_cluster(), name = p2p_name(), ...) {
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
PicoP2PFutureBackend <- function(cluster = p2p_cluster(), name = p2p_name(), ...) {
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
    reg = "workers-pico-p2p",
    workers = workers,
    ...
  )
  core[["futureClasses"]] <- c("PicoP2PFuture", core[["futureClasses"]])
  core <- structure(core, class = c("PicoP2PFutureBackend", "MultiprocessFutureBackend", "FutureBackend", class(core)))
  
  core[["pico"]] <- pico_pipe(core[["cluster"]], user = core[["name"]])
  m <- pico_hello(core[["pico"]], type = "client")
  
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



# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Future API
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
#' @importFrom future resolved
#' @keywords internal
#' @export
resolved.PicoP2PFuture <- function(x, .signalEarly = TRUE, ...) {
  future <- x

  resolved <- NA
  
  debug <- isTRUE(getOption("future.debug"))
  if (debug) {
    mdebugf_push("resolved() for %s ...", class(x)[1])
    on.exit({
      mdebugf("Resolved: %s", resolved)
      mdebugf_pop()
    })
  }

  rx <- future[["rx"]]

  ## Still running?
  resolved <- !rx$is_alive()
  
  resolved
}

#' @importFrom future result UnexpectedFutureResultError
#' @keywords internal
#' @export
result.PicoP2PFuture <- function(future, ...) {
  debug <- isTRUE(getOption("future.debug"))
  if (debug) {
    mdebugf_push("result() for %s ...", class(future)[1])
    mdebugf("Future UUID: %s", paste(future[["uuid"]], collapse = "-"))
    on.exit(mdebugf_pop())
  }
  
  result <- future[["result"]]
  if (!is.null(result)) {
    if (inherits(result, "FutureError")) stop(result)
    return(result)
  }
  
  if (future[["state"]] == "created") {
    future <- run(future)
  }

  rx <- future[["rx"]]
  if (debug) mdebug("Waiting for dispatch process to finish")
  rx$wait()
  file <- rx$get_result()
  future[["rx"]] <- NULL
  if (debug) mdebugf("FutureResult file: %s [%g bytes]", sQuote(file), file.size(file))
  if (!file_test("-f", file)) {
    stop(FutureError(sprintf("FutureResult file not found: ", sQuote(file))), future = future)
  }
  
  result <- local({
    if (debug) {
      mdebug_push("Reading FutureResult from file")
      mdebugf("FutureResult file: %s [%g bytes]", sQuote(file), file.size(file))
      on.exit(mdebug_pop())
    }
    result <- readRDS(file)
    file.remove(file)
    result
  })
    
  future[["result"]] <- result
  
  if (!inherits(result, "FutureResult")) {
    if (inherits(result, "FutureLaunchError")) {
    } else {
      ex <- UnexpectedFutureResultError(future)
      future[["result"]] <- ex
      stop(ex)
    }
  }

  future[["result"]] <- result
  future[["state"]] <- "finished"
  
  result
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
  send_future <- function(cluster, name, future_id, file, to, via, duration) {
    ## 1. Connect to pico
    pico <- future.p2p::pico_pipe(cluster, user = name)

    ## 2. Announce future
    repeat {
      m1 <- future.p2p::pico_have_future(pico, future = file, duration = duration)
      m2 <- future.p2p::pico_wait_for(pico, type = "offer", futures = m1[["future"]], expires = m1[["expires"]])
      if (m2[["type"]] != "expired") break
    }

    ## 3. Send future to workers
    worker <- m2[["from"]]
    stopifnot(is.character(worker), nzchar(worker))
    m3 <- future.p2p::pico_send_future(pico, future = file, to = worker, via = via)

    ## 4. Remove temporary file
    file.remove(file)

    ## 5. Wait for and receive FutureResult file
    path <- file.path(dirname(dirname(file)), "results")
    file <- future.p2p::pico_receive_result(pico, via = via, path = path)

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
  via <- via_channel()
  
  ## 1. Put future on the dispatcher queue
  void <- p2p_dir("results")
  future[["file"]] <- saveFuture(future, path = p2p_dir("queued"))
  
  if (debug) mdebugf("File: %s", sQuote(future[["file"]]))
  args <- list(
    cluster = cluster,
    name = name,
    future_id = future_id(future),
    file = future[["file"]],
    via = via,
    duration = getOption("future.p2p.duration.request", 10.0)
  )
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

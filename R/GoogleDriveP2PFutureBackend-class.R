#' A Google Drive P2P future is resolved through a Peer-to-Peer (P2P) workers communicating via Google Drive
#'
#' @param workers (optional) The maximum number of workers the 
#' backend may use at any time.
#'
#' @param \ldots Additional arguments passed to [future::FutureBackend()].
#'
#' @return A GoogleDriveP2PFutureBackend object
#'
#' @importFrom future FutureBackend
#' @keywords internal
#' @export
GoogleDriveP2PFutureBackend <- function(workers = availableGoogleDriveP2PWorkers(), ...) {
  if (is.function(workers)) workers <- workers()
  stop_if_not(length(workers) == 1L)
  if (is.numeric(workers)) {
    workers <- as.integer(workers)
    stop_if_not(!anyNA(workers), all(workers >= 1))
  } else {
    stop("Argument 'workers' should be numeric: ", mode(workers))
  }

  core <- FutureBackend(
    reg = "workers-google-drive-p2p",
    workers = workers,
    ...
  )
  core[["futureClasses"]] <- c("GoogleDriveP2PFuture", core[["futureClasses"]])
  core <- structure(core, class = c("GoogleDriveP2PFutureBackend", "MultiprocessFutureBackend", "FutureBackend", class(core)))
  core
}



#' @importFrom future run FutureError
#' @keywords internal
#' @importFrom future launchFuture
#' @export
launchFuture.GoogleDriveP2PFutureBackend <- function(backend, future, ...) {
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

  ## 1. Wait for an available worker
  workers <- backend[["workers"]]
  waitForWorker(type = "workers-google-drive-p2p", workers = workers, debug = debug)

  ## 2. Allocate future to worker
  reg <- backend[["reg"]]
  FutureRegistry(reg, action = "add", future = future, earlySignal = FALSE)

  ## Launch future, i.e. submit it to the Google Drive P2P queue
  local({
    if (debug) {
      mdebug_push("Submit future to Google Drive P2P queue ...")
      mdebug_pop()
    }
    future2 <- cloneFuture(future)
    future2[["uuid"]] <- future[["uuid"]]
    push_future(future2)
  })

  ## 3. Running
  future[["state"]] <- "running"

  invisible(future)
} ## launchFuture()


#' @importFrom future nbrOfWorkers
#' @export
nbrOfWorkers.GoogleDriveP2PFutureBackend <- function(evaluator) {
  backend <- evaluator
  workers <- backend[["workers"]]
  stop_if_not(length(workers) == 1L, !is.na(workers), workers >= 0L, is.finite(workers))
  workers
}


#' @importFrom future nbrOfFreeWorkers
#' @export
nbrOfFreeWorkers.GoogleDriveP2PFutureBackend <- function(evaluator = NULL, background = FALSE, ...) {
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
resolved.GoogleDriveP2PFuture <- function(x, .signalEarly = TRUE, ...) {
  future <- x
  
  debug <- isTRUE(getOption("future.debug"))
  if (debug) {
    mdebugf_push("resolved() for %s ...", class(x)[1])
    on.exit(mdebugf_pop())
  }
  
  resolved <- NextMethod()
  if (resolved) return(TRUE)

  future <- get_result(future, block = FALSE)
  resolved <- !is.null(future)
  
  resolved
}

#' @importFrom future result UnexpectedFutureResultError
#' @keywords internal
#' @export
result.GoogleDriveP2PFuture <- function(future, ...) {
  debug <- isTRUE(getOption("future.debug"))
  if (debug) {
    mdebugf_push("result() for %s ...", class(future)[1])
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

  result <- local({
    if (debug) {
      mdebug_push("Polling P2P network for results ...")
      mdebugf("Future UUID: %s", paste(future[["uuid"]], collapse = "-"))
      on.exit(mdebugf_pop())
    }
    future <- get_result(future)
    future[["result"]]
  })
  
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


#' google_drive_p2p futures
#'
#' _WARNING: This function must never be called.
#'  It may only be used with [future::plan()]_
#'
#' A google_drive_p2p future is an asynchronous multiprocess
#' future that will be evaluated in a background R session.
#'
#' @inheritParams future::Future
#' @inheritParams GoogleDriveP2PFutureBackend
#' 
#' @param workers The number of concurrent workers to use.
#' 
#' @param \ldots Additional arguments passed to `Future()`.
#'
#' @return An object of class `GoogleDriveP2PFuture`.
#'
#' @importFrom future future
#' @export
google_drive_p2p <- function(..., workers = availableGoogleDriveP2PWorkers(), envir = parent.frame()) {
  stop("INTERNAL ERROR: The future.google_drive_p2p::google_drive_p2p() must never be called directly")
}
class(google_drive_p2p) <- c("google_drive_p2p", "multiprocess", "future", "function")
attr(google_drive_p2p, "init") <- TRUE
attr(google_drive_p2p, "factory") <- GoogleDriveP2PFutureBackend



#' @return
#' `availableGoogleDriveP2PWorkers()` returns the number of registered workers on the P2P cluster.
#' It will always return at least one worker, which is yourself.
#' _WARNING: This is currently hardcoded to 10 workers, regardless of the number._
#' 
#' @rdname google_drive_p2p
#' @export
availableGoogleDriveP2PWorkers <- function() {
  nworkers <- 10L
  nworkers <- max(1L, nworkers, na.rm = TRUE)
  as.integer(nworkers)
}


#' @importFrom future Future reset
cloneFuture <- function(future) {
  future2 <- Future()
  for (name in names(future2)) {
    assign(name, value = future[[name]], envir = future2, inherits = TRUE)
  }
  future2 <- reset(future2)
  future2
}


## FIXME: To be implemented
waitForWorker <- function(...) {
}

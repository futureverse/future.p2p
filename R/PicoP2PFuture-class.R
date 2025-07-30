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

  ## Already collected?
  result <- future[["result"]]
  if (!is.null(result)) return(TRUE)
  
  ## Still running?
  rx <- future[["rx"]]
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

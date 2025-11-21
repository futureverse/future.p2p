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
  if (is.null(rx)) {
    resolved <- TRUE
  } else {
    resolved <- !rx$is_alive()
    dispatcher_status <- process_dispatcher_messages(rx, debug = debug)
  }

  state <- future[["state"]]
  
  ## Update state?
  if (state == "submitted" || state == "running") {
    if (debug) mdebugf("Future state before: %s", commaq(state))
    if (debug) mdebugf("Child process updates: [n=%d] %s", length(dispatcher_status), commaq(dispatcher_status))
    if ("wait" %in% dispatcher_status) state <- "running"
    future[["state"]] <- state
    if (debug) mdebugf("Future state after: %s", commaq(state))
  }

  ## Remove communication channels?
  if (resolved) {
    future[["state"]] <- "finished"
    channels <- attr(rx, "channels", exact = TRUE)
    if (length(channels) > 0) {
      if (debug) mdebugf("Removing communication channel files: [n=%d] %s", length(channels), commaq(channels))
      stop_if_not(is.character(channels))
      file.remove(channels)
      attr(rx, "channels") <- NULL
      future[["rx"]] <- rx
    }
  }
  
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

  ## Get the results
  response <- rx$get_result()

  ## Relay output, handle messages
  dispatcher_status <- process_dispatcher_messages(rx, debug = debug)

  ## Finalize the 'callr' process, which includes removing any temporary
  ## files that it created
  rx$finalize()

  ## Remove communication channels
  channels <- attr(rx, "channels", exact = TRUE)
  if (length(channels) > 0) {
    if (debug) mdebugf("Removing communication channel files: [n=%d] %s", length(channels), commaq(channels))
    stop_if_not(is.character(channels))
    file.remove(channels)
  }

  future[["rx"]] <- NULL

  stop_if_not(is.list(response))
  type <- response[["type"]]
  if (type == "event") {
    event <- response[["value"]]
    if (event == "interrupted") {
      result <- FutureResult(
        conditions = list(list(
          condition = structure(list(), class = c("interrupt", "condition")),
          signaled = 0L
        )),
        uuid = future[["uuid"]]
      )
    } else {
      stop(FutureError(sprintf("Unknown event from future dispatcher: event = %s", sQuote(event))))
    }
  } else if (type == "file") {
    file <- response[["value"]]
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
      stop_if_not(is.character(file))
      file.remove(file)
      result
    })
  } else {
    stop(FutureError(sprintf("Unknown response from future dispatcher: type = %s", sQuote(type))))
  }
    
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

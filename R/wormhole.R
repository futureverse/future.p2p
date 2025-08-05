#' Send and Receive Files via Wormhole
#'
#' @param file (character string) Path to a readable file.
#'
#' @param code (character string) Secret wormhole code.
#'
#' @param ... Not used.
#'
#' @param rsh (character vector; optional) Remote shell command with
#' options for launching the `wormhole` executable on another host.
#'
#' @seealso
#' This function relies on the <https://pico.sh> services.
#'
#' @importFrom utils file_test
#' @export
wormhole_send <- function(file, code = via_channel(), rsh = NULL, ...) {
  debug <- isTRUE(getOption("future.p2p.debug"))
  if (debug) {
    mdebug_push("wormhole_send() ...")
    mdebugf("Sending file: %s", file)
    mdebugf("Secret code: %s", code)
    on.exit({
      mdebug_pop()
    })
  }
  
  stopifnot(length(file) == 1L, is.character(file), !is.na(file), nzchar(file), file_test("-f", file))
  res <- wormhole_call("send", sprintf("--code=%s", code), file, ..., rsh = rsh)
  list(res = res, file = file, code = code)
}


#' @param path (character string) Temporary working directory.
#'
#' @rdname wormhole_send
#' @importFrom utils file_test
#' @export
wormhole_receive <- function(code, path = tempdir(), ..., rsh = NULL) {
  stopifnot(file_test("-d", path))
  path <- tempfile(pattern = "dir", tmpdir = path)
  dir.create(path)
  stopifnot(file_test("-d", path))

  debug <- isTRUE(getOption("future.p2p.debug"))
  if (debug) {
    mdebug_push("wormhole_receive() ...")
    mdebugf("Secret code: %s", code)
    mdebugf("Recieve to folder: %s", path)
    on.exit({
      mdebugf("Received files: [n=%d] %s", length(files), commaq(files))
      mdebug_pop()
    })
  }
  
  ## Switch to download directory
  opwd <- setwd(path)
  on.exit(setwd(opwd), add = TRUE, after = FALSE)
  
  out <- wormhole_call("receive", code, input = "y", ..., rsh = rsh)
  files <- dir(path = path, all.files = TRUE, full.names = TRUE, no.. = TRUE)
  files
}

#' @export
find_wormhole <- local({
  bin <- NULL
  function() {
    if (is.null(bin)) {
      debug <- isTRUE(getOption("future.p2p.debug"))
      if (debug) {
        mdebug_push("find_wormhole() ...")
        on.exit({
          mdebugf("File: %s", sQuote(bin))
          mdebug_pop()
        })
      }
      res <- Sys.which("wormhole")
      if (debug) mdebugf("Wormhole executable: %s", sQuote(res))
      if (nzchar(res)) {
        bfr <- system2(res, args = "--version", stdout = TRUE)

        ## Prune 'version' word and 'v' prefix
	bfr <- sub("[[:blank:]]+version[[:blank:]]+", " ", bfr)
	bfr <- sub("[[:blank:]]+v([[:digit:]])", " \\1", bfr)
	
        pattern <- "^([^[:blank:]]+)[[:blank:]]([[:digit:].]+)$"
        name <- sub(pattern, "\\1", bfr)
        attr(res, "name") <- name
        version <- sub(pattern, "\\2", bfr)
        attr(res, "verion") <- version
        bin <<- res
      }	
    }
    
    if (is.null(bin)) {
      stop("Failed to locate executable 'wormhole'")
    }
    
    bin
  }
})


#' @importFrom utils file_test
#' @export
wormhole_call <- function(command = c("send", "receive"), ..., input = NULL, rsh = NULL, timeout = 0) {
  command <- match.arg(command)
  debug <- isTRUE(getOption("future.p2p.debug"))
  if (debug) {
    mdebugf_push("wormhole_call(%s) ...", sQuote(command))
    mdebugf("Input: [n=%d] %s", length(input), if (is.null(input)) "<none>" else commaq(input))
    on.exit({
      mdebug("--- begin output ---")
      mdebug(paste(out, collapse = "\n"))
      mdebug("--- end output ---")
      mdebug_pop()
    })
  }
  
  bin <- find_wormhole()
  args <- c(command, ...)
  if (debug) {
    mdebugf("Command-line arguments: [n=%d] %s", length(args), paste(shQuote(args), collapse = " "))
  }
  
  if (!is.null(rsh)) {
    cmd <- Sys.which(rsh[1])
    if (!nzchar(cmd)) {
      stop(sprintf("Argument 'rsh' specifies a non-existing executable: %s", sQuote(rsh)))
    }
    args <- c(rsh[-1], bin, args)
    bin <- rsh[1]
  }
  
  out <- system2(bin, args = args, stdout = TRUE, stderr = TRUE, input = input, timeout = timeout)
  status <- attr(out, "status")
  if (!is.null(status)) {
    msg <- sprintf("wormhole_call(): System call returned with exit code %s", status)
    errmsg <- attr(out, "errmsg")
    if (!is.null(errmsg)) {
      msg <- sprintf("%s with error message %s", msg, sQuote(errmsg))
    }
    stop(msg)
  }
  invisible(out)
}

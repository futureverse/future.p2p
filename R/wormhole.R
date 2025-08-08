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
wormhole_send <- function(file, code, rsh = NULL, ...) {
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

  files <- NULL
  out <- wormhole_call("receive", code, input = "y", ..., rsh = rsh)
  files <- dir(path = path, all.files = TRUE, full.names = TRUE, no.. = TRUE)
  files
}

#' Find the Wormhole Executable
#'
#' @return
#' The absolute path to the `wormhole` executable as a character string.
#' Attribute `version-string` comprise the `wormhole --version` output,
#' and attributes `name` and `version` the parsed version string.
#' If no executable exists, an error is produced.
#'
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

      ## User specified binary?
      res <- getOption("future.p2p.wormhole")
      if (!is.null(res)) {
        if (!file_test("-f", res)) {
          stop("R option 'future.p2p.wormhole' specifies a non-existing file: ", sQuote(res))
	} else if (!file_test("-x", res)) {
        stop("R option 'future.p2p.wormhole' specifies a file that is non-executable: ", sQuote(res))
        }
      }	else {
        ## If not, install automatically, if missing
        res <- wormhole_pathname()

        ## Install wormhole?
        if (!file_test("-x", res)) res <- install_wormhole()
      }
      
      ## Legacy: fall back to pre-existing 'wormhole' executable
      if (!file_test("-x", res)) res <- Sys.which("wormhole")
      if (debug) mdebugf("Wormhole executable: %s", sQuote(res))
      
      if (nzchar(res)) {
        bfr <- system2(res, args = "--version", stdout = TRUE)
        attr(res, "version-string") <- bfr
	
        ## Prune 'version' word and 'v' prefix
	bfr <- sub("[[:blank:]]+version[[:blank:]]+", " ", bfr)
	bfr <- sub("[[:blank:]]+v([[:digit:]])", " \\1", bfr)
	
        pattern <- "^([^[:blank:]]+)[[:blank:]]([[:digit:].]+)$"
        name <- sub(pattern, "\\1", bfr)
        attr(res, "name") <- name
        version <- sub(pattern, "\\2", bfr)
        attr(res, "version") <- version
        bin <<- res
      }
    }
    
    if (is.null(bin)) {
      stop("Failed to locate 'wormhole' executable")
    }
    
    bin
  }
})


#' @importFrom utils file_test
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


#' @export
wormhole_filename <- function(sysname = Sys.info()[["sysname"]], arch = R.version[["arch"]]) {
  sysname <- tolower(sysname)

  if (sysname %in% c("linux", "darwin")) {
    if (arch == "x86_64") arch <- "amd64"
    ext <- ""
  } else if (sysname == "windows") {
    arch <- "386"
    ext <- ".exe"
  } else {
    stop(sprintf("Unknown system: %s", sysname))
  }

  sprintf("wormhole-william-%s-%s%s", sysname, arch, ext)
} ## wormhole_filename()


wormhole_pathname <- function(filename = wormhole_filename(), path = tools::R_user_dir("future.p2p", "data")) {
  file.path(path, filename)
} ## wormhole_pathname()

#' @importFrom utils download.file file_test
#' @export
install_wormhole <- function(pathname = wormhole_pathname(), version = "1.0.8") {
  ## Nothing to do?
  if (file_test("-x", pathname)) return(pathname)
  
  path <- dirname(pathname)
  if (!file_test("-d", path)) {
    dir.create(path, recursive = TRUE)
    stopifnot(file_test("-d", path))
  }
  filename <- basename(pathname)
  
  url <- sprintf("https://github.com/psanford/wormhole-william/releases/download/v%s/%s", version, filename)
  tf <- sprintf("%s.%s", pathname, basename(tempdir()))
  res <- download.file(url, destfile = tf, mode = "wb")
  stopifnot(file_test("-f", tf))
  Sys.chmod(tf, mode = "0755")
  stopifnot(file_test("-x", tf))
  file.rename(tf, to = pathname)
  stopifnot(file_test("-x", pathname))
  
  pathname
}

#' Send and Receive Files via Wormhole
#'
#' @param code (character string) Secret wormhole code.
#'
#' @param ... Not used.
#'
#' @seealso
#' This function relies on the <https://pico.sh> services.
#'
#' @importFrom utils file_test
#' @export
wormhole_send <- function(file, code = via_channel(), ...) {
  stopifnot(length(file) == 1L, is.character(file), !is.na(file), nzchar(file), file_test("-f", file))
  res <- wormhole_call("send", sprintf("--code=%s", code), file)
  list(res = res, file = file, code = code)
}

#' @export
wormhole_receive <- function(code, ...) {
  res <- wormhole_call("receive", code, input = "y")
  list(res = res, code = code)
}

#' @export
find_wormhole <- local({
  bin <- NULL
  function() {
    if (is.null(bin)) {
      res <- Sys.which("wormhole")
      if (nzchar(res)) bin <<- res
    }
    if (is.null(bin)) {
      stop("Failed to locate executable 'wormhole'")
    }
    bin
  }
})

#' @export
wormhole_call <- function(command = c("send", "receive"), ..., input = NULL) {
  command <- match.arg(command)
  message(sprintf("wormhole_call('%s') ...", command))
  print(sys.calls())
  on.exit(message(sprintf("wormhole_call('%s') ... done", command)))
  
  bin <- find_wormhole()
  args <- c(command, ...)
  str(list(args = args))
  res <- system2(bin, args = args, input = input)
  str(res)
  res
}

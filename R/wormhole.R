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
wormhole_send <- function(file, code, ...) {
  stopifnot(length(file) == 1L, is.character(file), !is.na(file), nzchar(file), file_test("-f", file))
  wormhole_call("send", sprintf("--code=%s", code), file)
}

#' @export
wormhole_receive <- function(code, ...) {
  wormhole_call("receive", code, input = "y")
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
  bin <- find_wormhole()
  args <- c(command, ...)
  print(args)
  system2(bin, args = args, input = input)
}

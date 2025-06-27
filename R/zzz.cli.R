parse_cmd_args <- function(cmdargs = getOption("future.p2p.tests.cmdargs", commandArgs(trailingOnly = TRUE))) {
  patterns <- list(
    "^--(channel)=(.*)$"
  )

  args <- list()
  for (pattern in patterns) {
    idx <- grep(pattern, cmdargs)
    if (length(idx) > 0) {
      cmdarg <- cmdargs[idx]
      cmdargs <- cmdargs[-idx]
      ## Use only last, iff multiple are given
      if (length(cmdarg) > 1) cmdarg <- cmdarg[length(cmdarg)]
      name <- gsub(pattern, "\\1", cmdarg)
      value <- gsub(pattern, "\\2", cmdarg)
      args[[name]] <- value
    }
  }

  if (length(cmdargs) > 0) {
    stop("Unknown future.p2p command-line arguments: ", paste(cmdargs, collapse = " "), call. = FALSE)
  }

str(args)

  args
} # parse_cmd_args()


# From R.utils::CmdArgsFunction()
cli_fcn <- function(fcn) {
  stop_if_not(is.function(fcn))
  class(fcn) <- c("cli_fcn", class(fcn))
  fcn
}

#' @export
print.cli_fcn <- function(x, ..., call = !interactive(), envir = parent.frame()) {
  if (!call) return(NextMethod())
  
  # Call function...
  res <- withVisible(do.call(x, args = parse_cmd_args(), envir = envir))

  # Should the result be printed?
  if (res$visible) {
    output <- attr(x, "output")
    if (is.null(output)) output <- print
    output(res$value)
  }

  # Return nothing
  invisible(return())
}


pico_p2p_worker <- cli_fcn(pico_p2p_worker)

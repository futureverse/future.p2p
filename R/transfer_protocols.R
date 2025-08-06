known_transfer_protocols <- function() {
  c("wormhole")
}
 
supported_transfer_protocols <- function() {
  protocols <- character(0L)
  for (protocol in known_transfer_protocols()) {
    if (protocol == "wormhole") {
      tryCatch({
        bin <- find_wormhole()
        protocols <- c(protocols, protocol)
      }, error = identity)
    }
  }
  
  if (length(protocols) == 0L) {
    stop(FutureError(sprintf("None of the known transfer protocols are supported: %s", commaq(known_transfer_protocols()))))
  }
  
  protocols
}

via_transfer_uri <- function(protocol = supported_transfer_protocols()[1]) {
  if (protocol == "wormhole") {
    digits <- sample.int(16L, size = 17L, replace = TRUE) %% 16
    digits[1:4] <- digits[1:4] %% 10
    digits <- as.hexmode(digits)
    digits <- as.character(digits)
    digits[5] <- "-"
    path <- paste(digits, collapse = "")
  } else {
    stop(FutureError(sprintf("Not one of the supported transfer protocols (%s): %s", sQuote(known_transfer_protocols()), sQuote(protocol))))
  }
  sprintf("%s://%s", protocol, path)
}

parse_transfer_uri <- function(uri) {
  stopifnot(length(uri) == 1, is.character(uri), !is.na(uri), nzchar(uri))
  pattern <- "^([^:]+)://(.*)$"
  protocol <- sub(pattern, "\\1", uri)
  path <- sub(pattern, "\\2", uri)
  if (!protocol %in% known_transfer_protocols()) {
    stop(FutureError(sprintf("Not one of the supported transfer protocols (%s): %s", sQuote(known_transfer_protocols()), sQuote(protocol))))
  }
  list(protocol = protocol, path = path)
}



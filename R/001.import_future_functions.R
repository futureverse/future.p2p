## To be imported from 'future', if available
FutureRegistry <- NULL
assertOwner <- NULL
sQuoteLabel <- NULL
.debug <- NULL
session_uuid <- NULL

## Import private functions from 'future'
#' @importFrom utils packageVersion
import_future_functions <- function() {
  FutureRegistry <<- import_future("FutureRegistry")
  assertOwner <<- import_future("assertOwner")
  sQuoteLabel <<- import_future("sQuoteLabel")
  session_uuid <<- import_future("session_uuid")

  .debug <<- import_future(".debug", mode = "environment", default = new.env(parent = emptyenv()))
}


#' @importFrom utils packageVersion
future_supports_state_submitted <- local({
  .value <- NA
  function() {
    if (is.na(.value)) .value <<- (packageVersion("future") > "1.67.0")
    .value
  }
})

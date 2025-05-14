## To be imported from 'future', if available
FutureRegistry <- NULL
assertOwner <- NULL
sQuoteLabel <- NULL
.debug <- NULL

## Import private functions from 'future'
import_future_functions <- function() {
  FutureRegistry <<- import_future("FutureRegistry")
  assertOwner <<- import_future("assertOwner")
  sQuoteLabel <<- import_future("sQuoteLabel")
  .debug <<- import_future(".debug", mode = "environment", default = new.env(parent = emptyenv()))
}


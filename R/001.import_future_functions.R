## Private functions imported from 'future'
assertOwner <- import_future("assertOwner")
sQuoteLabel <- import_future("sQuoteLabel")
session_uuid <- import_future("session_uuid")

## An environment must be imported when the package is loaded, otherwise
## we would get a copy and not share the state with 'future'
.debug <- NULL

## Same for FutureRegistry(), whose state lives in its closure. It must
## be shared with 'future', e.g. so that listFutures() sees our futures
FutureRegistry <- NULL

import_future_functions <- function() {
  .debug <<- import_future(".debug", mode = "environment")
  FutureRegistry <<- import_future("FutureRegistry")

  ## Use future:::waitForWorker(), if available, otherwise our local copy.
  ## This is done when the package is loaded, so that the one in the
  ## installed version of 'future' is used
  waitForWorker <<- import_future("waitForWorker", default = waitForWorker)
}

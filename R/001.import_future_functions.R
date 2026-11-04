## Private functions imported from 'future'
FutureRegistry <- import_future("FutureRegistry")
assertOwner <- import_future("assertOwner")
sQuoteLabel <- import_future("sQuoteLabel")
session_uuid <- import_future("session_uuid")

## An environment must be imported when the package is loaded, otherwise
## we would get a copy and not share the state with 'future'
.debug <- NULL

import_future_functions <- function() {
  .debug <<- import_future(".debug", mode = "environment")
}

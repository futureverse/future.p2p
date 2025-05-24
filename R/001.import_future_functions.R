## To be imported from 'future', if available
FutureRegistry <- NULL
assertOwner <- NULL
sQuoteLabel <- NULL
.debug <- NULL

## Import private functions from 'future'
#' @importFrom utils packageVersion
import_future_functions <- function() {
  FutureRegistry <<- import_future("FutureRegistry")
  assertOwner <<- import_future("assertOwner")
  
  ## future (>= 1.49.0)
  if (packageVersion("future") == "1.49.0") {
    sQuoteLabel <<- import_future("sQuoteLabel", default = function(label) {
      if (inherits(label, "Future")) {
        future <- label
        label <- future[["label"]]
        if (is.null(label)) {
          uuid <- future[["uuid"]]
          idx <- uuid[length(uuid)]
          label <- sprintf("<unnamed-%s>", idx)
        } else if (is.na(label)) {
          label <- "NA"
        } else {
          label <- sQuote(label)
        }
        return(label)
      }
      
      if (is.null(label)) {
        "NULL"
      } else if (is.na(label)) {
        "NA"
      } else {
        sQuote(label)
      }
    }) ## sQuoteLabel() ## default
  } else {
    sQuoteLabel <<- import_future("sQuoteLabel")
  }

  .debug <<- import_future(".debug", mode = "environment", default = new.env(parent = emptyenv()))
}

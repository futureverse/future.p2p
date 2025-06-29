#' @importFrom future Future reset
cloneFuture <- function(future, uuid = TRUE) {
  future2 <- Future()
  for (name in names(future2)) {
    assign(name, value = future[[name]], envir = future2, inherits = TRUE)
  }
  future2 <- reset(future2)
  if (uuid) {
    future2[["uuid"]] <- future[["uuid"]]
  }
  
  future2
}


saveFuture <- function(future, file = NULL, path = tempdir(), vanilla = TRUE) {
  if (is.null(file)) {
    file <- sprintf("%s.Future.rds", future_id(future))
    file <- file.path(path, file)
  }

  if (vanilla) {
    future <- cloneFuture(future, uuid = TRUE)
  }

  saveRDS(future, file = file)
  attr(file, "future_id") <- future_id(future)

  invisible(file)
}

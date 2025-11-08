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

  save_rds(future, file = file)
  attr(file, "future_id") <- future_id(future)

  invisible(file)
}


#' @importFrom utils file_test
save_rds <- function(object, file, ...) {
  file_tmp <- sprintf("%s.tmp", file)
  if (file_test("-f", file_tmp)) {
    stop("Cannot save RDS file. It is already in the process of being saved: ", sQuote(file_tmp))
  }
  
  on.exit({
    if (file_test("-f", file_tmp)) file.remove(file_tmp)
  })
  
  saveRDS(object, file = file_tmp, ...)

  ## Overwrite, just like saveRDS() does
  if (file_test("-f", file)) file.remove(file)
  file.rename(file_tmp, file)

 if (!file_test("-f", file)) {
   stop("Failed to save object to file: ", sQuote(file))
 }
 
 invisible(file)
}
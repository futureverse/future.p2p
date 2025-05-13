#' @importFrom googledrive drive_get
#' @export
drive_todo <- local({
  .cache <- NULL
  function() {
    if (is.null(.cache)) {
      .cache <<- drive_get("~/p2p-todo")
    }
    .cache
  }
})

#' @importFrom googledrive drive_get
#' @export
drive_done <- local({
  .cache <- NULL
  function() {
    if (is.null(.cache)) {
      .cache <<- drive_get("~/p2p-done")
    }
    .cache
  }
})

#' @importFrom googledrive drive_get
#' @export
drive_checked_out <- local({
  .cache <- NULL
  function() {
    if (is.null(.cache)) {
      .cache <<- drive_get("~/p2p-checked-out")
    }
    .cache
  }
})


#' @importFrom googledrive as_id drive_upload
#' @export
push_future <- function(future) {
  stopifnot(
    inherits(future, "Future"),
    isTRUE(future[["lazy"]])
  )

  uuid <- paste(future[["uuid"]], collapse = "-")
  file <- sprintf("%s.Future.rds", uuid)
  
  tf <- tempfile(fileext = ".rds")
  on.exit(unlink(tf))
  saveRDS(future, file = tf)

  drive_upload(tf, name = file, path = as_id(drive_todo()))
}


#' @importFrom googledrive as_id drive_mv drive_upload
#' @export
pop_future <- function() {
  tf <- tempfile(fileext = ".rds")
  on.exit(unlink(tf))
  files <- drive_ls(path = as_id(drive_todo()))
  ids <- files[["id"]]
  id <- ids[1]
  id2 <- drive_mv(as_id(id), path = as_id(drive_checked_out()))
  res <- drive_download(as_id(id2), path = tf, overwrite = TRUE)
  readRDS(tf)
}


#' @importFrom googledrive as_id drive_ls drive_rm drive_upload
#' @export
push_result <- function(future) {
  name <- NULL ## To please R CMD check
  
  stopifnot(
    inherits(future, "Future")
  )
  result <- future[["result"]]
  stopifnot(
    inherits(result, "FutureResult")
  )

  uuid <- paste(future[["uuid"]], collapse = "-")
  file <- sprintf("%s.FutureResult.rds", uuid)
  
  tf <- tempfile(fileext = ".rds")
  on.exit(unlink(tf))
  saveRDS(result, file = tf)

  res <- drive_upload(tf, name = file, path = as_id(drive_done()))

  ## Remove from checked-out
  uuid <- paste(future[["uuid"]], collapse = "-")
  file <- sprintf("%s.Future.rds", uuid)

  files <- drive_ls(path = as_id(drive_checked_out()))
  file <- subset(files, name == file)
  if (nrow(file) == 1) {
    res <- drive_rm(path = as_id(file))
  }

  res
}


#' @importFrom googledrive as_id drive_ls drive_rm drive_download
get_result <- function(future) {
  name <- NULL ## To please R CMD check
  
  stopifnot(
    inherits(future, "Future"),
    isTRUE(future[["lazy"]])
  )

  uuid <- paste(future[["uuid"]], collapse = "-")
  file <- sprintf("%s.FutureResult.rds", uuid)

  result <- NULL
  repeat({
    results <- drive_ls(path = as_id(drive_done()))
    result <- subset(results, name == file)
    if (nrow(result) == 1L) break
    Sys.sleep(1.0)
  })
  id <- as_id(result)
  
  tf <- tempfile(fileext = ".rds")
  on.exit(unlink(tf))
  res <- drive_download(id, path = tf, overwrite = TRUE)
  result <- readRDS(tf)
  ## Remove from Google Drive
  res <- drive_rm(id)

  future[["result"]] <- result
  future[["state"]] <- "finished"
  class(future) <- c("ConstantFuture", class(future))

  future
}

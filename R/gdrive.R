drive_parts <- function(path) {
  parts <- strsplit(path, split = "/", fixed = TRUE)[[1]]
  parts <- parts[nzchar(parts)]
  parts
}

drive_dir <- function(parts) {
  dir <- do.call(file.path, args = c(as.list(parts), fsep = "/"))
  sprintf("%s/", dir)
}

drive_dirname <- function(path) {
  parts <- drive_parts(path)
  drive_dir(parts[-length(parts)])
}

drive_basename <- function(path) {
  parts <- drive_parts(path)
  parts[length(parts)]
}


#' @importFrom googledrive local_drive_quiet drive_get drive_ls drive_mkdir
#' @export
drive_makedir <- function(path = "~/futureverse/future.p2p/") {
  debug <- isTRUE(getOption("future.p2p.debug"))
  if (debug) {
    message("drive_makedir() ...")
    message(sprintf("Path: %s", sQuote(path)))
    on.exit(message("drive_makedir() ... done"))
  }

  local_drive_quiet()
  
  parts <- drive_parts(path)
  stopifnot(length(parts) >= 2)
  stopifnot(parts[1] == "~")

  parent <- drive_dir(parts[1])
  parent_id <- drive_get(parent)
  stopifnot(nrow(parent_id) == 1L)
  
  ## Google Drive requires trailing slash for directories
  for (depth in 2:length(parts)) {
    dir <- drive_dir(parts[seq_len(depth)])
    if (depth == 2L) {
      dir_id <- drive_get(dir)
    } else {
      dirs_id <- drive_ls(path = parent_id)
      name <- drive_basename(dir)
      dir_id <- dirs_id[dirs_id$name == name, ]
    }
    if (inherits(dir_id, "error")) {
      stop("Please create Google Drive directory: ", sQuote(dir))
    } else if (nrow(dir_id) > 1L) {
      stop("Found multiple Google Drive directories with the same name. Please make sure there is only one: ", sQuote(dir))
    }

    ## Create folder?
    if (nrow(dir_id) == 0L) {
      name <- drive_basename(dir)
      dir_id <- drive_mkdir(name = name, path = parent_id)
      if (debug) message("Created new directory: ", sQuote(dir))
    } else {
      if (debug) message("Directory already exists: ", sQuote(dir))
    }
    if (debug) print(dir_id)
    stopifnot(nrow(dir_id) == 1L)
    parent_id <- dir_id
  } ## for (depth ...)
  
  parent_id
} ## drive_makedir()


#' @export
drive_p2p_dirs <- local({
  .cache <- new.env(parent = emptyenv())
  
  function(path = "~/futureverse/future.p2p/") {
    dirs <- c("todo", "running", "done")
    ids <- vector("list", length = length(dirs))
    names(ids) <- dirs
    for (dir in dirs) {
      path_dir <- drive_dir(drive_parts(paste(path, dir, sep = "/")))
      id <- .cache[[path_dir]]
      if (is.null(id)) {
        id <- drive_makedir(path_dir)
        .cache[[path_dir]] <- id
      }
      ids[[dir]] <- id
    }
    
    ids <- do.call(rbind, args = ids)
    ids
  }
})


drive_todo <- function() {
  ids <- drive_p2p_dirs()
  subset(ids, name == "todo")
}

drive_running <- function() {
  ids <- drive_p2p_dirs()
  subset(ids, name == "running")
}

drive_done <- function() {
  ids <- drive_p2p_dirs()
  subset(ids, name == "done")
}


list_todo <- function() {
  drive_ls(path = drive_todo())
}

list_running <- function() {
  drive_ls(path = drive_running())
}

list_done <- function() {
  drive_ls(path = drive_done())
}

list_all <- function() {
  ids <- drive_p2p_dirs()
  files <- vector("list", length = nrow(ids))
  for (kk in seq_along(ids)) {
    files[[kk]] <- drive_ls(path = ids[kk,])
  }
  files <- do.call(rbind, args = files)
  files
}

drive_find_file <- function(file) {
  ids <- drive_p2p_dirs()
  for (kk in seq_len(nrow(ids))) {
    id <- ids[kk, ]
    files <- drive_ls(path = id)
    file_id <- subset(files, name == file)
    if (nrow(file_id) == 1L) {
      attr(file_id, "where") <- id$name
      return(file_id)
    }
    stopifnot(nrow(file_id) == 0L)
  }
  NULL
}  


#' @importFrom googledrive drive_upload
#' @export
push_future <- function(future) {
  stopifnot(
    inherits(future, "Future"),
    isTRUE(future[["lazy"]])
  )

  uuid <- paste(future[["uuid"]], collapse = "-")
  file <- sprintf("%s.Future.rds", uuid)

  ## Assert it is not already uploaded
  file_id <- drive_find_file(file)
  if (!is.null(file_id)) {
    where <- attr(file_id, "where")
    stop(sprintf("Future is already in the %s directory", sQuote(where)))
  }
  
  tf <- tempfile(fileext = ".rds")
  on.exit(unlink(tf))
  saveRDS(future, file = tf)

  drive_upload(tf, name = file, path = drive_todo())
} ## push_future()



#' @importFrom googledrive drive_mv drive_upload
#' @export
pop_future <- function() {
  files <- drive_ls(path = drive_todo())
  stopifnot(nrow(files) > 0L)
  file_id <- files[1, ]
  
  tf <- tempfile(fileext = ".rds")
  on.exit(unlink(tf))
  id <- drive_mv(file_id, path = drive_running())
  res <- drive_download(id, path = tf, overwrite = TRUE)
  readRDS(tf)
} ## pop_future()


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

  res <- drive_upload(tf, name = file, path = drive_done())

  ## Remove from checked-out
  uuid <- paste(future[["uuid"]], collapse = "-")
  file <- sprintf("%s.Future.rds", uuid)

  files <- drive_ls(path = drive_running())
  file <- subset(files, name == file)
  if (nrow(file) == 1) {
    res <- drive_rm(path = as_id(file))
  }

  res
} ## push_result()


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
    results <- drive_ls(path = drive_done())
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
} ## get_result()


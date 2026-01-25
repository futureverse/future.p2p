find_rw <- function() {
  bin <- Sys.which("rw")
  if (!nzchar(bin)) {
    stop("External tool 'rw' not found")
  }
  bin
}

rw_config <- local({
  .cache <- NULL
  function() {
    if (is.null(.cache)) {
      bin <- find_rw()
      res <- system2(bin, args = c("--config"), stdout = TRUE)
      status <- attr(res, "status")
      if (!is.null(status)) {
        stop(sprintf("Failed to run 'rw config' (exit code %s)", status))
      }
      .cache <<- res
    }
    .cache
  }
})

#' @importFrom utils file_test
rw_r_libs <- local({
  .cache <- NULL
  function() {
    if (is.null(.cache)) {
      config <- rw_config()
      pattern <- "^rw_suggestions:RW_R_LIBS_USER=(.*)"
      value <- grep(pattern, config, value = TRUE)
      if (length(value) == 0L) {
        stop("'rw config' does not report on rw_suggestions:RW_R_LIBS_USER")
      } else if (length(value) > 1L) {
        stop(sprintf("'rw config' reports on more than one rw_suggestions:RW_R_LIBS_USER: %s", commaq(value)))
      }
      value <- sub(pattern, "\\1", value)
      .cache <<- value
    }
    
    path <- .cache
    path <- normalizePath(path, mustWork = FALSE)
    
    if (!file_test("-d", path)) {
      dir.create(path, recursive = TRUE)
      stopifnot(file_test("-d", path))
    }
    
    path
  }
})

rw_bootstrap <- function() {
  bin <- find_rw()
  
  tf <- tempfile(pattern = "future.p2p-bootstrap-", fileext = ".rds")
  on.exit(unlink(tf, recursive = TRUE))
  
  stage <- dirname(tf)
  
  code <- "if (!requireNamespace('future', quietly = TRUE)) install.packages('future')"
  
  epilogue_code <- c(
    "success <- requireNamespace('future', quietly = TRUE)",
    sprintf("saveRDS(success, file = '/host/stage/%s')", basename(tf))
  )

  r_libs <- rw_r_libs()
  
  args <- c(
    sprintf("--r-libs=%s", shQuote(r_libs)),
    sprintf("--stage=%s", shQuote(stage)),
    sprintf("--epilogue-expr=%s", shQuote(epilogue_code)),
    sprintf("--expr=%s", shQuote(code))
  )
  
  res <- system2(bin, args = args)

  ## Get results
  success <- readRDS(tf)
  if (!isTRUE(success)) {
    stop(FutureError("Failed to install 'future' packages in sandbox"))
  }
  
  r_libs
}


#' @importFrom future FutureError
rw_resolve_future <- function(f) {
  stopifnot(inherits(f, "Future"))
  bin <- find_rw()

  ## Install 'future' package, if missing
  r_libs <- rw_bootstrap()
  
  tf <- tempfile(pattern = "future.p2p-future-", fileext = ".rds")
  saveRDS(f, file = tf)
  on.exit(unlink(tf, recursive = TRUE))

  stage <- dirname(tf)

  prologue_code <- c(
    "result <- NULL",
    sprintf("f <- readRDS('/host/stage/%s')", basename(tf))
  )
  epilogue_code <- c(
    sprintf("saveRDS(result, '/host/stage/%s')", basename(tf))
  )
  
  args <- c(
    sprintf("--r-libs=%s", shQuote(r_libs)),
    sprintf("--stage=%s", shQuote(stage)),
    sprintf("--prologue-expr=%s", shQuote(prologue_code)),
    sprintf("--epilogue-expr=%s", shQuote(epilogue_code)),
    sprintf("--expr=%s", shQuote("result <- tryCatch({ future::result(f); f }, error = identity)"))
  )
  res <- system2(bin, args = args)

  ## Get results
  f2 <- readRDS(tf)

  ## Assert that the Future was resolved
  if (!inherits(f2, "Future")) {
    stop(FutureError(sprintf("Failed to resolve future %s in sandbox", sQuote(paste(f[["uuid"]], collapse = "-"))), future = f))
  }
  
  f2
} # rw_resolve_future()

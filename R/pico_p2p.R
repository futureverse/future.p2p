now_str <- function(when = Sys.time()) {
  format(when, format = "%FT%T")
}

pico_send_message_dataframe <- function(p, df) {
  msg <- unlist(df, use.names = TRUE)
  msg <- sprintf("%s=%s", names(msg), msg)
  msg <- paste(msg, collapse = ",")
  pico_send_message(p, msg)
  invisible(df)
}

#' @export
future_id <- function(future, ...) {
  id <- paste(future[["uuid"]], collapse = "-")
  stopifnot(length(id) == 1L, is.character(id), nzchar(id))
  id
}

#' @export
pico_next_message <- function(p, ...) {
  msg <- pico_receive_message(p)
  if (length(msg) == 0) return(NULL)
  parts <- strsplit(msg, split = ",", fixed = TRUE)
  parts <- lapply(parts, FUN = function(x) {
    x <- unlist(strsplit(x, split = "=", fixed = TRUE))
    names <- x[seq(from = 1L, to  = length(x), by = 2L)]
    value <- x[seq(from = 2L, to  = length(x), by = 2L)]
    names(value) <- names
    as.data.frame(as.list(value))
  })
  do.call(rbind, parts)  
}

#' @export
pico_hello <- function(p, from = p$user, type = c("worker", "client"), ...) {
  type <- match.arg(type)
  m <- data.frame(
    when = now_str(),
    type = type,
    from = from
  )
  pico_send_message_dataframe(p, m)
}

#' @export
pico_wait_for <- function(p, type, futures = NULL, delay = 0.1, ...) {
  repeat {
    m <- pico_next_message(p)
    if (!is.null(m) && m$type == type) {
      if (is.null(futures) || m$future %in% futures) break
    }
    Sys.sleep(0.1)
  }
  m
}


#' @export
pico_have_future <- function(p, future, duration = 60, from = p$user, ...) {
  stopifnot(inherits(future, "Future"))
  stopifnot(length(from) == 1L, is.character(from), nzchar(from))
  
  tf <- tempfile(fileext = ".rds")
  on.exit(file.remove(tf))
  saveRDS(future, file = tf)
  size <- file.size(tf)

  m <- data.frame(
    when = now_str(),
    expires = now_str(Sys.time() + duration),
    type = "request",
    from = from,
    future = future_id(future),
    size = size
  )
  
  pico_send_message_dataframe(p, m)
}

#' @export
pico_take_on_future <- function(p, to, future, duration = 60, from = p$user, ...) {
  stopifnot(length(future) == 1L, is.character(future), nzchar(future))
  stopifnot(length(to) == 1L, is.character(to), nzchar(to))
  stopifnot(length(from) == 1L, is.character(from), nzchar(from))

  m <- data.frame(
    when = now_str(),
    expires = now_str(Sys.time() + duration),
    type = "offer",
    from = from,
    to = to,
    future = future
  )

  pico_send_message_dataframe(p, m)
}

via_channel <- function() {
  digits <- sample.int(16L, size = 17L, replace = TRUE) %% 16
  digits[1:4] <- digits[1:4] %% 10
  digits <- as.hexmode(digits)
  digits <- as.character(digits)
  digits[5] <- "-"
  paste(digits, collapse = "")
}


#' @export
pico_send_future <- function(p, future, to, via = via_channel(), duration = 60, from = p$user, ...) {
  stopifnot(inherits(future, "Future"))
  stopifnot(length(to) == 1L, is.character(to), nzchar(to))
  stopifnot(length(via) == 1L, is.character(via), nzchar(via))
  stopifnot(length(from) == 1L, is.character(from), nzchar(from))

  m <- data.frame(
    when = now_str(),
    expires = now_str(Sys.time() + duration),
    type = "accept",
    from = from,
    to = to,
    future = future_id(future),
    via = via
  )

  tf <- file.path(tempdir(), sprintf("%s-Future.rds", m$future))
  saveRDS(future, file = tf)
  on.exit(file.remove(tf))
  
  m_res <- pico_send_message_dataframe(p, m)
  w_res <- wormhole_send(tf, code = sprintf("%s-f", m$via))
  m_res
}


#' @export
pico_receive_future <- function(p, via) {
  stopifnot(length(via) == 1, is.character(via), !is.na(via), nzchar(via))
  code <- sprintf("%s-f", via)
  file <- wormhole_receive(code)
  f <- readRDS(file)
  list(
    future = f,
    via = via
  )
}  


#' @importFrom future result
#' @export
pico_send_result <- function(p, future, via) {
  file <- file.path(tempdir(), sprintf("%s-FutureResult.rds", future_id(future)))
  r <- result(future)
  saveRDS(r, file = file)
  code <- sprintf("%s-r", via)
  res <- wormhole_send(file, code = code)
  invisible(res)
}


#' @export
pico_receive_result <- function(p, future, via) {
  stopifnot(inherits(future, "Future"))
  stopifnot(length(via) == 1, is.character(via), !is.na(via), nzchar(via))
  code <- sprintf("%s-r", via)
  file <- wormhole_receive(code)
  r <- readRDS(file)
  stopifnot(inherits(r, "FutureResult"))
  stopifnot(identical(r[["uuid"]], future[["uuid"]]))
  future[["result"]] <- r
  future
}

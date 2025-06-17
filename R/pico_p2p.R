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
pico_wait_for_request <- function(p, delay = 0.1, ...) {
  repeat {
    m <- pico_next_message(p)
    if (!is.null(m) && m$type == "request") break
    Sys.sleep(0.1)
  }
  m
}

#' @export
pico_wait_for_offer <- function(p, futures, delay = 0.1, ...) {
  stopifnot(length(futures) >= 1L, is.character(futures), all(nzchar(futures)))
  repeat {
    m <- pico_next_message(p)
    if (!is.null(m) && m$type == "offer") {
      if (m$future %in% futures) break
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
  
  pico_send_message_dataframe(p, m)
}

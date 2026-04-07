# Bug Report for future.p2p

This document summarizes the bugs and technical issues identified during
the codebase investigation on March 18, 2026.

## 1. Hardcoded Worker Count

In `R/PicoP2PFutureBackend-class.R`, the function
[`availablePicoP2PWorkers()`](https://future.p2p.futureverse.org/reference/availablePicoP2PWorkers.md)
is hardcoded to return `10L`, which does not reflect the actual number
of available workers.

``` r

availablePicoP2PWorkers <- function() {
  nworkers <- 10L
  nworkers <- max(1L, nworkers, na.rm = TRUE)
  as.integer(nworkers)
}
```

## 2. Unimplemented Function

The `waitForWorker()` function in `R/PicoP2PFutureBackend-class.R` (line
229) is an empty placeholder, even though a functional implementation
exists in `R/waitForWorker.R`.

## 3. Use of `NULL` Variables in Logging

In `R/worker.R` (lines 186–188), the `future` and `client` variables are
set to `NULL` before being used in an `info()` log message, which
results in “NULL” being printed instead of the actual identifiers.

``` r

future <- NULL
client <- NULL
info("Future %s has been resolved and results have been sent to client %s", sQuote(future), sQuote(client))
```

## 4. Function Shadowing Bug

In `R/worker.R` (lines 336–341), the local variable `info` shadows the
internal `info()` logging function. When `info()` is subsequently
called, R attempts to call the character vector as a function, causing a
crash.

``` r

info <- sprintf("state %s", sQuote(state))
...
info("withdrawing future: %s", msg) ## This will FAIL
```

## 5. Faulty Message Parsing

The
[`pico_receive_message_dataframe()`](https://future.p2p.futureverse.org/reference/pico_pipe.md)
function in `R/pico.R` (lines 144–147) fails if any message value
contains an equals sign (`=`). The `strsplit` call increases the number
of elements, causing a length mismatch when assigning names to values.

``` r

x <- unlist(strsplit(x, split = "=", fixed = TRUE))
names <- x[seq(from = 1L, to  = length(x), by = 2L)]
value <- x[seq(from = 2L, to  = length(x), by = 2L)]
names(value) <- names ## Error: names() length must match vector length
```

## 6. Potential Infinite Loop

In `R/pico.R`,
[`pico_hosted_channels()`](https://future.p2p.futureverse.org/reference/pico_pipe.md)
(line 158) can enter an infinite loop if a channel exists but lacks an
“Access List”. It matches `pattern_1` (presence of a channel) but fails
`pattern_2` (access list), leaving `channels` as `NULL` and triggering a
retry until timeout.

## 7. Missing Import and `sprintf` Bug

In `R/PicoP2PFuture-class.R` (line 98), `FutureError` is used but not
imported from the `future` package. Additionally, the `sprintf` call is
missing a `%s` placeholder.

``` r

stop(FutureError(sprintf("FutureResult file not found: ", sQuote(file))), future = future)
```

## 8. Fragile IPC via File Truncation

In `R/worker.R`, the `tx_worker()` function (line 88) uses
[`writeLines()`](https://rdrr.io/r/base/writeLines.html) on a file path,
which truncates the file on every call. If the background process hasn’t
read the previous message, it is lost.

## 9. Resource Leak on Error

In `result.PicoP2PFuture` (`R/PicoP2PFuture-class.R`, lines 110–120),
the temporary result file is removed only after a successful
[`readRDS()`](https://rdrr.io/r/base/readRDS.html). If it fails, the
file remains in the temporary directory.

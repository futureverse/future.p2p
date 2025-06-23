library(future.p2p)

message("[worker] connect to pico message queue")
p <- pico_pipe("chat", user = "hb")
print(p)

message("[worker] hello")
m <- pico_hello(p, type = "worker")
print(m)

repeat {

## Worker
message("[worker] wait for request")
m <- pico_wait_for(p, type = "request")
print(m)

message("[worker] offer to work")
res <- pico_take_on_future(p, to = m$from, future = m$future)
print(res)

message("[worker] wait for accept")
m <- pico_wait_for(p, type = "accept", futures = m$future)
print(m)

message(sprintf("[worker] receive future file via %s", m$via))
code <- sprintf("%s-f", m$via)
res <- wormhole_receive(code)
print(res)

message(sprintf("[worker] read future"))
file <- sprintf("%s-Future.rds", m$future)
print(file.info(file))
f <- readRDS(file)
print(f)

message(sprintf("[worker] processing future"))
r <- tryCatch(result(f), error = identity)
print(r)

message(sprintf("[worker] send future"))
file <- file.path(tempdir(), sprintf("%s-FutureResult.rds", future_id(f)))
saveRDS(r, file = file)
code <- sprintf("%s-r", m$via)
res <- wormhole_send(file, code = code)
print(res)

} ## repeat()
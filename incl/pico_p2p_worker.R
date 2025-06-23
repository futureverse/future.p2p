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

message("[client] receive future from worker")
res <- pico_receive_future(p, via = m$via)
f <- res$future
print(f)

message(sprintf("[worker] processing future"))
r <- tryCatch(result(f), error = identity)
print(r)

message(sprintf("[worker] send future result"))
res <- pico_send_result(p, future = f, via = m$via)
str(res)

} ## repeat()
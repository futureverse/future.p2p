library(future.p2p)

message("[worker] connect to pico message queue")
p <- pico_pipe("chat", user = "hb")
print(p)

repeat {

## Worker
message("[worker] wait for request")
m <- pico_wait_for_request(p)
print(m)

message("[worker] offer to work")
res <- pico_take_on_future(p, to = m$from, future = m$future)
print(res)

} ## repeat()
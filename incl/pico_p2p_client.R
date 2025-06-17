library(future.p2p)

message("[client] connect to pico message queue")
p <- pico_pipe("chat", user = "hb")
print(p)

## Client
f <- future(42, lazy = TRUE)
message("[client] request help")
m <- pico_have_future(p, future = f)
print(m)

message("[client] wait for an offer")
m2 <- pico_wait_for_offer(p, futures = m$future)
print(m2)

message("[client] send future to worker")
m3 <- pico_send_future(p, future = f, to = m2$from)
print(m3)

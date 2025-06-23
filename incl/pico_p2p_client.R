library(future.p2p)

message("[client] connect to pico message queue")
p <- pico_pipe("chat", user = "hb")
print(p)

message("[client] hello")
m <- pico_hello(p, type = "client")
print(m)

## Client
f <- future({
  data.frame(pid = Sys.getpid(), value = 42)
}, lazy = TRUE)
message("[client] request help")
m <- pico_have_future(p, future = f)
print(m)

message("[client] wait for an offer")
m2 <- pico_wait_for(p, type = "offer", futures = m$future)
print(m2)

message("[client] send future to worker")
m3 <- pico_send_future(p, future = f, to = m2$from)
print(m3)

message(sprintf("[client] receive future results via %s", m3$via))
f <- pico_receive_result(p, future = f, via = m3$via)
v <- value(f)
print(v)

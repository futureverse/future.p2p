library(future.p2p)

message("[client] send file")
file <- tempfile()
cat(file = file, as.character(Sys.time()))
res <- wormhole_send(file)
print(res)

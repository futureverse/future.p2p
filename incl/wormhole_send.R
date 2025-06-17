library(future.p2p)

message("[client] send file")
file <- tempfile()
cat(file = file, as.character(Sys.time()))
code <- "1234-abc-def"
res <- wormhole_send(file, code = code)
print(res)

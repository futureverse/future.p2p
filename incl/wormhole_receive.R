library(future.p2p)

message("[worker] receive file")
code <- readline()
res <- wormhole_receive(code)
print(res)

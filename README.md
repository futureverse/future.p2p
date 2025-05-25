# future.p2p - Use Shared Peer-to-Peer Compute Resources via Futureverse


```r
library(future)

googledrive::drive_auth()
  
## Resolve future via your P2P network of friends
plan(future.p2p::google_drive_p2p, .init = FALSE)

## Create future
a <- 42
f <- future(Sys.info())
  
## Get results
v <- value(f)
print(v)
```

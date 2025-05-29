# future.p2p - Use Shared Peer-to-Peer Compute Resources via Futureverse


## Using Google Drive P2P Future Network

```r
library(future)

## Authenticate yourself with Google Drive
googledrive::drive_auth()
  
## Resolve future via your P2P network of friends
plan(future.p2p::google_drive_p2p)

## Create future
a <- 42
f <- future(Sys.info())
  
## Get results
v <- value(f)
print(v)
```


## Contribute Your Compute Power to the Google Drive P2P Future Network

```r
googledrive::drive_auth()
future.p2p::google_drive_p2p_worker()
```

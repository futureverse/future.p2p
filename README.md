# future.p2p - Use Shared Peer-to-Peer Compute Resources via Futureverse

```r
library(future)

## Resolve future on hosts in your private P2P network
plan(future.p2p::p2p_gdrive, endpoint = "https://drive.google.com/drive/u/0/folders/11a2qLFer-etq0O")

f <- future(Sys.info())
v <- value(f)
print(v)
```


# Gets the identifier of the current P2P client or P2P worker

Gets the identifier of the current P2P client or P2P worker

## Usage

``` r
p2p_client_id(...)

p2p_worker_id(...)
```

## Value

`p2p_client_id()` and `p2p_worker_id()` return the client and worker
identifier, which both have format`"{username}@{hostname}:{pid}"`.

# A Pico P2P future is resolved through a Peer-to-Peer (P2P) workers communicating via pico.sh and Wormhole

A Pico P2P future is resolved through a Peer-to-Peer (P2P) workers
communicating via pico.sh and Wormhole

## Usage

``` r
PicoP2PFutureBackend(
  cluster = p2p_cluster_name(),
  host = "pipe.pico.sh",
  ssh_args = NULL,
  ...
)
```

## Arguments

- cluster:

  The p2p cluster to connect to.

- host:

  (character string) The hostname serving the pico service.

- ssh_args:

  (character vector) Optional SSH arguments.

- ...:

  Additional arguments passed to
  [`future::FutureBackend()`](https://future.futureverse.org/reference/FutureBackend-class.html).

## Value

A `PicoP2PFutureBackend` object

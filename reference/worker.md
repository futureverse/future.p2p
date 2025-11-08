# Launches a P2P worker and adds it to a P2P cluster

Launches a P2P worker and adds it to a P2P cluster

## Usage

``` r
worker(
  cluster = p2p_cluster_name(),
  host = "pipe.pico.sh",
  ssh_args = NULL,
  duration = 60 * 60
)
```

## Arguments

- cluster:

  The p2p cluster to contribute to.

- host:

  (character string) The hostname serving the pico service.

- ssh_args:

  (character vector) Optional SSH arguments.

- duration:

  Duration (in seconds) to offer working on futures.

## Sequential, single-core processing by default

A P2P worker runs sequentially (`plan(sequential)`) and is configured to
with a single CPU core to prevent nested parallelization.

## Examples

``` r
if (FALSE) { # interactive()
## Start a P2P cluster worker
future.p2p::worker()
}
```

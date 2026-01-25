# Launches a P2P worker and adds it to a P2P cluster

Launches a P2P worker and adds it to a P2P cluster

## Usage

``` r
worker(
  cluster = p2p_cluster_name(host = host, ssh_args = ssh_args),
  host = "pipe.pico.sh",
  ssh_args = NULL,
  duration = 60 * 60,
  sandbox = FALSE
)
```

## Arguments

- cluster:

  The P2P cluster to contribute to.

- host:

  (character string) The hostname serving the pico service.

- ssh_args:

  (character vector) Optional SSH arguments.

- duration:

  Duration (in seconds) to offer working on futures.

- sandbox:

  If TRUE, the future is resolved in R WebAssembly (webR), using the
  external `rw` tool (1).

## Value

Nothing.

## Sequential, single-core processing by default

A P2P worker runs sequentially and is configured with a single CPU core
to prevent nested parallelization.

## References

1.  rw: CLI for webR with Sandboxing Features, *under development*,
    <https://github.com/HenrikBengtsson/rw>.

## Examples

``` r
if (FALSE) { # interactive()
## Start a P2P cluster worker
future.p2p::worker()
}
```

# p2p futures

*WARNING: This function must never be called. It may only be used with
[`future::plan()`](https://future.futureverse.org/reference/plan.html)*

## Usage

``` r
cluster(
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

  Not used.

## Value

An object of class `PicoP2PFuture`.

## Details

A 'p2p' future is an asynchronous multiprocess future that will be
evaluated in a background R session.

The Pico P2P future backend relies on Pico (1) to distribute futures
among a peer-to-peer (P2P) cluster of R workers. Users with a Pico
account can join the P2P cluster by being invited to a shared folder.

Users who wish to contribute their compute power to the P2P cluster
should call
[`worker()`](https://future.p2p.futureverse.org/reference/worker.md).

Users who wish to take advantage of the compute power of the P2P cluster
should use `plan(future.p2p::cluster)`.

## References

1.  pico.sh, The ultimate ssh powered services for developers,
    <https://pico.sh/>.

## Examples

``` r
if (FALSE) { # interactive()
# Connect to personal P2P cluster, which is automatically launched
plan(future.p2p::cluster)

## Create future
a <- 42
f <- future({ 2 * a })

## Get results
v <- value(f)
print(v)
}
```

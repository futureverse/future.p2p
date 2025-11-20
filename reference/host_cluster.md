# Hosts a shared P2P cluster

Hosts a shared P2P cluster

## Usage

``` r
host_cluster(
  cluster = p2p_cluster_name(users, host = host, ssh_args = ssh_args),
  users = character(0L),
  host = "pipe.pico.sh",
  ssh_args = NULL,
  duration = 14 * 24 * 60 * 60
)
```

## Arguments

- cluster:

  (character string) The name of the p2p cluster.

- users:

  (character vector) Names of Pico users who should have access, in
  addition to the owner. The default is a personal cluster that only you
  have access to.

- host:

  (character string) The hostname serving the pico service.

- ssh_args:

  (character vector) Optional SSH arguments.

- duration:

  Duration (in seconds) to offer this cluster.

## Examples

``` r
if (FALSE) { # interactive()
# Connect to personal P2P cluster, which is automatically launched
host_cluster(users = c("bob", "carol"))
}
```

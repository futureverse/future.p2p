# Gets the default name of the P2P cluster

Gets the default name of the P2P cluster

## Usage

``` r
p2p_cluster_name(users = character(0))
```

## Arguments

- users:

  Users to have access to the cluster. This controls whether the default
  cluster names should be "personal" or "friends".

## Value

`p2p_cluster_name()` returns R option `future.p2p.cluster`, if set. If
not set, it returns `{pico_name}/personal` if `length(users) == 0`,
otherwise `{pico_name}/friends`.

# Gets the number of known P2P workers

Gets the number of known P2P workers

## Usage

``` r
availablePicoP2PWorkers()
```

## Value

`availablePicoP2PWorkers()` returns the number of registered workers on
the P2P cluster. It will always return at least one worker, which is
yourself. *WARNING: This is currently hardcoded to 10 workers,
regardless of the number.*

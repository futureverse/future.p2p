# Find a Wormhole Executable

Find a Wormhole Executable

## Usage

``` r
find_wormhole()
```

## Value

The absolute path to the `wormhole` executable as a character string.
Attribute `version-string` comprise the `wormhole --version` output, and
attributes `name` and `version` the parsed version string. If no
executable exists, an error is produced.

## Details

Unless R option `future.p2p.wormhole` specifies an executable, the
default is to download and install `wormhole-william` locally and use
that one.

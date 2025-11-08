# Options used by future.p2p

Below are the R options and environment variables that are used by the
future.p2p package.  
  
*WARNING: Note that the names and the default values of these options
may change in future versions of the package. Please use with care until
further notice.*

## Packages must not change these options

Just like for other R options, as a package developer you must *not*
change any of the below options. Only the end-user should set these. If
you find yourself having to tweak one of the options, make sure to undo
your changes immediately afterward.

## Options

- future.p2p.wormhole::

  (character) Specifies the absolute path to the `wormhole` executable.
  If not specified, a default one will be installed.

## Options for debugging

- future.p2p.debug::

  (logical) If `TRUE`, extensive debug messages are generated.

## Environment variables that set R options

All of the above R future.p2p.\* options can be set by corresponding
environment variable `R_FUTURE_P2P_*` *when the future.p2p package is
loaded*. This means that those environment variables must be set before
the future.p2p package is loaded in order to have an effect. For
example, if `R_FUTURE_P2P_DEBUG=true`, then option future.p2p.debug is
set to `TRUE` (logical).

## Examples

``` r
# See debug messages
options(future.p2p.debug = TRUE)
```

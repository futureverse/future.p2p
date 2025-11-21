# Version 0.4.0 [2025-11-20]

## New Features

 * Add support for canceling and interrupting 'future.p2p' futures.

## Bug Fixes

 * The `future.p2p` backend was holding on to temporary **callr**
   files longer than necessary. Such files were only removed when the
   future object itself was removed. This would result in a large
   number of temporary files accumulating where there were many
   futures processed. Now the backend finalizes the **callr** process
   as soon as the future results have been collected, which results in
   removing temporary files created by callr sooner. Previously, the
   finalizer was only run when the future object was removed and
   garbage collected.

 * Package gave errors on "Error in as.POSIXct.numeric(time1) :
   'origin' must be supplied" when using R (< 4.3.0).

 * Package failed to  install 'wormhole-williams' automatically on ARM7
   machines like Raspberry Pi.


# Version 0.3.0 [2025-08-26]

## Significant Changes

 * Package no longer attaches **future** when attached; the **future**
   package is now only imported.


# Version 0.2.1 [2025-08-13]

## Miscellaneous

 * Pico username is now inferred using the new `ssh pico.sh user`
   endpoint.


# Version 0.2.0 [2025-08-13]

## Significant Changes

 * Removed internal functions from the public API.


# Version 0.1.0 [2025-08-10]

This is the first public version of the **future.p2p** package.

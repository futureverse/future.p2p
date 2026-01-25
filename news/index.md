# Changelog

## Version (development version)

### Beta Features

- Add support for `worker(sandbox = TRUE)`, which make the P2P worker to
  resolve futures in R WebAssembly (**webR**). This requires that the
  command-line tool `rw` is installed on the system. See the `rw` tool
  for how it isolates the host system from the R expression being
  evaluated in R WebAssembly.

### Bug Fixes

- [`pico_username()`](https://future.p2p.futureverse.org/reference/pico_username.md)
  would fail if the underlying SSH call produce standard-error messages,
  e.g. `sign_and_send_pubkey: signing failed for RSA "/home/alice/.ssh/id_ed25519" from agent: agent refused operation`.

## Version 0.4.0

### New Features

- Add support for canceling and interrupting ‘future.p2p’ futures.

### Bug Fixes

- The `future.p2p` backend was holding on to temporary **callr** files
  longer than necessary. Such files were only removed when the future
  object itself was removed. This would result in a large number of
  temporary files accumulating when many futures were processed. Now the
  backend finalizes the **callr** process as soon as the future results
  have been collected, which results in removing temporary files created
  by callr sooner. Previously, the finalizer was only run when the
  future object was removed and garbage collected.

- Package gave errors on “Error in as.POSIXct.numeric(time1) : ‘origin’
  must be supplied” when using R (\< 4.3.0).

- Package failed to install ‘wormhole-william’ automatically on ARM7
  machines like Raspberry Pi.

## Version 0.3.0

### Significant Changes

- Package no longer attaches **future** when attached; the **future**
  package is now only imported.

## Version 0.2.1

### Miscellaneous

- Pico username is now inferred using the new `ssh pico.sh user`
  endpoint.

## Version 0.2.0

### Significant Changes

- Removed internal functions from the public API.

## Version 0.1.0

This is the first public version of the **future.p2p** package.

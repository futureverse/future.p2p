# Find the local RSA SSH private key for payload decryption

Uses the same "first registered key" strategy as encryption: looks up
the current user's own first RSA public key on `auth.pico.sh`, then
scans `~/.ssh/*.pub` files for a match and returns the corresponding
private key.

## Usage

``` r
pico_find_local_rsa_key(host = "auth.pico.sh")
```

## Arguments

- host:

  (character string) The pico.sh authentication hostname.

## Value

An RSA private key object (class `"rsa"`) from the openssl package.

## Details

Override by setting R option `future.p2p.ssh_key` to an explicit private
key file path. The located key is memoized for the duration of the R
session.

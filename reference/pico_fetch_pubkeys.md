# Fetch SSH public keys for a pico.sh user

Retrieves all SSH public keys registered by `username` on pico.sh from
`https://auth.pico.sh/pubkeys/{username}`. Results are cached for the
duration of the R session.

## Usage

``` r
pico_fetch_pubkeys(username, host = "auth.pico.sh")
```

## Arguments

- username:

  (character string) The pico.sh username.

- host:

  (character string) The pico.sh authentication hostname.

## Value

A character vector of SSH public key lines, one key per element.

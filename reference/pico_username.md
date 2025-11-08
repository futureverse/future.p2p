# Gets your pico.sh username

Gets your pico.sh username

## Usage

``` r
pico_username(host = "pico.sh", ssh_args = NULL, timeout = 10)
```

## Arguments

- host:

  (character string) The hostname serving the pico service.

- ssh_args:

  (character vector) Optional SSH arguments.

- timeout:

  (numeric scalar) Maximum number of seconds before giving up.

## Value

A character string or a timeout error.

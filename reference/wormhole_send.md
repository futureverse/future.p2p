# Send and Receive Files via Wormhole

Send and Receive Files via Wormhole

## Usage

``` r
wormhole_send(file, code, rsh = NULL, ...)

wormhole_receive(code, path = tempdir(), ..., rsh = NULL)
```

## Arguments

- file:

  (character string) Path to a readable file.

- code:

  (character string) Secret wormhole code.

- rsh:

  (character vector; optional) Remote shell command with options for
  launching the `wormhole` executable on another host.

- ...:

  Not used.

- path:

  (character string) Temporary working directory.

## See also

This function relies on the <https://pico.sh> services.

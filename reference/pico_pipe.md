# Connect to a pico pipe

Connect to a pico pipe

## Usage

``` r
pico_pipe(
  topic = NULL,
  command = c("pipe", "pub", "sub", "ls", "help"),
  args = c(),
  host = "pipe.pico.sh",
  ssh_args = NULL,
  ...
)

pico_terminate(p, ...)

pico_send_message(p, message, newline = TRUE, ...)

pico_send_message_dataframe(p, df)

pico_receive_message(p, n = 1L, ...)

pico_receive_message_dataframe(p, ..., pattern = NULL)

pico_hosted_channels(host = "pipe.pico.sh", ssh_args = NULL, timeout = 10)
```

## Arguments

- topic:

  (character string) The topic to connect to. If connecting to another
  users topic, prefix with their username and a forward slash, e.g.
  `alice/topic`.

- command:

  (character string) Type of pipe or command.

- args:

  (character vector) Optional arguments.

- host:

  (character string) The hostname serving the pico service.

- ssh_args:

  (character vector) Optional SSH arguments.

- p:

  A `pico_pipe` object.

- message:

  (character string) A message to send.

- newline:

  (logical) If TRUE, a newline (LF; `\n`) will be appended to the
  message, otherwise not.

- df:

  (data.frame) Data frame to send as a message.

- n:

  (integer) Number of messages to read.

- pattern:

  (character string; optional) A regular expression so scan for.

- timeout:

  (numeric scalar) Timeout (in seconds).

- ...:

  (character vector; optional) Named attributes that are recorded with
  the returned object.

## Value

A [processx::process](http://processx.r-lib.org/reference/process.md) of
class `pico_pipe`.

## See also

This function relies on the <https://pico.sh> services.

# Encrypt a file for a pico.sh user using their RSA public key

Uses a hybrid RSA+AES envelope scheme: a random AES session key encrypts
the file contents, and the recipient's RSA public key wraps the session
key. The encrypted session key, IV, and ciphertext are stored together
in an RDS file with a `.enc` suffix appended to `file`.

## Usage

``` r
pico_encrypt_file_for(file, recipient, host = "auth.pico.sh")
```

## Arguments

- file:

  (character string) Path to an existing file to encrypt.

- recipient:

  (character string) Peer ID (`{username}@{hostname}:{pid}`) or plain
  pico.sh username of the intended recipient.

- host:

  (character string) The pico.sh authentication hostname.

## Value

Path to the encrypted file (`file` with `.enc` appended).

## Details

Requires the openssl package and that `recipient` has at least one RSA
key registered on pico.sh. If either requirement is not met, an
informative error is produced.

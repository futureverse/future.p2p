# Decrypt a file encrypted with [`pico_encrypt_file_for()`](https://future.p2p.futureverse.org/reference/pico_encrypt_file_for.md)

Decrypts using the local RSA private SSH key. Looks for the key at
`~/.ssh/id_rsa` by default, or at the path given by R option
`future.p2p.ssh_key`.

## Usage

``` r
pico_decrypt_file(file)
```

## Arguments

- file:

  (character string) Path to the encrypted `.enc` file.

## Value

Path to the decrypted file (`.enc` suffix removed from `file`).

<!--
%\VignetteIndexEntry{future.p2p: Getting Started}
%\VignetteAuthor{Henrik Bengtsson}
%\VignetteKeyword{R}
%\VignetteKeyword{package}
%\VignetteKeyword{vignette}
%\VignetteKeyword{Rprofile}
%\VignetteKeyword{Renviron}
%\VignetteEngine{future.p2p::selfonly}
-->

The **[future.p2p]** package implements a future backend for parallel
and distributed processing via a peer-to-peer (P2P) compute cluster. A
P2P cluster can be either _personal_ or _shared_ with others. A
personal cluster is only accessible by the user who runs it.

This document shows you how the get started. I recommend starting out
with a _personal_ P2P cluster just to get a feel for what is
involved. After that, setting one up among friends, is almost as
easy. Regardless, all users need to set up a few things before they
can join a P2P cluster.

A P2P cluster consists of:

 1. a task message board, and
 
 2. a peer-to-peer file-transfer backend.

The task message board is a lightweight common "forum" used for
clients to post futures ("tasks") that should be processed and for
workers to offer their help to process the futures.

The P2P file-transfer backend is how clients and workers transfer
futures and results between each others.

Users who wish to join a P2P cluster, needs to create a [pico.sh]
account, which given them access to the task message board. In
contrast, P2P file transfers that take place between clients and
workers, are anonymous and requires no accounts. 


## ⚠️ Security ⚠️

_Important warning: Please note that there is nothing preventing a
user in your P2P cluster from sending malicious R code to your P2P
worker!_

For example, a P2P user may submit a future that erases all files on
the P2P worker or a future that attempts to read non-encrypted secret
files of yours, e.g.

```r
f <- future(system("erase-all-user-files"))
```

and

```r
f <- future(readLines("~/.ssh/id_ed25519"))
```

Because of this, it is important that you only join shared P2P
clusters that you trust, i.e. where you trust all the P2P user and the
user who hosts it such that they do not invite non-trusted or unknown
users.

There are mechanisms for launching P2P workers in _sandboxed_
environments. For instance, by running P2P workers in a sandboxed
virtual machine (VM, e.g. [quickemu]), in a sandboxed Linux container
(e.g. [Apptainer], [Docker] and [Podman]), or via dedicated sandboxing
tools (e.g. [Bubblewrap], [Firejail], [landrun], and macOS
`sandbox-exec`), you can mitigate some of the risk of malicious code
accessing the host machine where your personal data lives.



## Setup instructions

### 1. Generating an SSH public-private key pair

In order to have a [pico.sh] account, you need to connect to their
services using _SSH keys_ - they do not accept password logins, which
also would not work for **future.p2p**.

If you don't know about SSH keys, the gist is that they allow you to
SSH without having to enter your password each time. Instead, your
computer authenticates with the server using secure public-private
keys. It's a very convenient way of working with SSH. You can read
more it in the Wikibooks article ['OpenSSH/Cookbook/Public Key
Authentication'](https://en.wikibooks.org/wiki/OpenSSH%2FCookbook%2FPublic_Key_Authentication),
which also provides detailed instructions. The gist for creating a SSH
key pair is:

```sh
$ ssh-keygen
```

and then follow the instructions. Although you can leave the
passphrase empty, I recommend to set one and let the operating
system's _SSH agent_ manage authentication. This means that you will
only have to authenticate once when you log in into your computer,
instead of at each SSH connection.


### 2. Create P2P user account

With SSH key pairs configured, you can now create a [pico.sh] account
by calling:

```sh
$ ssh pico.sh
```

You will be asked to _choose a username_ . This will be your P2P
cluster username. You cannot change it later, so pick one with some
care. Your account will be created when you hit <kbd>ENTER</kbd> -
that's it. This adds your public SSH key to the pico.sh servers, which
is then used to identify you in all future interactions.

If you wish to access your pico.sh account from multiple machines, you
can add additional public SSH keys via `ssh pico.sh`.

Finally, verify that you have SSH access also to `pipe.pico.sh` (sic!);

```sh
$ ssh pipe.pico.sh
```

Make sure to _accept the SSH fingerprint_, otherwise you will not be
able to connect to the P2P cluster from R.

[pico.sh]: https://pico.sh/
[future.p2p]: https://github.com/futureverse/future.p2p
[quickemu]: https://github.com/quickemu-project/quickemu
[Apptainer]: https://apptainer.org/
[Docker]: https://www.docker.com/
[Podman]: https://podman.io/
[Bubblewrap]: https://github.com/containers/bubblewrap
[Firejail]: https://github.com/netblue30/firejail
[landrun]: https://github.com/Zouuup/landrun

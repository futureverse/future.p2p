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
easy. Regardlessly, all users need to set up a few things before they
can join a P2P cluster.

A P2P cluster consists of:

 1. a task message board, and
 
 2. a peer-to-peer file-transfer backend.

The task message board is a lightweight common "forum" used for
clients to post futures ("tasks") that should be processed and for
workers to offer their help to process the futures.

The P2P file-transfer backend is how clients and workers transfer
futures and results between each others.


## Create P2P user account

Users who wish to join a P2P cluster, needs to create a [pico.sh]
account, which given them access to the task message board. In
contrast, P2P file transfers that take place between clients and
workers, are anonymous and requires no accounts. To create a [pico.sh]
account, call:

```sh
$ ssh pico.sh
```

and _choose a username_ and click <kbd>ENTER</kbd>.  This adds your
public SSH key to the pico.sh servers, which is then used to identify
you in all future interactions.

If you wish to access your pico.sh account from multiple machines, you
can add additional public SSH keys via `ssh pico.sh`.

Finally, verify that you have SSH access also to `pipe.pico.sh` (sic!);

```sh
$ ssh pipe.pico.sh
```

Make sure to _accept the SSH fingerprint_, otherwise you will not be
able to connect to the P2P cluster from R.

[pico.sh]: https://pico.sh/
[future.p2p]: https://github.com/HenrikBengtsson/future.p2p

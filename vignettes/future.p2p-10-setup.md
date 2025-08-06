<!--
%\VignetteIndexEntry{future.p2p: One-Time Setup}
%\VignetteAuthor{Henrik Bengtsson}
%\VignetteKeyword{R}
%\VignetteKeyword{package}
%\VignetteKeyword{vignette}
%\VignetteKeyword{Rprofile}
%\VignetteKeyword{Renviron}
%\VignetteEngine{future.p2p::selfonly}
-->

The **[future.p2p]** package implements a future backend for parallel
and distributed processing via a peer-to-peer (P2P) compute cluster.
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
contrast, P2P file transfers are anonymous and requires no accounts,
but requires specific P2P file-transfer software tools `ssh-keygen`
installed.

This document walks you through how to set these up. _You only need to
do the below once_.


## TL;DR

In order to join such a P2P cluster,

1. create a [pico.sh] account,

2. `ssh pipe.pico.sh` once, and

3. install `wormhole` (e.g. [Magic-Wormhole] or [wormhole-william]).


## Create a P2P account

To create a [pico.sh] account, call:

```sh
$ ssh pico.sh
```

and _choose a username_ and click <kbd>ENTER</kbd>.  This will add
your public SSH key to the pico.sh servers, which is then used to
identify you in all future interactions.


## Verify SSH access

Next, verify SSH access to `pipe.pico.sh` (sic!);

```sh
$ ssh pipe.pico.sh
```

Make sure to _accept the SSH fingerprint_, otherwise you will not be
able to connect to the P2P cluster from R.


## Install Wormhole




[future.p2p]: https://github.com/HenrikBengtsson/future.p2p
[future]: https://future.futureverse.org
[futureverse]: https://www.futureverse.org
